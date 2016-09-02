package renesca.graph

object Path {
  def apply(head: Relation, tail: Relation*): Either[String, Path] = {
    val relations = head :: tail.toList
    val nodes = relations.flatMap(r => Seq(r.startNode, r.endNode))
    val isPath = nodes.drop(1).dropRight(1).grouped(2).map(l => l.head == l.last).forall(_ == true)
    val origin = relations.head.origin
    val sameOrigin = origin.isInstanceOf[LocalOrigin] && relations.map(_.origin).forall(_.kind == origin.kind)
    // TODO: should be able to handle interrupted paths and mixed directions
    if(!isPath)
      return Left("Relations do not form a path")
    // TODO: type system?
    if(!sameOrigin)
      return Left("Relations have inconsistent origin")

    // TODO: match nodes could be allowed in any path?
    // in general: match nodes could be handled like non-local nodes...
    val pathNodes = nodes.drop(1).dropRight(1).distinct.filter(_.origin.kind == origin.kind)

    Right(new Path(nodes.distinct, pathNodes, relations, origin.asInstanceOf[LocalOrigin]))
  }
}

class Path private[graph](val allNodes: Seq[Node],
                          val nodes: Seq[Node],
                          val relations: Seq[Relation],
                          var origin: LocalOrigin
                           ) extends SubGraph {

  override def toString = s"""Path(${ relations.mkString(",") })"""

  def canEqual(other: Any): Boolean = other.isInstanceOf[Path]

  override def equals(other: Any): Boolean = other match {
    case that: Path => (that canEqual this) && this.nodes == that.nodes && this.relations == that.relations
    case _          => false
  }

  override def hashCode: Int = origin.hashCode

}
