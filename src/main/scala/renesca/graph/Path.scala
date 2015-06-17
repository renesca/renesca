package renesca.graph

object Path {
  private def apply(relations: Seq[Relation], origin: ItemOrigin): Either[String,Path] = {
    val nodes = relations.flatMap(r => Seq(r.startNode, r.endNode))
    val distinctNodes = nodes.distinct
    val isPath = nodes.drop(1).dropRight(1).grouped(2).map(l => l.head == l.last).forall(_ == true)
    val sameOrigin = relations.map(_.origin).exists(_ == origin)
    // TODO: should be able to handle interrupted paths and mixed directions
    if (!isPath)
      return Left("Given relations do not form a path")
    // TODO: this should be checked in the typesystem
    if (!sameOrigin)
      return Left(s"Relation has unsuitable origin (== $origin)")

    Right(new Path(distinctNodes, relations, origin))
  }

  def merge(relations: Relation*): Either[String,Path] = {
    apply(relations, Merge())
  }

  def find(relations: Relation*): Either[String,Path] = {
    apply(relations, Match())
  }
}

class Path private[graph](val nodes: Seq[Node],
                          val relations: Seq[Relation],
                          val origin: ItemOrigin
                           ) extends SubGraph {
}
