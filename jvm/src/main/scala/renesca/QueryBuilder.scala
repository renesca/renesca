package renesca

import renesca.graph._
import renesca.table.Table

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

import scala.collection.mutable
import concurrent.{Future, ExecutionContext}

class PathDependencyGraph(paths: Set[Path]) {
  private val resolved: mutable.ArrayBuffer[Path] = mutable.ArrayBuffer.empty
  private val seen: mutable.Set[Path] = mutable.Set.empty
  private val pathCreators: Map[Node, Path] = paths.flatMap(p => p.nodes.map(_ -> p).toMap).toMap

  case class CircularException(path: Path, dependency: Path) extends Exception

  private def resolvePath(path: Path): Unit = {
    if (resolved.contains(path))
      return

    seen += path
    val readNodes = path.allNodes.diff(path.nodes).filter(_.origin.isLocal)
    val dependencies = readNodes.flatMap(pathCreators.get(_))
    dependencies.foreach(dep => {
      if (!resolved.contains(dep)) {
        if (seen.contains(dep)) {
          throw CircularException(path, dep)
        }

        resolvePath(dep)
      }
    })

    resolved += path
  }

  def resolvePaths: Either[String, Seq[Seq[Path]]] = {
    try {
      paths.foreach(p => resolvePath(p))
    } catch {
      case CircularException(path, dep) =>
        return Left(s"Circular dependency between paths: $path depends on $dep")
    }

    Right(resolved.map(Seq(_)))
  }
}

class QueryBuilder {
  protected def newQueryGenerator = new QueryGenerator

  protected def newPathDependencyGraph(paths: Set[Path]) = new PathDependencyGraph(paths)

  protected def filterGraphChanges(graphChanges: Seq[GraphChange]) = {
    // First get rid of duplicate changes, keep only the last change
    val reversedDistinctChanges = graphChanges.reverse.distinct

    // handle deletions
    val deletedItems = mutable.Set.empty[Item]
    val addedItems = mutable.Set.empty[Item]
    reversedDistinctChanges.filter {
      case DeleteItem(item) =>
        if (addedItems.contains(item))
          false
        else {
          deletedItems += item
          true
        }
      case AddItem(item) =>
        if (deletedItems.contains(item))
          false
        else {
          addedItems += item
          true
        }
      case c: GraphItemChange =>
        !deletedItems.contains(c.item)
      case _ =>
        true
    }.reverse
  }

  protected def checkChanges(allChanges: Seq[GraphChange], deleteItems: Seq[Item], deletePaths: Seq[Path], addPaths: Seq[Path], addLocalPaths: Seq[Path], addRelations: Seq[Relation]): Option[String] = {
    val illegalChange = allChanges.find(!_.isValid)
    if (illegalChange.isDefined)
      return Some("Found invalid graph change: " + illegalChange.get)

    // check whether paths try to resolve the same items
    val pathNodes = addPaths.flatMap(_.nodes)
    if (pathNodes.distinct.size != pathNodes.size) {
      return Some("Paths cannot resolve the same nodes")
    }

    val pathRelations = addPaths.flatMap(_.relations)
    if (pathRelations.distinct.size != pathRelations.size) {
      return Some("Paths cannot resolve the same relations")
    }

    // check whether nodes in a new relation should also be deleted
    if (deleteItems.intersect(addRelations.flatMap(r => Seq(r.startNode, r.endNode))).nonEmpty) {
      return Some("Cannot delete start- or endnode of a new relation: " + deleteItems.mkString(","))
    }

    // each path can only be deleted as a whole, there is no defined way to match partial paths.
    val incompletePath = deletePaths.find { path =>
      val pathItems = path.nodes ++ path.relations
      val intersection = deleteItems.intersect(pathItems)
      pathItems.length != intersection.length
    }

    if (incompletePath.isDefined)
      return Some("Cannot partially match paths, will not delete parts of a path: " + incompletePath.get)

    None
  }

  def generateQueries(graphChanges: Seq[GraphChange]): Either[String, Seq[() => Seq[QueryConfig]]] = {
    val gen = newQueryGenerator

    val changes = filterGraphChanges(graphChanges)

    val deleteItems = changes.collect { case DeleteItem(i) => i }

    // gather content changes
    val contentChanges = changes.collect { case c: GraphContentChange => c }

    // gather path changes, we ignore all paths that reference deleted items
    val (deletePaths, addPaths) = changes.collect { case AddPath(p) => p }.partition(p => deleteItems.intersect(p.allNodes ++ p.relations).nonEmpty)
    val (addLocalProducePaths, addLocalRelationPaths, addNonLocalPaths) = {
      val (addLocalPaths, addNonLocalPaths) = addPaths.partition(p => p.allNodes.diff(p.nodes).exists(_.origin.isLocal))
      val (addLocalProducePaths, addLocalRelationPaths) = addLocalPaths.partition(p => p.nodes.nonEmpty)

      (addLocalProducePaths, addLocalRelationPaths, addNonLocalPaths)
    }

    // all add-relation and add-node changes that are already included in paths need to be ignored,
    // as they are handled when adding the paths itself
    val addRelations = changes.collect { case AddItem(r: Relation) => r }.filterNot(addPaths.flatMap(_.relations).toSet)
    val (addLocalRelations, addNonLocalRelations) = addRelations.partition(r => r.startNode.origin.isLocal || r.endNode.origin.isLocal)
    val addNodes = changes.collect { case AddItem(n: Node) => n }.filterNot(addPaths.flatMap(_.nodes).toSet)

    // translate origin of delete items. merge nodes and relations can be
    // matched via their merge properties, so we can delete them like match
    // items.
    deleteItems.foreach(item => item.origin = item.origin match {
      case Merge(merge, _) => Match(merge)
      case other => other
    })

    deletePaths.foreach(item => item.origin = item.origin match {
      case Merge(merge, _) => Match(merge)
      case other => other
    })

    checkChanges(changes, deleteItems, deletePaths, addPaths, addLocalProducePaths, addRelations) match {
      case Some(err) => return Left(err)
      case None =>
    }

    // gather delete changes
    val (independentDeleteItems, dependentDeleteRelations) = {
      val filteredDeleteItems = deleteItems.filterNot(deletePaths.flatMap(p => p.nodes ++ p.relations).toSet)
      val deleteNodes = filteredDeleteItems.collect { case n: Node => n }
      val filteredDeleteNodes = deleteNodes.filter(i => !i.origin.isLocal || i.origin.kind == Match.kind)
      val deleteRelations = filteredDeleteItems.collect { case r: Relation => r }.filter { relation =>
        // deleting a node deletes all connected relations, thus relation
        // deletion is already handled if the start- or endnode is deleted.
        // create relation can be ignored
        !deleteNodes.contains(relation.startNode) && !deleteNodes.contains(relation.endNode) && (!relation.origin.isLocal || relation.origin.kind == Match.kind)
      }

      // split delete changes into dependent and independent changes
      // independent changes that are directly handled in the first request:
      // 1. all nodes
      // 2. relations that can be referenced by their id (non-local relation)
      // 2. relations that can be referenced by the id of their non-local start- and endnode
      // all other delete changes are send in the last request, when everything is resolved
      val (deleteLocalRelations, deleteNonLocalRelations) = deleteRelations.partition(r => r.origin.isLocal && (r.startNode.origin.isLocal || r.endNode.origin.isLocal))

      (filteredDeleteNodes ++ deleteNonLocalRelations, deleteLocalRelations)
    }

    val filteredDeletePaths = deletePaths.filter(p => !p.origin.isLocal || p.origin.kind == Match.kind)

    // independent changes that do not dependent on the results of any other
    // change request and can be sent in one request
    val independentChanges = {
      val contentChangeQueries = gen.contentChangesToQueries(contentChanges) _
      val deleteQueries = gen.deletionToQueries(independentDeleteItems) _
      val addNodeQueries = gen.addNodesToQueries(addNodes) _
      val addNonLocalPathQueries = gen.addPathsToQueries(addNonLocalPaths) _
      val addNonLocalRelationQueries = gen.addRelationsToQueries(addNonLocalRelations) _

      () => contentChangeQueries() ++
        deleteQueries() ++
        addNodeQueries() ++
        addNonLocalPathQueries() ++
        addNonLocalRelationQueries()
    }

    // node producing paths that can be referenced by relations and might
    // reference nodes created in the first request the dependency graph
    // resolves dependencies between the paths, and sends a request for each
    // independent chunk
    val localProducePathChunks = newPathDependencyGraph(addLocalProducePaths.toSet).resolvePaths match {
      case Left(err) => return Left(err)
      case Right(chunks) => chunks.map(c => gen.addPathsToQueries(c) _)
    }

    // after all possible node producing queries are resolved, the relations
    // can be handled in the last request
    val relationChanges = {
      val addLocalRelationQueries = gen.addRelationsToQueries(addLocalRelations) _
      val addLocalRelationPathQueries = gen.addPathsToQueries(addLocalRelationPaths) _
      val deleteQueries = gen.deletionToQueries(dependentDeleteRelations) _
      val deletePathQueries = gen.deletionPathsToQueries(filteredDeletePaths) _

      () => addLocalRelationQueries() ++
        addLocalRelationPathQueries() ++
        deleteQueries() ++
        deletePathQueries()
    }

    Right(
      Seq(independentChanges) ++
        localProducePathChunks ++
        Seq(relationChanges)
    )
  }

  def applyQueries(queryRequests: Seq[() => Seq[QueryConfig]], queryHandler: (Seq[Query]) => Future[Seq[(Graph, Table)]]): Future[Unit] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit class SeqExtension[A](s: Seq[A]) {
      def foldLeftToFuture[B](initial: B)(f: (B, A) => Future[B])(implicit ec: ExecutionContext): Future[B] =
        s.foldLeft(Future(initial))((future, item) => future.flatMap(f(_, item)))

      def mapInSeries[B](f: A => Future[B])(implicit ec: ExecutionContext): Future[Seq[B]] =
        s.foldLeftToFuture(Seq[B]())((seq, item) => f(item).map(seq :+ _))
    }

    val future = queryRequests.mapInSeries((f) => {
      val configs = f()
      if (configs.isEmpty) Future.successful(Seq.empty) else {
        val (queries, callbacks) = configs.map(c => (c.query, c.callback)).unzip
        for (q <- queryHandler(queries)) yield (q.zip(callbacks).map { case ((g, t), f) => f(g, t) })
      }
    })
    future.map((it) => None)
  }
}
