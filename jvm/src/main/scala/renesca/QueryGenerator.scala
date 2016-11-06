package renesca

import renesca.graph._
import renesca.table._

import scala.collection.mutable

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

case class QueryConfig(item: SubGraph, query: Query, callback: (Graph, Table) => Either[String, () => Any] = (_: Graph, _: Table) => Right(() => ()))

class QueryGenerator {
  val resolvedItems: mutable.Map[Item,Origin] = mutable.Map.empty

  def contentChangesToQueries(contentChanges: Seq[GraphContentChange])() = {
    contentChanges.groupBy(_.item).map {
      case (item, changes) =>
        if(item.origin.isLocal)
          throw new Exception("Trying to edit local item: " + item)

        val propertyAdditions: mutable.Map[PropertyKey, ParameterValue] = mutable.Map.empty
        val propertyRemovals: mutable.Set[PropertyKey] = mutable.Set.empty
        val labelAdditions: mutable.Set[Label] = mutable.Set.empty
        val labelRemovals: mutable.Set[Label] = mutable.Set.empty
        changes.foreach {
          case SetProperty(_, key, value) =>
            propertyRemovals -= key
            propertyAdditions += key -> value
          case RemoveProperty(_, key)     =>
            propertyRemovals += key
            propertyAdditions -= key
          case SetLabel(_, label)         =>
            labelRemovals -= label
            labelAdditions += label
          case RemoveLabel(_, label)      =>
            labelRemovals += label
            labelAdditions -= label
        }

        val isRelation = item match {
          case _: Node     => false
          case _: Relation => true
        }

        val qPatterns = new QueryPatterns(resolvedItems)
        val variable = qPatterns.randomVariable
        val matcher = if(isRelation) s"match ()-[$variable]->()" else s"match ($variable)"
        val propertyRemove = propertyRemovals.map(r => s"remove $variable.`$r`").mkString(" ")
        val labelAdd = labelAdditions.map(a => s"set $variable:`$a`").mkString(" ")
        val labelRemove = labelRemovals.map(r => s"remove $variable:`$r`").mkString(" ")
        val setters = s"set $variable += {${ variable }_propertyAdditions} $propertyRemove $labelAdd $labelRemove"
        val setterMap = ParameterMap(s"${ variable }_propertyAdditions" -> propertyAdditions.toMap.asJson)

        val query = Query(
          s"$matcher where id($variable) = {${ variable }_itemId} $setters",
          ParameterMap(s"${ variable }_itemId" -> item.origin.asInstanceOf[Id].id.asJson) ++ setterMap
        )

        QueryConfig(item, query)
    }.toSeq
  }

  def deletionToQueries(deleteItems: Seq[Item])() = {
    deleteItems.map { item =>
      val qPatterns = new QueryPatterns(resolvedItems)
      item match {
        case n: Node =>
          val NodePattern(keyword, query, postfix, parameters, variable) = qPatterns.nodePattern(n, forDeletion = true)
          val optionalVariable = qPatterns.randomVariable
          QueryConfig(n, Query(s"$keyword $query $postfix optional match ($variable)-[$optionalVariable]-() delete $optionalVariable, $variable", parameters))
        case r: Relation =>
          //TODO: invalidate Id origin of deleted item?
          if (!r.origin.isLocal) { // if not local we can match by id and have a simpler query
            val RelationPattern(keyword, query, postfix, parameters, variable) = qPatterns.relationPattern(r,  forDeletion =true)
            QueryConfig(r, Query(s"$keyword ()-$query-() $postfix delete $variable", parameters))
          } else {
            val QueryRelationPattern(queryPattern, variable, params) = qPatterns.queryRelationPattern(r, forDeletion = true)
            QueryConfig(r, Query(s"$queryPattern delete $variable", params))
          }
      }
    }
  }

  def deletionPathsToQueries(deletePaths: Seq[Path])() = {
    deletePaths.map { path =>
      val qPatterns = new QueryPatterns(resolvedItems)
      val QueryPathPattern(queryPattern, parameters, variableMap) = qPatterns.queryPathPattern(path, forDeletion = true)

      val (optionalMatchers, matcherVariables) = path.nodes.map { n =>
        val optionalVariable = qPatterns.randomVariable
        val variable = variableMap(n)
        (s"optional match ($variable)-[$optionalVariable]-()", optionalVariable)
      }.unzip

      val variables = (path.relations ++ path.nodes).map(variableMap)
      val returnClause = "delete " + (matcherVariables ++ variables).mkString(",")

      QueryConfig(path, Query(s"$queryPattern ${optionalMatchers.mkString(" ")} $returnClause", parameters))
    }
  }

  def addPathsToQueries(addPaths: Seq[Path])() = {
    addPaths.map(path => {
      val qPatterns = new QueryPatterns(resolvedItems)
      val QueryPath(query, reverseVariableMap) = qPatterns.queryPath(path)

      QueryConfig(path, query, (graph: Graph, table: Table) => {
        if(table.rows.size > 1)
          Left("More than one query result for path: " + path)
        else
          table.rows.headOption.map(row => {
            table.columns.foreach(col => {
              val item = reverseVariableMap(col)
              val id = row(col).asObject.get("id").get.asNumber.get.toLong.get //TODO: better json traversal
              resolvedItems += item -> Id(id)
            })
            Right(() => {
              table.columns.foreach(col => {
                val item = reverseVariableMap(col)
                val map = row(col)
                val id = map.asObject.get("id").get.asNumber.get.toLong.get //TODO: better json traversal
                item.origin = Id(id)
                item.properties.clear()
                val properties = map.asObject.get("properties").get.asObject.get.toMap.map { case (k, v) => PropertyKey(k) -> v } //TODO: better json traversal and transformation
                item.properties ++= properties
                item match {
                  case n: Node =>
                    n.labels.clear()
                    val labels = map.asObject.get("labels").get.asArray.get
                    n.labels ++= labels.map(l => Label(l.asString.get))
                  case _       =>
                }
              })
            })
          }).getOrElse(Left("Query result is missing desired path: " + path))
      })
    })
  }

  def addRelationsToQueries(addRelations: Seq[Relation])() = {
    addRelations.map(relation => {
      val qPatterns = new QueryPatterns(resolvedItems)
      val query = qPatterns.queryRelation(relation)

      QueryConfig(relation, query, (graph: Graph, table: Table) => {
        if(graph.relations.size > 1)
          Left("More than one query result for relation: " + relation)
        else
          graph.relations.headOption.map(dbRelation => {
            resolvedItems += relation -> dbRelation.origin
            Right(() => {
              relation.properties.clear()
              relation.properties ++= dbRelation.properties
              relation.origin = dbRelation.origin
            })
          }).getOrElse(Left("Query result is missing desired relation: " + relation))
      })
    })
  }

  def addNodesToQueries(addNodes: Seq[Node])() = {
    addNodes.map(node => {
      val qPatterns = new QueryPatterns(resolvedItems)
      val query = qPatterns.queryNode(node)

      QueryConfig(node, query, (graph: Graph, table: Table) => {
        if (graph.nodes.size > 1)
          Left("More than one query result for node: " + node)
        else
          graph.nodes.headOption.map(dbNode => {
            resolvedItems += node -> dbNode.origin
            Right(() => {
              node.properties.clear()
              node.labels.clear()
              node.properties ++= dbNode.properties
              node.labels ++= dbNode.labels
              node.origin = dbNode.origin
            })
          }).getOrElse(Left("Query result is missing desired node: " + node))
      })
    })
  }
}
