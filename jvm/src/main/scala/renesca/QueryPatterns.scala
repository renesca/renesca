package renesca

import renesca.graph._
import renesca.parameter._

import scala.collection.mutable

case class SelectLiteralMap(literalMapMatcher: String, remainingProperties: ParameterMap, parameterMap: ParameterMap)
case class NodePattern(queryType: String, queryPart: String, propertySetter: String, parameterMap: ParameterMap, returns: String)
case class RelationPattern(queryType: String, queryPart: String, propertySetter: String, parameterMap: ParameterMap, returns: String)
case class QueryRelationPattern(query: String, variable: String, parameterMap: ParameterMap)
case class QueryPathPattern(query: String, parameterMap: ParameterMap, variableMap: Map[Item, String])
case class QueryPath(query: Query, reverseVariableMap: Map[String, Item])

class QueryPatterns(resolvedItems: mutable.Map[Item,Origin]) {
  private var variableCounter = 0
  def randomVariable = {
    val variable = "V" + variableCounter
    variableCounter += 1
    variable
  }

  def selectLiteralMap(variable: String, properties: Properties, selection: Set[PropertyKey]): SelectLiteralMap = {
    val remainingProperties = properties.filterKeys(!selection.contains(_)).toMap
    val selectedProperties = properties.filterKeys(selection.contains(_))
    val parameterMap = selectedProperties.toMap.map { case (k, v) => (PropertyKey(s"${ variable }_${ k }"), v) }
    val literalMap = selectedProperties.map { case (k, _) => s"$k: {${ variable }_${ k }}" }
    val literalMapMatcher = if(literalMap.isEmpty) "" else literalMap.mkString("{", ",", "}")
    SelectLiteralMap(literalMapMatcher, remainingProperties, parameterMap)
  }


  //TODO should limit number of matches for merge and match to 1!
  // match/merge ... with variable limit 1 ...
  def nodePattern(node: Node, forDeletion: Boolean = false): NodePattern = {
    val variable = randomVariable
    val labels = node.labels.map(label => s":`$label`").mkString
    resolvedItems.getOrElse(node, node.origin) match {
      case Id(id)                =>
        NodePattern("match", s"($variable)", s"where id($variable) = {${ variable }_nodeId}", ParameterMap(s"${ variable }_nodeId" -> Id(id)), variable)
      case Create()              =>
        NodePattern("create", s"($variable $labels {${ variable }_properties})", "", ParameterMap(s"${ variable }_properties" -> node.properties.toMap), variable)
      case Merge(merge, onMatch) =>
        val SelectLiteralMap(mergeLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, node.properties, merge.toSet)
        val onMatchProperties = remainingProperties.filterKeys(onMatch contains _)
        NodePattern("merge", s"($variable $labels $mergeLiteralMap)", s"on create set $variable += {${ variable }_onCreateProperties} on match set $variable += {${ variable }_onMatchProperties}",
          parameterMap ++ ParameterMap(s"${ variable }_onCreateProperties" -> remainingProperties.toMap, s"${ variable }_onMatchProperties" -> onMatchProperties.toMap),
          variable)
      case Match(matches)        =>
        val SelectLiteralMap(matchLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, node.properties, matches)
        val (propertySetter, propertyMap) = if (forDeletion || remainingProperties.isEmpty)
          ("", ParameterMap.empty)
        else
          (s"set $variable += {${ variable }_properties}", ParameterMap(s"${ variable }_properties" -> remainingProperties.toMap))

        NodePattern("match", s"($variable $labels $matchLiteralMap)", propertySetter,
          parameterMap ++ propertyMap,
          variable)
    }
  }

  def relationPattern(relation: Relation, forDeletion: Boolean = false): RelationPattern = {
    val variable = randomVariable
    resolvedItems.getOrElse(relation, relation.origin) match {
      case Id(id)                =>
        RelationPattern("match", s"[$variable]", s"where id($variable) = {${ variable }_relationId}", ParameterMap(s"${ variable }_relationId" -> Id(id)), variable)
      case Create()              =>
        RelationPattern("create", s"[$variable :`${ relation.relationType }` {${ variable }_properties}]", "", ParameterMap(s"${ variable }_properties" -> relation.properties.toMap), variable)
      case Merge(merge, onMatch) =>
        val SelectLiteralMap(mergeLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, relation.properties, merge)
        val onMatchProperties = remainingProperties.filterKeys(onMatch)
        RelationPattern("merge", s"[$variable :`${ relation.relationType }` $mergeLiteralMap]", s"on create set $variable += {${ variable }_onCreateProperties} on match set $variable += {${ variable }_onMatchProperties}",
          parameterMap ++ ParameterMap(s"${ variable }_onCreateProperties" -> remainingProperties.toMap, s"${ variable }_onMatchProperties" -> onMatchProperties.toMap),
          variable)
      case Match(matches)        =>
        val SelectLiteralMap(matchLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, relation.properties, matches)
        val (propertySetter, propertyMap) = if (forDeletion || remainingProperties.isEmpty)
          ("", ParameterMap.empty)
        else
          (s"set $variable += {${ variable }_properties}", ParameterMap(s"${ variable }_properties" -> remainingProperties.toMap))

        RelationPattern("match", s"[$variable :`${ relation.relationType }` $matchLiteralMap]", propertySetter,
          parameterMap ++ propertyMap,
          variable)
    }
  }

  def queryNode(node: Node): Query = {
    val NodePattern(keyword, query, postfix, parameters, variable) = nodePattern(node)
    Query(s"$keyword $query $postfix return $variable", parameters)
  }

  def queryRelationPattern(relation: Relation, forDeletion: Boolean = false): QueryRelationPattern = {
    if(resolvedItems.getOrElse(relation.startNode, relation.startNode.origin).isLocal)
      throw new Exception("Start node in relation is still local: " + relation)
    if(resolvedItems.getOrElse(relation.endNode, relation.endNode.origin).isLocal)
      throw new Exception("End node in relation is still local: " + relation)

    val NodePattern(startKeyword, startQuery, startPostfix, startParameters, startVariable) = nodePattern(relation.startNode, forDeletion)
    val NodePattern(endKeyword, endQuery, endPostfix, endParameters, endVariable) = nodePattern(relation.endNode, forDeletion)
    val RelationPattern(keyword, query, postfix, parameters, variable) = relationPattern(relation, forDeletion)

    QueryRelationPattern(s"$startKeyword $startQuery $startPostfix $endKeyword $endQuery $endPostfix $keyword ($startVariable)-$query->($endVariable) $postfix", variable, startParameters ++ parameters ++ endParameters)
  }

  def queryRelation(relation: Relation): Query = {
    val QueryRelationPattern(queryPattern, variable, params) = queryRelationPattern(relation)
    Query(s"$queryPattern return $variable", params)
  }

  def queryPathPattern(path: Path, forDeletion: Boolean = false): QueryPathPattern = {
    val variableMap = mutable.LinkedHashMap.empty[Item, String]

    // first match all non-path nodes
    val boundNodes = path.relations.flatMap(r => Seq(r.startNode, r.endNode)).distinct diff path.nodes
    if(boundNodes.exists(n => resolvedItems.getOrElse(n, n.origin).isLocal))
      throw new Exception("Bound node on path is still local: " + path)

    val (nodeQueries, nodeParameters) = boundNodes.map(node => {
      val NodePattern(keyword, query, postfix, parameters, variable) = nodePattern(node, forDeletion)
      variableMap += node -> variable
      (s"$keyword $query $postfix", parameters.toMap)
    }).unzip

    val nodeQuery = nodeQueries.mkString(" ")

    // now the actual path can matched/merged/created
    val (pathQuery, pathParameters) = {
      val (pathQueries, pathSetters, pathParameters) = path.relations.zipWithIndex.map { case (relation, i) =>
        // a path assures that the start node was already matched by the previous relation (if there is one)
        val NodePattern(_, startQuery, startSetter, startParameters, startVariable) = if(i == 0)
                                                                             variableMap.get(relation.startNode).map(variable => ("", s"($variable)", "", ParameterMap.empty, variable)).getOrElse(nodePattern(relation.startNode, forDeletion))
                                                                           else
                                                                             NodePattern("", "", "", ParameterMap.empty, variableMap(relation.startNode))
        val NodePattern(_, endQuery, endSetter, endParameters, endVariable) = variableMap.get(relation.endNode).map(variable => ("", s"($variable)", "", ParameterMap.empty, variable)).getOrElse(nodePattern(relation.endNode, forDeletion))
        val RelationPattern(_, relQuery, relSetter, relParameters, relVariable) = relationPattern(relation, forDeletion)
        variableMap ++= Seq(relation.startNode -> startVariable, relation.endNode -> endVariable, relation -> relVariable)

        (s"$startQuery-$relQuery->$endQuery",
          s"$startSetter $endSetter $relSetter",
          startParameters ++ endParameters ++ relParameters)
      }.unzip3

      val parameters = pathParameters.flatten.toMap
      val pathQuery = s"""${ pathQueries.mkString(" ") } ${ pathSetters.mkString(" ") }"""
      (path.origin match {
        case Create()    => s"create $pathQuery"
        case Match(_)    => s"match $pathQuery"
        case Merge(_, _) => s"merge $pathQuery"
      }, parameters)
    }

    val parameters = (nodeParameters.flatten ++ pathParameters).toMap

    QueryPathPattern(s"$nodeQuery $pathQuery", parameters, variableMap.toMap)
  }

  def queryPath(path: Path): QueryPath  = {
    val QueryPathPattern(queryPattern, parameters, variableMap) = queryPathPattern(path)
    val reverseVariableMap = variableMap.map { case (k, v) => (v, k) }.toMap

    val returnClause = "return " + variableMap.map {
      case (k, variable) => k match {
        case _: Node     => s"{id: id($variable), properties: $variable, labels: labels($variable)} as $variable"
        case _: Relation => s"{id: id($variable), properties: $variable} as $variable"
      }
    }.mkString(",")

    QueryPath(Query(s"$queryPattern $returnClause", parameters), reverseVariableMap)
  }
}
