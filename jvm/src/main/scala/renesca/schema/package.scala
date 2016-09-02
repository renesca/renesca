package renesca

package object schema {

  import renesca.{graph => raw}

  type AbstractRelationFactoryStartEnd[START <: Node, END <: Node] = AbstractRelationFactory[START, _ <: AbstractRelation[START, END], END]
  type AbstractRelationFactoryNode[NODE <: Node] = AbstractRelationFactory[_ <: NODE, _ <: AbstractRelation[_, _], _ <: NODE]
  type AbstractRelationFactoryAny = AbstractRelationFactory[_ <: Node, _ <: AbstractRelation[_, _], _ <: Node]
}

