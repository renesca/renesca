package renesca.graph

import collection.mutable

class Relations(private[graph] val self: mutable.LinkedHashSet[Relation] = mutable.LinkedHashSet.empty[Relation])
  extends mutable.Set[Relation] with mutable.SetLike[Relation, Relations] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(relation: Relation) = {
    if (relation.id.isLocal)
      localChanges += RelationAdd(relation)
    self += relation
    this
  }

  override def -=(relation: Relation) = {
    localChanges += RelationDelete(relation.id)
    self -= relation
    this
  }

  override def iterator = self.iterator
  override def contains(relation: Relation) = self contains relation
  override def empty = new Relations(self.empty)
}
