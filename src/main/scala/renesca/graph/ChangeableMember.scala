package renesca.graph

import scala.collection.mutable

trait ChangeableMember {
  val id: Id

  private[graph] val localChanges: mutable.ArrayBuffer[GraphChange]

  protected def addChange(change: GraphChange): Unit = {
    if (!id.isLocal)
      localChanges += change
  }
}
