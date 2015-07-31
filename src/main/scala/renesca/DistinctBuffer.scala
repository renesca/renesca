package renesca

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DistinctBuffer {
  def empty = new DistinctBuffer(ArrayBuffer.empty, mutable.HashSet.empty)

  def apply[T](elems: T*) = fromSeq(elems)

  def fromSeq[T](elems: Seq[T]): DistinctBuffer[T] = {
    val buffer = ArrayBuffer.empty[T]
    val set = mutable.HashSet.empty[T]
    for(elem <- elems) {
      if(!(set contains elem)) {
        buffer += elem
        set += elem
      }
    }
    new DistinctBuffer[T](buffer, set)
  }

  def newBuilder[T] = new mutable.Builder[T, DistinctBuffer[T]] {
    val buffer = ArrayBuffer.empty[T]
    val set = mutable.HashSet.empty[T]

    override def +=(elem: T): this.type = {
      if(!(set contains elem)) {
        buffer += elem
        set += elem
      }
      this
    }
    override def result(): DistinctBuffer[T] = new DistinctBuffer(buffer, set)
    override def clear() {
      buffer.clear()
      set.clear()
    }
  }

  implicit def canBuildFrom[T]: CanBuildFrom[DistinctBuffer[_], T, DistinctBuffer[T]] =
    new CanBuildFrom[DistinctBuffer[_], T, DistinctBuffer[T]] {
      def apply(): mutable.Builder[T, DistinctBuffer[T]] = newBuilder
      def apply(from: DistinctBuffer[_]): mutable.Builder[T, DistinctBuffer[T]] = newBuilder
    }
}


final class DistinctBuffer[T] private(buffer: ArrayBuffer[T], set: mutable.HashSet[T])
  extends mutable.Buffer[T]
  with mutable.BufferLike[T, DistinctBuffer[T]] {

  override protected[this] def newBuilder: mutable.Builder[T, DistinctBuffer[T]] =
    DistinctBuffer.newBuilder

  override def apply(n: Int): T = {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    buffer(n)
  }

  override def update(n: Int, newelem: T) {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException
    if(set contains newelem) return

    buffer.update(n, newelem)
    set += newelem
  }

  override def remove(n: Int): T = {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    val removed = buffer.remove(n)
    set -= removed
    removed
  }

  override def clear() {
    buffer.clear()
    set.clear()
  }

  override def +=(elem: T): DistinctBuffer.this.type = {
    if(set contains elem) return this

    set += elem
    buffer += elem
    this
  }

  override def +=:(elem: T): DistinctBuffer.this.type = {
    if(set contains elem) return this

    set += elem
    elem +=: buffer
    this
  }

  override def insertAll(n: Int, elems: Traversable[T]) {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    val newElems = elems.toSeq diff set.toSeq
    set ++= newElems
    buffer.insertAll(n, newElems)
  }

  override def iterator: Iterator[T] = buffer.iterator

  override def length = buffer.length
}
