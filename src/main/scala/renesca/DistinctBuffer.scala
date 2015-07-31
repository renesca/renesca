package renesca

import scala.collection.generic.{CanBuildFrom, GenericCompanion}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DistinctBuffer[A] private(
                                 protected val buffer: ArrayBuffer[A],
                                 protected val set: mutable.HashSet[A])
  extends AbstractDistinctBuffer[A, DistinctBuffer] {
  override protected def factory = DistinctBuffer
}

object DistinctBuffer extends AbstractDistinctBufferFactory[DistinctBuffer] {
  override protected def constructor[A](buffer: ArrayBuffer[A], set: mutable.HashSet[A]) = new DistinctBuffer[A](buffer, set)
}


trait AbstractDistinctBufferWithFixedTypeFactory[A, CC <: AbstractDistinctBufferWithFixedType[A, CC]] {
  protected def constructor(buffer: ArrayBuffer[A], set: mutable.HashSet[A]): CC

  def newBuilder: mutable.Builder[A, CC] = new mutable.Builder[A, CC] {
    val buffer = ArrayBuffer.empty[A]
    val set = mutable.HashSet.empty[A]

    override def +=(elem: A): this.type = {
      if(!(set contains elem)) {
        buffer += elem
        set += elem
      }
      this
    }
    override def result(): CC = constructor(buffer, set)
    override def clear() {
      buffer.clear()
      set.clear()
    }
  }

  implicit def canBuildFrom: CanBuildFrom[AbstractDistinctBufferWithFixedType[_, CC], A, AbstractDistinctBufferWithFixedType[A, CC]] =
    new CanBuildFrom[AbstractDistinctBufferWithFixedType[_, CC], A, AbstractDistinctBufferWithFixedType[A, CC]] {
      def apply(): mutable.Builder[A, AbstractDistinctBufferWithFixedType[A, CC]] = newBuilder
      def apply(from: AbstractDistinctBufferWithFixedType[_, CC]): mutable.Builder[A, AbstractDistinctBufferWithFixedType[A, CC]] = newBuilder
    }

  def empty: CC = newBuilder.result()

  def apply(elems: A*): CC = {
    if(elems.isEmpty) empty
    else {
      val b = newBuilder
      b ++= elems
      b.result()
    }
  }
}

trait AbstractDistinctBufferFactory[CC[X] <: AbstractDistinctBuffer[X, CC]] extends GenericCompanion[CC] {
  protected def constructor[A](buffer: ArrayBuffer[A], set: mutable.HashSet[A]): CC[A]

  protected def myNewBuilder[A]: mutable.Builder[A, CC[A]] = new mutable.Builder[A, CC[A]] {
    val buffer = ArrayBuffer.empty[A]
    val set = mutable.HashSet.empty[A]

    override def +=(elem: A): this.type = {
      if(!(set contains elem)) {
        buffer += elem
        set += elem
      }
      this
    }
    override def result(): CC[A] = constructor(buffer, set)
    override def clear() {
      buffer.clear()
      set.clear()
    }
  }

  override def newBuilder[A] = myNewBuilder[A]

  implicit def canBuildFrom[A]: CanBuildFrom[AbstractDistinctBuffer[_, CC], A, AbstractDistinctBuffer[A, CC]] =
    new CanBuildFrom[AbstractDistinctBuffer[_, CC], A, AbstractDistinctBuffer[A, CC]] {
      def apply(): mutable.Builder[A, AbstractDistinctBuffer[A, CC]] = newBuilder
      def apply(from: AbstractDistinctBuffer[_, CC]): mutable.Builder[A, AbstractDistinctBuffer[A, CC]] = newBuilder
    }
}

trait AbstractDistinctBufferWithFixedType[A, CC <: AbstractDistinctBufferWithFixedType[A, CC]]
  extends mutable.Buffer[A]
  with mutable.BufferLike[A, AbstractDistinctBufferWithFixedType[A, CC]] {

  protected def factory: AbstractDistinctBufferWithFixedTypeFactory[A, CC]
  protected def buffer: ArrayBuffer[A]
  protected def set: mutable.HashSet[A]

  override protected[this] def newBuilder: mutable.Builder[A, AbstractDistinctBufferWithFixedType[A, CC]] =
    factory.newBuilder

  override def apply(n: Int): A = {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    buffer(n)
  }

  override def update(n: Int, newelem: A) {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException
    if(set contains newelem) return

    buffer.update(n, newelem)
    set += newelem
  }

  override def remove(n: Int): A = {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    val removed = buffer.remove(n)
    set -= removed
    removed
  }

  override def clear() {
    buffer.clear()
    set.clear()
  }

  override def +=(elem: A): AbstractDistinctBufferWithFixedType.this.type = {
    if(set contains elem) return this

    set += elem
    buffer += elem
    this
  }

  override def +=:(elem: A): AbstractDistinctBufferWithFixedType.this.type = {
    if(set contains elem) return this

    set += elem
    elem +=: buffer
    this
  }

  override def insertAll(n: Int, elems: Traversable[A]) {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    val newElems = elems.toSeq diff set.toSeq
    set ++= newElems
    buffer.insertAll(n, newElems)
  }


  //TODO: override contains by set.contains
  // override def contains[A1 >: A](elem: A1): Boolean = set.contains(elem)

  override def iterator: Iterator[A] = buffer.iterator

  override def length = buffer.length
}

trait AbstractDistinctBuffer[A, CC[X] <: AbstractDistinctBuffer[X, CC]]
  extends mutable.Buffer[A]
  with mutable.BufferLike[A, AbstractDistinctBuffer[A, CC]] {

  protected def factory: AbstractDistinctBufferFactory[CC]
  protected def buffer: ArrayBuffer[A]
  protected def set: mutable.HashSet[A]

  override protected[this] def newBuilder: mutable.Builder[A, AbstractDistinctBuffer[A, CC]] =
    factory.newBuilder[A]

  override def apply(n: Int): A = {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    buffer(n)
  }

  override def update(n: Int, newelem: A) {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException
    if(set contains newelem) return

    buffer.update(n, newelem)
    set += newelem
  }

  override def remove(n: Int): A = {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    val removed = buffer.remove(n)
    set -= removed
    removed
  }

  override def clear() {
    buffer.clear()
    set.clear()
  }

  override def +=(elem: A): AbstractDistinctBuffer.this.type = {
    if(set contains elem) return this

    set += elem
    buffer += elem
    this
  }

  override def +=:(elem: A): AbstractDistinctBuffer.this.type = {
    if(set contains elem) return this

    set += elem
    elem +=: buffer
    this
  }

  override def insertAll(n: Int, elems: Traversable[A]) {
    if(n < 0 || length <= n) throw new IndexOutOfBoundsException

    val newElems = elems.toSeq diff set.toSeq
    set ++= newElems
    buffer.insertAll(n, newElems)
  }


  //TODO: override contains by set.contains
  // override def contains[A1 >: A](elem: A1): Boolean = set.contains(elem)

  override def iterator: Iterator[A] = buffer.iterator

  override def length = buffer.length
}
