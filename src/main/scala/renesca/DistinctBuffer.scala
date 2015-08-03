package renesca

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DistinctBuffer[A] private[renesca](
                                          protected[renesca] val buffer: ArrayBuffer[A],
                                          protected[renesca] val set: mutable.HashSet[A])
  extends AbstractDistinctBuffer[A, DistinctBuffer] {
  override protected[renesca] def factory = DistinctBuffer
}

object DistinctBuffer extends AbstractDistinctBufferFactory[DistinctBuffer] {
  override protected[renesca] def constructor[A](buffer: ArrayBuffer[A], set: mutable.HashSet[A]) = new DistinctBuffer[A](buffer, set)
}


trait AbstractDistinctBufferWithFixedTypeFactory[A, CC <: AbstractDistinctBufferWithFixedType[A, CC]] {
  implicit protected[renesca] def constructor(buffer: ArrayBuffer[A], set: mutable.HashSet[A]): CC

  implicit def canBuildFrom: CanBuildFrom[AbstractDistinctBufferWithFixedType[_, CC], A, AbstractDistinctBufferWithFixedType[A, CC]] =
    new CanBuildFrom[AbstractDistinctBufferWithFixedType[_, CC], A, AbstractDistinctBufferWithFixedType[A, CC]] {
      def apply(): mutable.Builder[A, AbstractDistinctBufferWithFixedType[A, CC]] = new DistinctBufferBuilder(constructor)
      def apply(from: AbstractDistinctBufferWithFixedType[_, CC]) = new DistinctBufferBuilder(constructor)
    }

  def empty[A]: CC = AbstractDistinctBufferFactoryImpl.empty

  def apply(elems: A*): CC = AbstractDistinctBufferFactoryImpl.apply(elems:_*)
}

trait AbstractDistinctBufferFactory[CC[X] <: AbstractDistinctBuffer[X, CC]] {
  implicit protected[renesca] def constructor[A](buffer: ArrayBuffer[A], set: mutable.HashSet[A]): CC[A]

  implicit def canBuildFrom[A]: CanBuildFrom[AbstractDistinctBuffer[_, CC], A, AbstractDistinctBuffer[A, CC]] =
    new CanBuildFrom[AbstractDistinctBuffer[_, CC], A, AbstractDistinctBuffer[A, CC]] {
      def apply(): mutable.Builder[A, AbstractDistinctBuffer[A, CC]] = new DistinctBufferBuilder(constructor)
      def apply(from: AbstractDistinctBuffer[_, CC]) = new DistinctBufferBuilder(constructor)
    }

  def empty[A]: CC[A] = AbstractDistinctBufferFactoryImpl.empty

  def apply[A](elems: A*): CC[A] = AbstractDistinctBufferFactoryImpl.apply(elems:_*)
}

object AbstractDistinctBufferFactoryImpl {
  def empty[A, CC](implicit constructor: (ArrayBuffer[A], mutable.HashSet[A]) => CC): CC = (new DistinctBufferBuilder[A, CC](constructor)).result()

  def apply[A,CC](elems: A*)(implicit constructor: (ArrayBuffer[A], mutable.HashSet[A]) => CC): CC = {
    if(elems.isEmpty) empty(constructor)
    else {
      val b = new DistinctBufferBuilder[A, CC](constructor)
      b ++= elems
      b.result()
    }
  }
}

trait AbstractDistinctBufferWithFixedType[A, CC <: AbstractDistinctBufferWithFixedType[A, CC]]
  extends AbstractDistinctBufferImpl[A]
  with mutable.BufferLike[A, AbstractDistinctBufferWithFixedType[A, CC]] {

  protected[renesca] def factory: AbstractDistinctBufferWithFixedTypeFactory[A, CC]

  override protected[renesca] def newBuilder =
    new DistinctBufferBuilder(factory.constructor)
}

trait AbstractDistinctBuffer[A, CC[X] <: AbstractDistinctBuffer[X, CC]]
  extends AbstractDistinctBufferImpl[A]
  with mutable.BufferLike[A, AbstractDistinctBuffer[A, CC]] {

  protected[renesca] def factory: AbstractDistinctBufferFactory[CC]

  override protected[renesca] def newBuilder: mutable.Builder[A, AbstractDistinctBuffer[A, CC]] =
    new DistinctBufferBuilder(factory.constructor)
}

class DistinctBufferBuilder[A, CC](constructor: (ArrayBuffer[A], mutable.HashSet[A]) => CC) extends mutable.Builder[A, CC] {
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

trait AbstractDistinctBufferImpl[A] extends mutable.Buffer[A] {
  protected[renesca] def buffer: ArrayBuffer[A]
  protected[renesca] def set: mutable.HashSet[A]

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

  override def +=(elem: A): this.type = {
    if(set contains elem) return this

    set += elem
    buffer += elem
    this
  }

  override def +=:(elem: A): this.type = {
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

