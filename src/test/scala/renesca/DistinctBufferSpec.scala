package renesca

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.graph.{Label, RelationType}
import renesca.parameter.PropertyKey

@RunWith(classOf[JUnitRunner])
class DistinctBufferSpec extends Specification {
  "DistinctBuffer" should {
    "provide factory" in {
      val b = DistinctBuffer(1, 2, 3)
      b must beAnInstanceOf[DistinctBuffer[Int]]
      b must contain(exactly(1, 2, 3).inOrder)
    }

    "provide empty factory" in {
      val b = DistinctBuffer.empty
      b must beAnInstanceOf[DistinctBuffer[Int]]
      b must beEmpty
    }

    "discard double elements in factory" in {
      val b = DistinctBuffer(1, 2, 3, 2)
      b must contain(exactly(1, 2, 3).inOrder)
    }

    "override methods" in {

      "apply" in {
        val b = DistinctBuffer(1, 2, 3)
        b.apply(-1) must throwAn[IndexOutOfBoundsException]
        b.apply(3) must throwAn[IndexOutOfBoundsException]
        b.apply(1) mustEqual 2
      }

      "update" in {
        val b = DistinctBuffer(1, 2, 3)
        b.update(-1, 4) must throwAn[IndexOutOfBoundsException]
        b.update(3, 4) must throwAn[IndexOutOfBoundsException]
        b.update(1, 4)
        b must contain(exactly(1, 4, 3).inOrder)
      }

      "remove, -=" in {
        val b = DistinctBuffer(1, 2, 3)
        b.remove(-1) must throwAn[IndexOutOfBoundsException]
        b.remove(3) must throwAn[IndexOutOfBoundsException]
        b.remove(1)
        b must contain(exactly(1, 3).inOrder)
        (b -= 3) must beAnInstanceOf[DistinctBuffer[Int]]
        b must contain(exactly(1).inOrder)
      }

      "clear" in {
        val b = DistinctBuffer(1, 2, 3)
        b.clear()
        b must beEmpty

        // in case set was not cleared it would prevent adding 1,2,3
        b += 2
        b must contain(exactly(2).inOrder)
      }

      "+=" in {
        val b = DistinctBuffer(1, 2)
        (b += 3) must beAnInstanceOf[DistinctBuffer[Int]]
        b must contain(exactly(1, 2, 3).inOrder)

        (b += 2) must beAnInstanceOf[DistinctBuffer[Int]]
        b must contain(exactly(1, 2, 3).inOrder)
      }

      "+=:" in {
        val b = DistinctBuffer(1, 2)
        (3 +=: b) must beAnInstanceOf[DistinctBuffer[Int]]
        b must contain(exactly(3, 1, 2).inOrder)

        (2 +=: b) must beAnInstanceOf[DistinctBuffer[Int]]
        b must contain(exactly(3, 1, 2).inOrder)
      }

      "insertAll" in {
        val b = DistinctBuffer(1, 2, 3)
        b.insertAll(-1, Nil) must throwAn[IndexOutOfBoundsException]
        b.insertAll(3, Nil) must throwAn[IndexOutOfBoundsException]
        b.insertAll(1, List(4, 1, 2, 5))
        b must contain(exactly(1, 4, 5, 2, 3).inOrder)
      }

      "iterator" in {
        val b = DistinctBuffer(1, 2, 3)
        val it = b.iterator
        it.next mustEqual 1
        it.next mustEqual 2
        it.next mustEqual 3
        it.hasNext mustEqual false
      }

      "length" in {
        val b = DistinctBuffer(1, 2, 3)
        b.length mustEqual 3
      }
    }

    "inherit methods" in {
      "take" in {
        val b = DistinctBuffer(1, 2, 3)
        b.take(2) must contain(exactly(1, 2).inOrder)
        b.take(2) must beAnInstanceOf[DistinctBuffer[Int]]
      }

      "map" in {
        import DistinctBuffer.canBuildFrom

        //TODO why do we have to do import canBuildFrom?
        // if we don't import:

        // [error] DistinctBufferSpec.scala:120: ambiguous implicit values:
        // [error]  both method canBuildFrom in trait AbstractDistinctBufferFactory of type [A]=> scala.collection.generic.CanBuildFrom[renesca.AbstractDistinctBuffer[_, renesca.DistinctBuffer],A,renesca.AbstractDistinctBuffer[A,renesca.DistinctBuffer]]
        // [error]  and method canBuildFrom in object Buffer of type [A]=> scala.collection.generic.CanBuildFrom[scala.collection.mutable.Buffer.Coll,A,scala.collection.mutable.Buffer[A]]
        // [error]  match expected type scala.collection.generic.CanBuildFrom[renesca.AbstractDistinctBuffer[Int,renesca.DistinctBuffer],String,That]
        // [error]         b.map(_.toString) must beAnInstanceOf[DistinctBuffer[String]]
        // [error]              ^

        val b = DistinctBuffer(1, 2, 3)

        b.map(_.toString) must contain(exactly("1", "2", "3").inOrder)
        b.map(_.toString) must beAnInstanceOf[DistinctBuffer[String]]
      }

      "clone" in {
        val b = DistinctBuffer(1, 2, 3)
        b.clone() must beAnInstanceOf[DistinctBuffer[Int]]
      }

      "equals" in {
        DistinctBuffer(1, 2, 3) mustEqual DistinctBuffer(1, 2, 3)
        DistinctBuffer(1, 2, 3, 2) mustEqual DistinctBuffer(1, 2, 3, 3)

        DistinctBuffer(1, 2) mustNotEqual DistinctBuffer(1, 3)
        DistinctBuffer(1, 2, 1) mustNotEqual DistinctBuffer(1, 3)
      }
    }
  }

  //TODO: covarinance checks
}

