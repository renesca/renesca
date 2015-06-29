package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PathSpec extends Specification with Mockito {

  "Path" should {
    "fail on inconsistent path origin" in {
      val a = Node.matches
      val b = Node.create
      val c = Node.merge
      val r1 = Relation.create(a, "kicks", b)
      val r2 = Relation.merge(b, "kicks", c)
      val p = Path(r1, r2)

      p mustEqual Left("Relations have inconsistent origin")
    }

    "fail on disconnected path" in {
      val a = Node.matches
      val b = Node.create
      val c = Node.merge
      val r1 = Relation.create(a, "kicks", b)
      val r2 = Relation.merge(a, "kicks", c)
      val p = Path(r1, r2)

      p mustEqual Left("Relations do not form a path")
    }

    "path equals same path" in {
      val a = Node.matches
      val b = Node.create
      val c = Node.merge
      val r1 = Relation.create(a, "kicks", b)
      val r2 = Relation.create(b, "kicks", c)
      val Right(p) = Path(r1, r2)
      val Right(q) = Path(r1, r2)

      p.equals(q) mustEqual true
    }

    "path is not equal to something else" in {
      val a = Node.matches
      val b = Node.create
      val c = Node.merge
      val r1 = Relation.create(a, "kicks", b)
      val r2 = Relation.create(b, "kicks", c)
      val r3 = Relation.create(b, "kicks", a)
      val Right(p) = Path(r1, r2)
      val Right(q) = Path(r1, r3)

      p.equals(q) mustEqual false
      p.equals("hi") mustEqual false
    }
  }
}

