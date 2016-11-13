package renesca.table

import org.specs2.mock._
import org.specs2.mutable._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import renesca._

class TableSpec extends Specification with Mockito {
  implicit def toJson[T: Encoder](x: T) = x.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)

  "Table" >> {
    "access row cells by column" >> {
      val columnToIndex = Map(("a", 0), ("b", 1))
      val row = Row(IndexedSeq(5, 6), columnToIndex)

      row("a").asNumber.get.toLong.get mustEqual 5
      row("b").asNumber.get.toLong.get mustEqual 6
    }

    "access rows by index" >> {
      val columnToIndex = Map(("a", 0), ("b", 1))
      val row1 = Row(IndexedSeq("x", "y"), columnToIndex)
      val row2 = Row(IndexedSeq("hau", "rein"), columnToIndex)
      val table = Table(List("a", "b"), Vector(row1, row2))

      table(0) mustEqual row1
      table(1) mustEqual row2
    }

    "test non-emptyness" >> {
      val table = Table(List("a", "b"), Vector.empty[Vector[ParameterValue]])

      table.nonEmpty mustEqual false
      table.isEmpty mustEqual true
    }

    "test non-emptyness" >> {
      val columnToIndex = Map(("a", 0), ("b", 1))
      val row1 = Row(IndexedSeq("x", "y"), columnToIndex)
      val row2 = Row(IndexedSeq("hau", "rein"), columnToIndex)
      val table = Table(List("a", "b"), Vector(row1, row2))

      table.nonEmpty mustEqual true
      table.isEmpty mustEqual false
    }
  }

  "TableFactory" >> {
    "create Table from raw data" >> {
      val table = Table(List("p", "q"), List(List[ParameterValue](1, 2), List[ParameterValue](1, 4)))

      table.columns mustEqual List("p", "q")
      table.rows(0) mustEqual Row(Array[ParameterValue](1, 2), Map(("p", 0), ("q", 1)))
      table.rows(1) mustEqual Row(Array[ParameterValue](1, 4), Map(("p", 0), ("q", 1)))
    }
  }
}
