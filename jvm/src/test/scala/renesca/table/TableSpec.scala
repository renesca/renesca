package renesca.table

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import renesca.json
import renesca.parameter.implicits._
import renesca.parameter.{ArrayParameterValue, ParameterValue}

@RunWith(classOf[JUnitRunner])
class TableSpec extends Specification with Mockito {
  "Table" should {
    "access row cells by column" in {
      val columnToIndex = Map(("a", 0), ("b", 1))
      val row = Row(IndexedSeq(5, 6), columnToIndex)

      row("a") mustEqual 5
      row("b") mustEqual 6
    }

    "access rows by index" in {
      val columnToIndex = Map(("a", 0), ("b", 1))
      val row1 = Row(IndexedSeq("x", "y"), columnToIndex)
      val row2 = Row(IndexedSeq("hau", "rein"), columnToIndex)
      val table = Table(List("a", "b"), Vector(row1, row2))

      table(0) mustEqual row1
      table(1) mustEqual row2
    }

    "test non-emptyness" in {
      val table = Table(List("a", "b"), Vector.empty[Vector[ParameterValue]])

      table.nonEmpty mustEqual false
      table.isEmpty mustEqual true
    }

    "test non-emptyness" in {
      val columnToIndex = Map(("a", 0), ("b", 1))
      val row1 = Row(IndexedSeq("x", "y"), columnToIndex)
      val row2 = Row(IndexedSeq("hau", "rein"), columnToIndex)
      val table = Table(List("a", "b"), Vector(row1, row2))

      table.nonEmpty mustEqual true
      table.isEmpty mustEqual false
    }
  }

  "TableFactory" should {
    "create Table from raw data" in {
      val table = Table(List("p", "q"), List(List[ParameterValue](1, 2), List[ParameterValue](1, 4)))

      table.columns mustEqual List("p", "q")
      table.rows(0) mustEqual Row(Array[ParameterValue](1, 2), Map(("p", 0), ("q", 1)))
      table.rows(1) mustEqual Row(Array[ParameterValue](1, 4), Map(("p", 0), ("q", 1)))
    }

    "create Table from json classes" in {
      val table = Table(json.Result(List("x", "y", "z"), List(
        json.Data(row = Some(ArrayParameterValue(List(1, 2, 3)))),
        json.Data(row = Some(ArrayParameterValue(List(4, 5, 6))))
      )))

      table.columns mustEqual List("x", "y", "z")
      table.rows(0) mustEqual Row(Array[ParameterValue](1, 2, 3), Map(("x", 0), ("y", 1), ("z", 2)))
      table.rows(1) mustEqual Row(Array[ParameterValue](4, 5, 6), Map(("x", 0), ("y", 1), ("z", 2)))
    }
  }
}
