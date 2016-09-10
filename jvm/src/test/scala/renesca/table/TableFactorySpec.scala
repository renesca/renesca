package renesca.table

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import renesca.json
import renesca.parameter.implicits._
import renesca.parameter.{ArrayParameterValue, ParameterValue}

@RunWith(classOf[JUnitRunner])
class TableFactorySpec extends Specification with Mockito {
  "Table" should {
    "create Table from json classes" in {
      val table = json.TableFactory(json.Result(List("x", "y", "z"), List(
        json.Data(row = Some(ArrayParameterValue(List(1, 2, 3)))),
        json.Data(row = Some(ArrayParameterValue(List(4, 5, 6))))
      )))

      table.columns mustEqual List("x", "y", "z")
      table.rows(0) mustEqual Row(Array[ParameterValue](1, 2, 3), Map(("x", 0), ("y", 1), ("z", 2)))
      table.rows(1) mustEqual Row(Array[ParameterValue](4, 5, 6), Map(("x", 0), ("y", 1), ("z", 2)))
    }
  }
}
