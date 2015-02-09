package renesca.table

import renesca.parameter.ParameterValue
import renesca.json

object Table {
  def apply(columns:Seq[String], data:Traversable[Traversable[ParameterValue]]):Table = {
    val columnToIndex:Map[String,Int] = columns.zipWithIndex.toMap
    val rows:Array[Row] = data.map{ values =>
      val cells:Array[ParameterValue] = values.toArray
      new Row(cells, columnToIndex)
    }.toArray
    new Table(columns, rows)
  }
  
  def apply(columns:Seq[String], jsonData:Seq[json.Data]):Table = {
    val data = jsonData.map{_.row.get.value}
    Table(columns, data)
  }
}

case class Row private[table] (cells:IndexedSeq[ParameterValue], private[table] val columnToIndex:Map[String,Int] ) {
  def apply(column:String):ParameterValue = cells(columnToIndex(column))
}

case class Table private[table] (columns:Seq[String], rows:IndexedSeq[Row]) {
  def apply(index:Int):Row = rows(index)
}
