package controllers

import play.api.templates._
import play.api._
import play.api.libs.json._

trait GenCell {
  def html: Html
  def json: JsValue
  def link: Option[String]
}

trait GenRow {
  def cols: Seq[GenCell]
}

trait GenDataTable {
  def rows: Seq[GenRow]
  def id: String
  def sel: String
  def dataTableJson: JsValue
  def columnFilterJson: JsValue
  def columnDefs: Seq[GenDataColumnDef]
}

trait GenDataColumnDef {
  def title: String
}

case class DataColumnDef[T](title: String,
  toHtml: T => Html, toJson: T => JsValue,
  sorting: Sorting = NoSorting, filter: Filter = NoFilter,
  toLink: Option[T => String] = None) extends GenDataColumnDef

case class DataTable[T](id: String, ajaxUrl: String, columnDefs: Seq[DataColumnDef[T]], rowData: Seq[T]) extends GenDataTable {
  case class Row(data: T) extends GenRow {
    def cols = columnDefs.map(Cell(_))
    case class Cell(columnDef: DataColumnDef[T]) extends GenCell {
      def json: JsValue = columnDef.toJson(data)
      def html: Html = columnDef.toHtml(data)
      def link: Option[String] = columnDef.toLink.map(_(data))
    }
  }
  def rows = rowData.map(Row(_))
  def sel = "#" + id
  def dataTableJson = JsObject(Seq(
    "bProcessing" -> JsBoolean(true),
    "sAjaxSource" -> JsString(ajaxUrl),
    "aoColumns" -> JsArray(columnDefs.map(_.sorting.json))
  ))
  def columnFilterJson = JsObject(Seq(
    "sRangeFormat" -> JsString("{from} to {to}"),
    "aoColumns" -> JsArray(columnDefs.map(_.filter.json))
  ))
}
