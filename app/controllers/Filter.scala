package controllers

import play.api.templates._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import play.api.libs.json._

trait Filter {
  def json: JsValue
}

case object NoFilter extends Filter {
  def json = JsNull
}

case class StringFilter(sel: String) extends Filter {
  def json = JsObject(Seq(
    "sSelector" -> JsString(sel),
    "type" -> JsString("string")
  ))
}

case class NumberRangeFilter(sel: String) extends Filter {
  def json = JsObject(Seq(
    "sSelector" -> JsString(sel),
    "type" -> JsString("number-range")
  ))
}

case class SelectFilter(sel: String, values: Seq[String]) extends Filter {
  def json = JsObject(Seq(
    "sSelector" -> JsString(sel),
    "type" -> JsString("select"),
    "values" -> JsArray(values.map(JsString(_)))
  ))
}
