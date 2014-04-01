package controllers

import play.api.templates._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import play.api.libs.json._

trait Sorting {
  def json: JsValue
}

case object NoSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(false)
  ))
}

case object AlphaNumSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(true),
    "sType" -> JsString("alpha-num")
  ))
}

case object NumericSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(true),
    "sType" -> JsString("numeric")
  ))
}

case object StringSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(true),
    "sType" -> JsString("string")
  ))
}
