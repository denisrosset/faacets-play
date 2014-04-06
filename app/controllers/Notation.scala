package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.templates._
import com.faacets._
import spire.math.Rational
import pretty._

trait Notation {
  implicit val prettyParams = PrettyParams(true, Some(4), Center)
  def id: String
  def title: String
  def content: Html = HtmlFormat.escape("$$ " + texContent + " $$")
  def priority: Int
  def texContent: String = tex((bellExpression.lower, prettifiedExpr, bellExpression.upper))
  def bellExpression: BellExpression
  def prettifiedExpr: Prettified
  def style: String = "overflow: auto; white-space: nowrap"
}

case class SPTable(bellExpression: BellExpression) extends Notation {
  val id = "sptable"
  val title = "Sig. prob. table"
  val priority = 1
  val prettifiedExpr = prettified(bellExpression.expr.as(SPRepresentation).tensor)
}

case class SPText(bellExpression: BellExpression) extends Notation {
  val id = "sptext"
  val title = "SP expr."
  val priority = 2
  val prettifiedExpr = prettified(bellExpression.expr.as(SPRepresentation).expression)
}

case class NPTable(bellExpression: BellExpression) extends Notation {
  val id = "nptable"
  val title = "Non-sig. prob. table"
  val priority = 1
  val prettifiedExpr = prettified(bellExpression.expr.as(NPRepresentation).tensor)
}

case class NPText(bellExpression: BellExpression) extends Notation {
  val id = "nptext"
  val title = "NP expr."
  val priority = 2
  val prettifiedExpr = prettified(bellExpression.expr.as(NPRepresentation).expression)
}

case class NGTable(bellExpression: BellExpression) extends Notation {
  val id = "ngtable"
  val title = "Collins-Gisin table"
  val priority = 1
  val prettifiedExpr = prettified(bellExpression.expr.as(NGRepresentation).tensor)
}

case class NGText(bellExpression: BellExpression) extends Notation {
  val id = "ngtext"
  val title = "CG expr."
  val priority = 2
  val prettifiedExpr = prettified(bellExpression.expr.as(NGRepresentation).expression)
}

case class NCTable(bellExpression: BellExpression) extends Notation {
  val id = "nctable"
  val title = "Correlators table"
  val priority = 1
  val prettifiedExpr = prettified(bellExpression.expr.as(NCRepresentation).tensor)
}

case class NCText(bellExpression: BellExpression) extends Notation {
  val id = "nctext"
  val title = "NC expr."
  val priority = 2
  val prettifiedExpr = prettified(bellExpression.expr.as(NCRepresentation).expression)
}
