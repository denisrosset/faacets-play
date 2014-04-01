package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.templates._
import com.faacets._
import pretty._
import spire.math.Rational

class VecViewHelper(val bellExpression: BellExpression) {
  def optFrom(bool: Boolean): Option[Unit] = bool match {
    case true => Some( () )
    case false => None
  }
  val notations: Seq[Notation] = {
    val maxOutputs = bellExpression.scenario.parties.flatMap(_.inputs).max
    val unsortedNotations: Seq[Notation] = 
      bellExpression.representation.isSignaling match {
        case true => Seq(SPTable(bellExpression), SPText(bellExpression))
        case false => Seq(NPTable(bellExpression), NGTable(bellExpression),
          NPText(bellExpression), NGText(bellExpression)) ++ (maxOutputs match {
            case 2 => Seq(NCTable(bellExpression), NCText(bellExpression))
            case _ => Seq()
          })
      }
    unsortedNotations.sortBy(_.priority)
  }
}
