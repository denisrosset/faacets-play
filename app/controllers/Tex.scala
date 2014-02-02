package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import impl._
import perm._
import spire.math.Rational

sealed abstract class LineStyle {
  def rowTex: String
  def colTexChar: String
}

case object NoLineStyle extends LineStyle {
  def rowTex = ""
  def colTexChar = ""
}

case object SolidLineStyle extends LineStyle {
  def rowTex = """\hline"""
  def colTexChar = "|"
}

case object DashedLineStyle extends LineStyle {
  def rowTex = """\hdashline"""
  def colTexChar = ":"
}

sealed abstract class Align {
  def texChar: String
}

case object Left extends Align { def texChar = "l" }
case object Center extends Align { def texChar = "c" }
case object Right extends Align { def texChar = "r" }

trait Tex {
  def toTex: String
}

case class TexTable(val rows: Int, val cols: Int, val cells: Seq[Seq[Tex]], val rowFormat: Seq[LineStyle] = Seq.empty[LineStyle], val colFormat: Seq[LineStyle] = Seq.empty[LineStyle], val colAlign: Seq[Align] = Seq.empty[Align]) {
  def toTex = {
    def notEmptyOrElse[Tex](seq: Seq[Tex], elseValue: Seq[Tex]) = if (seq.isEmpty) elseValue else seq
    val colFormatChars = notEmptyOrElse[LineStyle](colFormat, Seq.fill(cols+1)(NoLineStyle)).map(_.colTexChar)
    val rowFormatSeq: Seq[LineStyle] = notEmptyOrElse[LineStyle](rowFormat, Seq.fill(rows+1)(NoLineStyle))
    val colAlignChars = notEmptyOrElse(colAlign, Seq.fill(cols)(Right)).map(_.texChar)
    val bg = """\begin{array}{""" + (colFormatChars zip colAlignChars).map {
      case (i,j) => i+j }.mkString("") + colFormatChars.last + "}"
    val dataRows = cells.map( row => row.map(_.toTex).mkString(" & ") )
    val formatRows = (rowFormatSeq.map(_.rowTex + """ \\ """) zip dataRows).map {
      case (i,j) => i+j }.mkString(""" \\ """)
    val ed = (rowFormatSeq.last match {
      case NoLineStyle => ""
      case other: LineStyle => """ \\ """ + other.rowTex
    }) + """\end{array}"""
    bg + formatRows + ed
  }
}

object Tex {
  def fromVec(vec: GenVec): String = {
    val repr = vec.scenario.Repr(vec.representation)
    val terms = repr.terms.map( t => "\\text{" + t.toString + "}" )
    val (coeffs, factor) = vec.coeffs.extractingFactor
    def rationalToTex(r: Rational): String = {
      if (r.denominator == 1) {
        if (r.numerator == 1)
          ""
        else
          r.numerator.toString
      } else s"\\frac{${r.numerator}}{${r.denominator}}"
    }
    val texTerms = (coeffs.elements zip terms).map {
      case (coeff, term) => new Tex {
        def toTex = s"\\tooltip{$coeff}{$term}"
      }
    }
    rationalToTex(factor) + " " + (vec.scenario.parties.length match {
      case 0 => ""
      case 1 => """\left ( """ + TexTable.fromNotation1(texTerms, vec.scenario, vec.representation) +  """\right )"""
      case 2 =>
        val rows = vec.scenario.parties(0).Repr(vec.representation).size
        val cols = vec.scenario.parties(1).Repr(vec.representation).size
        """\left ( """ + TexTable.fromNotation(Seq.tabulate(rows, cols)( (i,j) => texTerms(i + j * rows) ), vec.scenario, vec.representation).toTex + """\right )"""
      case _ => 
        val rows = vec.scenario.parties(0).Repr(vec.representation).size
        val cols = vec.scenario.parties(1).Repr(vec.representation).size
        val stride = rows*cols
        val n = vec.coeffs.length / stride
        (0 until n).map { k => 
          val coeffs = Seq.tabulate(rows, cols)( (i,j) => texTerms(i + j * rows + k * stride) )
          TexTable.fromNotation(coeffs, 
            Scenario(vec.scenario.parties.take(2)), vec.representation)
            .toTex
        }.mkString("""\left ( """, """ \quad """, """\right )""")
    })
  }
}

/*
  def niceTable = {
    scenario.parties.length match {
      case 0 => ""
      case 1 => """\left ( """ + TexTable.fromNotation1(texTerms, scenario, repr) +  """\right )"""
      case 2 => {
      }
      case _ => {
      }
    }
  }
 */

object TexTable {
  def fromNotation1(row: Seq[Tex], 
    scenario: Scenario, representation: Representation): TexTable = {
    assert(scenario.parties.length == 1)
    val party = scenario.parties(0)
    val partyRepr = party.Repr(representation)
    val cols = partyRepr.size
    val colFormat = Array.fill[LineStyle](cols + 1)(NoLineStyle)
    for (sep <- partyRepr.groups.flatten.scanLeft(0)(_+_).drop(1).dropRight(1))
      colFormat(sep) = DashedLineStyle
    for (sep <- partyRepr.groups.map(_.sum).scanLeft(0)(_+_).drop(1).dropRight(1))
      colFormat(sep) = SolidLineStyle
    val colAlignment = Array.fill[Align](cols)(Right)
    val cells = Seq(row)
    val rowFormat = Seq(NoLineStyle, NoLineStyle)
    TexTable(1, cols, cells, rowFormat, colFormat, colAlignment)
  }

  def fromNotation(cells: Seq[Seq[Tex]], 
    scenario: Scenario, representation: Representation): TexTable = {
    assert(scenario.parties.length == 2)
    val rowPartyRepr = scenario.parties(0).Repr(representation)
    val colPartyRepr = scenario.parties(1).Repr(representation)
    val rows = rowPartyRepr.size
    val cols = colPartyRepr.size
    val colAlignment = Array.fill[Align](cols)(Right)
    val colFormat = Array.fill[LineStyle](cols + 1)(NoLineStyle)
    val rowFormat = Array.fill[LineStyle](rows + 1)(NoLineStyle)
    val colDashedIndices = colPartyRepr.groups.flatten.scanLeft(0)(_+_)
    val colSolidIndices = colPartyRepr.groups.map(_.sum).scanLeft(0)(_+_)
    val rowDashedIndices = rowPartyRepr.groups.flatten.scanLeft(0)(_+_)
    val rowSolidIndices = rowPartyRepr.groups.map(_.sum).scanLeft(0)(_+_)
    if (!colPartyRepr.groups.flatten.forall(_ == 1))
      for (sep <- colDashedIndices.drop(1).dropRight(1))
        colFormat(sep) = DashedLineStyle
    for (sep <- colSolidIndices.drop(1).dropRight(1))
      colFormat(sep) = SolidLineStyle
    if (!rowPartyRepr.groups.flatten.forall(_ == 1))
      for (sep <- rowDashedIndices.drop(1).dropRight(1))
        rowFormat(sep) = DashedLineStyle
    for (sep <- rowSolidIndices.drop(1).dropRight(1))
      rowFormat(sep) = SolidLineStyle

    TexTable(rows, cols, cells, rowFormat, colFormat, colAlignment)
  }
}
