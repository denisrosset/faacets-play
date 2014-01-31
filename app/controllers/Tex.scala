package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import impl._
import perm._

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

case class TexTable[T <% Tex](val rows: Int, val cols: Int, val cells: Seq[Seq[T]], val rowFormat: Seq[LineStyle] = Seq.empty[LineStyle], val colFormat: Seq[LineStyle] = Seq.empty[LineStyle], val colAlign: Seq[Align] = Seq.empty[Align]) {
  def toTex = {
    def notEmptyOrElse[T](seq: Seq[T], elseValue: Seq[T]) = if (seq.isEmpty) elseValue else seq
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

}


object TexTable {
/*
  def fromNotation1[T <% Tex](row: Seq[T], scenario: Scenario, repr: Repr): TexTable = {
    assert(scenario.parties.length == 1)
    val party = scenario.parties(0)
    val partyRepr = repr.forParty(party)
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

  def fromNotation[T <% Tex](cells: Seq[Seq[T]], scenario: Scenario, repr: Repr): TexTable = {
    assert(scenario.parties.length == 2)
    val rowPartyRepr = repr.forParty(scenario.parties(0))
    val colPartyRepr = repr.forParty(scenario.parties(1))
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
  }*/
}
