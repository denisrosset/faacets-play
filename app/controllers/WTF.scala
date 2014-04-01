package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.templates._
import com.faacets._
import spire.math.Rational

case class WTF(scenario: String, coeffs: String)
