package controllers

import scala.math.Ordering

sealed trait Dir
case object Asc extends Dir
case object Desc extends Dir
