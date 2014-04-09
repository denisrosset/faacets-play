package controllers

import play.api.db.slick.Config.driver.simple.{Query => SlickQuery, Session => SlickSession}

trait SlickDataset extends DatasetBack {
  type DatasetSession = SlickSession
  type Query <: SlickQuery[_, Record]
  import play.api.db.slick.Config.driver.simple._
  def countTotalRecords(implicit session: DatasetSession) = query.length.run
  def countRecords(q: Query)(implicit session: DatasetSession) = q.length.run
  def retrieveRecords(q: Query, start: Int, length: Int)(implicit session: DatasetSession) = q.drop(start).take(length).list

  trait SlickCol[Data] extends Col[Data]
}
