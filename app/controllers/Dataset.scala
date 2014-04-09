package controllers

trait DatasetBack extends Dataset
trait DatasetFront extends Dataset

trait Dataset {
  type Record
  type DatasetSession

  def cols: Seq[Col[_]]

  trait FiltCol[Data, Cond] extends Col[Data] {
    def performFilt(q: Query, cond: Cond)(implicit session: DatasetSession): Query
  }
  trait FiltBack[Data, Cond] extends FiltCol[Data, Cond]
  trait FiltFront[Data, Cond] extends FiltCol[Data, Cond]

  trait SortCol[Data] extends Col[Data] {
    def performSort(q: Query, dir: Dir)(implicit session: DatasetSession): Query
  }
  trait SortBack[Data] extends SortCol[Data]
  trait SortFront[Data] extends SortCol[Data]

  trait Col[Data] {
    def title: String
    def recordToData: Record => Data
  }

  type Query
  def query: Query

  def countTotalRecords(implicit session: DatasetSession): Int
  def countRecords(q: Query)(implicit session: DatasetSession): Int
  def retrieveRecords(q: Query, start: Int, length: Int)(implicit session: DatasetSession): Seq[Record]
}
