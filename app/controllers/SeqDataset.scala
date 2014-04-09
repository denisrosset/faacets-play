package controllers
import scala.math.Ordering

trait SeqDataset extends DatasetBack {
  type DatasetSession = Unit
  type Query = Seq[Record]
  def query: Seq[Record]

  trait SeqFiltBack[Data, Cond, Filt] extends FiltBack[Data, Cond] {
    def predicate(filt: Filt, cond: Cond): Boolean
    val dataToFilt: (Data => Filt)
    def performFilt(q: Query, cond: Cond)(implicit session: DatasetSession) =
      q.filter(record => predicate(dataToFilt(recordToData(record)), cond))
  }

  trait SeqSortBack[Data, Sort] extends SortBack[Data] {
    val ordering: Ordering[Sort]
    val dataToSort: (Data => Sort)
    def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
      case Asc => q.sortBy(record => dataToSort(recordToData(record)))(ordering)
      case Desc => q.sortBy(record => dataToSort(recordToData(record)))(ordering.reverse)
    }
  }

  trait AlphaNumSeqSortBack[Data] extends SeqSortBack[Data, String] {
    val ordering = AlphaNumOrdering
  }

  trait NumericSeqSortBack[Data] extends SeqSortBack[Data, Int] {
    val ordering = Ordering.Int
  }

  trait StringSeqSortBack[Data] extends SeqSortBack[Data, String] {
    val ordering: Ordering[String] = Ordering.String
  }

  trait StringContainsSeqFiltBack[Data] extends SeqFiltBack[Data, String, String] {
    def predicate(filt: String, cond: String) = filt.contains(cond)
  }

  trait StringStartsWithSeqFiltBack[Data] extends SeqFiltBack[Data, String, String] {
    def predicate(filt: String, cond: String) = filt.startsWith(cond)
  }

  trait NumberRangeSeqFiltBack[Data] extends SeqFiltBack[Data, (Option[Int], Option[Int]), Int] {
    def predicate(filt: Int, cond: (Option[Int], Option[Int])) =
      (cond._1.map( filt >= _ ).getOrElse(true) &&
        cond._2.map( filt <= _ ).getOrElse(true))
  }

  trait StringIsSeqFiltBack[Data] extends SeqFiltBack[Data, String, String] {
    def predicate(filt: String, cond: String) = (filt == cond)
  }

  def countTotalRecords(implicit session: DatasetSession) = query.length
  def countRecords(q: Query)(implicit session: DatasetSession) = q.length
  def retrieveRecords(q: Query, start: Int, length: Int)(implicit session: DatasetSession) = q.drop(start).take(length)

  trait SeqCol[Data] extends Col[Data]
}
