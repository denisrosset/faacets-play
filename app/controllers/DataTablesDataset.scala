package controllers
import play.api.libs.json._

trait DataTablesDataset extends DatasetFront {
  // To be extended by user 
  def id: String
  def jsonUrl: String
  def rangeFormat: String
  def cols: Seq[DataTablesCol[_]]

  trait DataTablesSortFront[Data] extends SortFront[Data] with DataTablesCol[Data] {
    def sType: String
    override def dataTablesConfigJsonSeq = (super.dataTablesConfigJsonSeq ++
      Seq("bSortable" -> JsBoolean(true), "sType" -> JsString(sType)))
  }

  trait AlphaNumDataTablesSortFront[Data] extends DataTablesSortFront[Data] {
    val sType = "alpha-num"
  }

  trait NumericDataTablesSortFront[Data] extends DataTablesSortFront[Data] {
    val sType = "numeric"
  }

  trait StringDataTablesSortFront[Data] extends DataTablesSortFront[Data] {
    val sType = "string"
  }

  trait NumberDataTablesFiltFront[Data] extends DataTablesFiltFront[Data, Int] {
    val stringToCond: (String => Int) = _.toInt
    val filtType = "number"
  }

  trait TextDataTablesFiltFront[Data] extends DataTablesFiltFront[Data, String] {
    val stringToCond: (String => String) = identity
    val filtType = "text"
  }

  trait DataTablesFiltFront[Data, Cond] extends FiltFront[Data, Cond] with DataTablesCol[Data] {
    def stringToCond: (String => Cond)
    def sSelector: String
    def filtType: String

    def performFiltUsingString(q: Query, string: String)(implicit session: DatasetSession) =
      performFilt(q, stringToCond(string))

    override def columnFilterConfigJsonSeq = (super.columnFilterConfigJsonSeq ++
      Seq("sSelector" -> JsString(sSelector), "type" -> JsString(filtType)))
  }

  import scala.util.Try

  def numberRange(str: String): (Option[Int], Option[Int]) =
    str.split('~') match {
      case Array(s1, s2) => (Try(s1.toInt).toOption, Try(s2.toInt).toOption)
      case Array(s1) => (Try(s1.toInt).toOption, None)
      case _ => (None, None)
    }

  trait NumberRangeDataTablesFiltFront[Data] extends DataTablesFiltFront[Data, (Option[Int], Option[Int])] {
    val filtType = "number-range"
    val stringToCond = numberRange(_)
  }

  trait SelectDataTablesFiltFront[Data, Cond] extends DataTablesFiltFront[Data, Cond] {
    val filtType = "select"
    def selectValues: Seq[String]
    override def columnFilterConfigJsonSeq = (super.columnFilterConfigJsonSeq ++
      Seq("values" -> JsArray(selectValues.map(JsString(_)))))
  }

  trait DataTablesCol[Data] extends Col[Data] {
    def dataToJson: (Data => JsValue)

    def recordToJson(record: Record) = dataToJson(recordToData(record))

    def dataTablesConfigJsonSeq = Seq.empty[(String, JsValue)]
    def dataTablesConfigJson = JsObject(dataTablesConfigJsonSeq)

    def columnFilterConfigJsonSeq = Seq.empty[(String, JsValue)]
    def columnFilterConfigJson = JsObject(columnFilterConfigJsonSeq)
  }

  def sel: String = "#" + id
  def dataTablesConfigJson: JsValue = JsObject(Seq(
    "bProcessing" -> JsBoolean(true),
    "sAjaxSource" -> JsString(jsonUrl),
    "bServerSide" -> JsBoolean(true),
    "aoColumns" -> JsArray(cols.map(_.dataTablesConfigJson))
  ))
  def columnFilterConfigJson: JsValue = JsObject(Seq(
    "sRangeFormat" -> JsString(rangeFormat),
    "aoColumns" -> JsArray(cols.map(_.columnFilterConfigJson))
  ))

  case class Request(echo: Int, displayStart: Int, displayLength: Int,
    sortColDir: Seq[(Int, Dir)], filters: Seq[(Int, String)])

  object Request {
    def apply(queryString: Map[String, Seq[String]]): Request = {
      val Some(Seq(AnInt(echo))) = queryString.get("sEcho")
      val Some(Seq(AnInt(displayStart))) = queryString.get("iDisplayStart")
      val Some(Seq(AnInt(displayLength))) = queryString.get("iDisplayLength")
      val Some(Seq(AnInt(sortingCols))) = queryString.get("iSortingCols")
      val sortColDir: Seq[(Int, Dir)] = (0 until sortingCols).map { i =>
        val Some(Seq(AnInt(sortCol))) = queryString.get(s"iSortCol_$i")
        val sortDir = queryString.get(s"sSortDir_$i") match {
          case Some(Seq("asc")) => Asc
          case Some(Seq("desc")) => Desc
        }
        (sortCol, sortDir)
      }.toSeq

      val filters: Seq[(Int, String)] = (0 until cols.length).flatMap { i =>
        queryString.get(s"sSearch_$i").flatMap {
          case Seq(search) => Some(i -> search)
          case _ => None
        }
      }

      Request(echo, displayStart, displayLength, sortColDir, filters)
    }
  }

  def recordJson(record: Record): JsValue = JsArray(cols.map(col => col.recordToJson(record)))

  def requestJson(queryString: Map[String, Seq[String]])(implicit session: DatasetSession): JsValue = {
    val request = Request(queryString)
    import request._
    val filteredQuery = (query /: filters) {
      case (q, (colInd, "")) => q
      case (q, (colInd, filterString)) => 
        val col = cols(colInd).asInstanceOf[DataTablesFiltFront[_, _]]
        col.performFiltUsingString(q, filterString)
    }
    val total = countTotalRecords
    val count = countRecords(filteredQuery)
    val sortedQuery = (filteredQuery /: sortColDir) {
      case (q, (colInd, dir)) => 
        val col = cols(colInd).asInstanceOf[SortCol[_]]
        col.performSort(q, dir)
    }

    val records = retrieveRecords(sortedQuery, displayStart, displayLength)
    val recordsJson = JsArray(records.map(recordJson))

    JsObject(Seq(
      "sEcho" -> JsString(echo.toString),
      "iTotalRecords" -> JsNumber(total),
      "iTotalDisplayRecords" -> JsNumber(count),
      "aaData" -> recordsJson
    ))
  }
}

object AnInt {
  import scala.util.Try
  def unapply(s: String): Option[Int] = Try(s.toInt).toOption
}
