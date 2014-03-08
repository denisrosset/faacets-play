package controllers

import play.api.templates._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import impl._

import play.api.libs.json._

case class Highlight(id: String, title: String, short: String)

case class Tab(id: String, title: String, content: Html)

case class Tabs(id: String, tabs: Seq[Tab], activeIdOption: Option[String] = None) {
  def activeId = activeIdOption.getOrElse(tabs.head.id)
}

trait Filter {
  def json: JsValue
}

case object NoFilter extends Filter {
  def json = JsNull
}

case class StringFilter(sel: String) extends Filter {
  def json = JsObject(Seq(
    "sSelector" -> JsString(sel),
    "type" -> JsString("string")
  ))
}

case class NumberRangeFilter(sel: String) extends Filter {
  def json = JsObject(Seq(
    "sSelector" -> JsString(sel),
    "type" -> JsString("number-range")
  ))
}

case class SelectFilter(sel: String, values: Seq[String]) extends Filter {
  def json = JsObject(Seq(
    "sSelector" -> JsString(sel),
    "type" -> JsString("select"),
    "values" -> JsArray(values.map(JsString(_)))
  ))
}

trait Sorting {
  def json: JsValue
}

case object NoSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(false)
  ))
}

case object AlphaNumSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(true),
    "sType" -> JsString("alpha-num")
  ))
}

case object NumericSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(true),
    "sType" -> JsString("numeric")
  ))
}

case object StringSorting extends Sorting {
  def json = JsObject(Seq(
    "bSortable" -> JsBoolean(true),
    "sType" -> JsString("string")
  ))
}

case class DataColumn(title: String, colData: AnyRef => Html, sorting: Sorting = NoSorting, filter: Filter = NoFilter, colLink: Option[AnyRef => String] = None)

case class DataTable(id: String, cols: Seq[DataColumn], rows: Seq[AnyRef]) {
  def sel = "#" + id
  def dataTableJson = JsObject(Seq(
    "aoColumns" -> JsArray(cols.map(_.sorting.json))
  ))
  def columnFilterJson = JsObject(Seq(
    "sRangeFormat" -> JsString("{from} to {to}"),
    "aoColumns" -> JsArray(cols.map(_.filter.json))
  ))
}

case class WTF(scenario: String, coeffs: String)

object Application extends Controller {
  val root = RootFolder(new java.io.File("data"))

  val highlights = Seq(
    Highlight("randomness", "Randomness and Bell inequalities", "Randomness"),
    Highlight("loopholes", "Loopholes in Bell experiments", "Loopholes in experiments"),
    Highlight("characterization", "Bell inequalities and the characterization of non-locality", "Characterization of non-locality"),
    Highlight("freewill", "Measurement setting independence and experimental free will", "Measurement setting independence"),
    Highlight("randi", "The quantum Randi challenge", "The quantum Randi challenge"))

  def pathToUrl(path: Seq[Key]) = path.toList match {
    case Nil => "/db"
    case _ => "/db/" + path.mkString("/")
  }

  implicit def stringToHTML(str: String) = HtmlFormat.escape(str)

  def getItem(pathElements: List[Key], current: Entry = root): Entry =
    (pathElements, current) match {
      case (Nil, ie: InequalityEntry) => ie
      case (hd :: tl, ie: InequalityEntry) =>
        throw new IllegalArgumentException("Cannot browse path past inequality")
      case (Nil, f: Folder) => f
      case (hd :: tl, f: Folder) => f.get(hd) match {
        case Some(entry) => getItem(tl, entry)
        case None => throw new IllegalArgumentException(s"Did not find key $hd in folder ${current.path}")
      }
    }

  def high(id: String) = Action {
    highlights.find(_.id == id) match {
      case Some(found) => Ok(views.html.highlight(found.title, fromMarkdownFile(s"${found.id}.md"), highlights))
      case None => NotFound
    }
  }

  def anyRefToLifted(that: AnyRef): String = {
    val tags = that.asInstanceOf[InequalityEntry].inequality.tags
    if (tags.contains("pure"))
      return "no"
    if (tags.contains("degenerate"))
      return "yes"
    return ""
  }

  def anyRefToProduct(that: AnyRef): String = {
    val tags = that.asInstanceOf[InequalityEntry].inequality.tags
    if (tags.contains("simple"))
      return "no"
    if (tags.contains("composite"))
      return "yes"
    return ""
  }

  def anyRefToPath(that: AnyRef): String = Application.pathToUrl(that.asInstanceOf[Entry].path)

  def folderDataTable(folder: Folder) = {
    def ie(that: AnyRef) = that.asInstanceOf[InequalityEntry]
    def toRefs(that: AnyRef): Html = that match {
      case ie: StandardEntry => // TODO: refactor to avoid injection attacks
        HtmlFormat.raw(ie.canonicalEntries.map(entry => "<a href='" + anyRefToPath(entry) + "'>#" + entry.key.toString + "</a>").mkString(" "))
      case _ => ""
    }
    val crossColumns: Seq[DataColumn] = folder match {
      case f: StandardFolder =>
        Seq(DataColumn("Canonical", toRefs, sorting = AlphaNumSorting))
      case _ => Seq.empty[DataColumn]
    }
    val columns = Seq(
      DataColumn("Key", ie(_).key.toString, sorting = AlphaNumSorting, StringFilter("#keyFilter"), Some(anyRefToPath)),
      DataColumn("Scenario", ie(_).inequality.bra.scenario.toText,
        sorting = StringSorting),
      DataColumn("#P", ie(_).scenarioInfo.numOfParties.toString,
        sorting = NumericSorting, filter = NumberRangeFilter("#partiesFilter")),
      DataColumn("#I", ie(_).scenarioInfo.maxNumInputs.toString,
        sorting = NumericSorting, filter = NumberRangeFilter("#inputsFilter")),
      DataColumn("#O", ie(_).scenarioInfo.maxNumOutputs.toString,
        sorting = NumericSorting, filter = NumberRangeFilter("#outputsFilter")),
      DataColumn("#reprs", ie(_).symmetryInfo.numberOfRepresentatives.toString,
        sorting = NumericSorting, filter = NumberRangeFilter("#reprFilter")),
      DataColumn("Lifting?", anyRefToLifted(_), sorting = StringSorting, filter = SelectFilter("#liftedFilter", Seq("yes", "no"))),
      DataColumn("Product?", anyRefToProduct(_), sorting = StringSorting, filter = SelectFilter("#productFilter", Seq("yes", "no"))),
      DataColumn("1st pub", ie(_).firstPubInfo.year.toString, sorting = NumericSorting),
      DataColumn("1st author", ie(_).firstPubInfo.firstAuthor, sorting = StringSorting)
    ) ++ crossColumns
    DataTable("folder", columns, folder.inequalities.values.toSeq)
  }

  def dbcanonical(id: Int) = Action {
    Redirect(s"/db/canonical/$id")
  }

  def db(pathStringSlashes: String) = Action {
    val item = getItem(pathStringSlashes.split("/").filterNot(_=="").map(Key(_)).toList)
    item match {
      case folder: Folder =>
        Ok(views.html.folder(folder, folderDataTable(folder)))
      case ie: InequalityEntry =>
        Ok(views.html.inequalityentry(ie))
    }
  }

  def dbroot = Action {
    Ok(views.html.dbroot(root, folderDataTable(root)))
  }

  val doiRegex = "doi:\\s?(10\\.\\d{4}\\/\\S+)".r
  def doiReplace(str: String) = doiRegex.replaceAllIn(str,
    m => """<a href="http://dx.doi.org/%s">%s</a>""" format (m.matched, m.matched))
  val oldArxivRegex = """arXiv:(([\-a-zA-Z\.]+)/(\d+))""".r
  def oldArxivReplace(str: String) = oldArxivRegex.replaceAllIn(str,
    m => """<a href="http://arxiv.org/abs/%s">%s</a>""" format (m.group(0), m))
  val newArxivRegex = """arXiv:(\d+\.\d+(v\d+)?)""".r
  def newArxivReplace(str: String) = newArxivRegex.replaceAllIn(str,
    m => """<a href="http://arxiv.org/abs/%s">%s</a>""" format (m.group(0), m))
  def httpReplace(str: String) = str.startsWith("http") match {
    case true =>
      val name = str.split("/").last
      s"<a href='$str'>$name</a>"
    case false => str
  }
  // TODO: security check of URL
  def extractUrl(s: String) = 
    scala.xml.XML.loadString("<div>" + httpReplace(doiReplace(newArxivReplace(oldArxivReplace(s)))) + "</div>")

  val wtfForm: Form[WTF] = Form(
    mapping(
      "scenario" -> text,
      "coeffs" -> nonEmptyText
    )(WTF.apply)(WTF.unapply)
  )

  def wtf = Action {
    Ok(views.html.wtf(wtfForm, fromMarkdownFile("syntax.md")))
  }

  def wtfPost = Action { implicit request =>
    wtfForm.bindFromRequest.fold(
      formWithErrors => BadRequest,
      wtfdata => {
        val scenario = (wtfdata.scenario.trim == "") match {
          case true => Bra.fromText(wtfdata.coeffs).scenario
          case false => Scenario.fromText(wtfdata.scenario)
        }
        val bra = Bra.fromText(wtfdata.coeffs, scenario)
        val inequality = ExplicitInequality(bra = bra).withDecomposition
        val vvh = new VecViewHelper(inequality.bra)
        val indatabase: Seq[scala.Either[CanonicalEntry, VecViewHelper]] = 
          inequality.canonical.map { ineq =>
            val indb = root.canonical.inequalityIndex.get(ineq.bra)
            indb match {
              case Some(ce) => scala.Left(ce)
              case None => scala.Right(new VecViewHelper(ineq.bra))
            }
          }
        val recognized = indatabase.flatMap(_.left.toOption)
          .map(ce => ce.key.toString.toInt -> ce).toMap
        val other = indatabase.flatMap(_.right.toOption)
          .zipWithIndex.map { case (vvh, ind) => (ind -> vvh) }.toMap
        Ok(views.html.wtfresult(vvh, HtmlFormat.escape(Yaml.saveString(inequality.decomposition.get.stripped)), recognized, other))
      })
  }

  def fromMarkdownFile(filename: String): Html = {
    import java.io.File // TODO: check for unsafe key
    import scala.io.Source
    val content = Source.fromFile(new File(s"content/$filename")).getLines().mkString("\n")
    val html = HtmlFormat.raw(new org.pegdown.PegDownProcessor().markdownToHtml(content))
    html
  }

  def about = Action {
    Ok(views.html.about(fromMarkdownFile("about.md")))
  }

  def index = Action {
    Ok(views.html.index(highlights))
  }
}
