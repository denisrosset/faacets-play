package controllers

import play.api.templates._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import com.faacets.db._
import comp._
import disk._


case class Highlight(id: String, title: String, short: String)

case class Tab(id: String, title: String, content: Html)

case class Tabs(id: String, tabs: Seq[Tab], activeIdOption: Option[String] = None) {
  def activeId = activeIdOption.getOrElse(tabs.head.id)
}

import play.api.db.slick.{Config => DBConfig, DBAction, DBSessionRequest}
import DBConfig.driver.simple.{Session => DBSession}

object Application extends Controller {

  def pathToUrl(path: Seq[Key]) = path.toList match {
    case Nil => "/db"
    case _ => "/db/" + path.mkString("/")
  }

  def extractUrl(ref: String): Html = SourceRef.option(ref) match {
    case None => Html((<div>{ref}</div>).toString)
    case Some(sref) => Html((<div><a href={sref.url}>{ref}</a></div>).toString)
  }

  implicit def stringToHTML(str: String) = HtmlFormat.escape(str)

  def compendium(implicit session: DBSession): DBDiskSplitComp = {
    val compPath = Play.current.configuration.getString("compendium.path").getOrElse("data")
    val compFolder = new java.io.File(compPath)
    val diskComp = new DiskComp { val baseFolder = compFolder }
    val dbComp = DatabaseComp(DBConfig.driver.profile)(session.database)
    new DBDiskSplitComp(dbComp, diskComp)
  }

  def branchDataTable(c: DBDiskSplitComp, branch: PathBranch)(implicit session: DBSession) = {
    val digests = c.readDigests(branch)

    /*
     def toRefs(that: AnyRef): Html = that match {
     case ie: StandardEntry => // TODO: refactor to avoid injection attacks
     HtmlFormat.raw(ie.canonicalEntries.map(entry => "<a href='" + anyRefToPath(entry) + "'>#" + entry.key.toString + "</a>").mkString(" "))
     case _ => ""
     }

     val crossColumns: Seq[DataColumn] = folder match {
     case f: StandardFolder =>
     Seq(DataColumn("Canonical", toRefs, sorting = AlphaNumSorting))
     case _ => Seq.empty[DataColumn]
     }*/
    val path = c.fullPath(branch)
    def booleanToString(b: Boolean) = b match {
      case true => "yes"
      case false => "no"
    }
    val columnDefs = Seq(
      DataColumnDef[(Key, BEDigest)]("Key", _._1.toString, sorting = AlphaNumSorting, StringFilter("#keyFilter"), Some(pair => pathToUrl(path :+ pair._1))),
      DataColumnDef[(Key, BEDigest)]("Scenario", _._2.scenario.toText,
        sorting = StringSorting),
      DataColumnDef[(Key, BEDigest)]("#P", _._2.scenario.numOfParties.toString,
        sorting = NumericSorting, filter = NumberRangeFilter("#partiesFilter")),
      DataColumnDef[(Key, BEDigest)]("#I", _._2.scenario.maxNumInputs.toString,
        sorting = NumericSorting, filter = NumberRangeFilter("#inputsFilter")),
      DataColumnDef[(Key, BEDigest)]("#O", _._2.scenario.maxNumOutputs.toString,
        sorting = NumericSorting, filter = NumberRangeFilter("#outputsFilter")),
      //      DataColumn("#reprs", ie(_).symmetryInfo.numberOfRepresentatives.toString,
      //      sorting = NumericSorting, filter = NumberRangeFilter("#reprFilter")),
      DataColumnDef[(Key, BEDigest)]("IO-Lifted?", pair => booleanToString(pair._2.isIOLifted), sorting = StringSorting, filter = SelectFilter("#ioLiftedFilter", Seq("yes", "no"))),
      DataColumnDef[(Key, BEDigest)]("Composite?", pair => booleanToString(pair._2.isComposite), sorting = StringSorting, filter = SelectFilter("#compositeFilter", Seq("yes", "no")))
    )
    /*,
     DataColumn("1st pub", ie(_).firstPubInfo.year.toString, sorting = NumericSorting),
     DataColumn("1st author", ie(_).firstPubInfo.firstAuthor, sorting = StringSorting)
     ) ++ crossColumns*/
    DataTable("branch", columnDefs, c.readDigests(branch).toSeq)
  }

  def db(pathStringSlashes: String) = DBAction { implicit request =>
    implicit val s = request.dbSession
    val path = pathStringSlashes.split("/").filterNot(_=="").map(Key(_)).toSeq
    val c = compendium(s)
    val node = c.nodeFromPath(path)
    node match {
      case branch: PathBranch =>
        Ok(views.html.folder(branch, c.branches(branch).toSeq, branchDataTable(c, branch)))
      case leaf: PathLeaf =>
        val be = c.read(leaf)
        Ok(views.html.inequalityentry(leaf, be))
    }
  }

  def dbroot = DBAction { implicit request =>
    val c = compendium(request.dbSession)
    Ok(views.html.dbroot(c.root(request.dbSession)))
  }

  def dbcanonical(id: Int) = Action {
    Redirect(s"/db/canonical/$id")
  }

  val wtfForm: Form[WTF] = Form(
    mapping(
      "scenario" -> text,
      "coeffs" -> nonEmptyText
    )(WTF.apply)(WTF.unapply)
  )
/*

  def wtf = Action {
    Ok(views.html.wtf(wtfForm, Text.fromMarkdownFile("syntax.md")))
  }

  def wtfPost = DBAction { implicit request =>
    implicit val dbSession = request.dbSession
    val c = compendium
    wtfForm.bindFromRequest.fold(
      formWithErrors => BadRequest,
      wtfdata => {
        val scenario = (wtfdata.scenario.trim == "") match {
          case true => Expr.parseExpression(wtfdata.coeffs).scenario
          case false => Scenario.fromText(wtfdata.scenario)
        }
        val expr = Expr.parseExpression(scenario)(wtfdata.coeffs)
        val bellExpression = BellExpression(expr = expr).completed.inCompendium(c)
        val vvh = new VecViewHelper(bellExpression)
        val (canonicalExpressions, expressions) = bellExpression.canonicalElements(c)


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
*/
}
