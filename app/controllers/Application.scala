package controllers

import play.api.templates._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import com.faacets.db._
import comp._

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

    def canonicalKeysToHtml(keys: Seq[Int]) =
      HtmlFormat.raw(keys.map(index => "<a href='" + pathToUrl(Seq(Key("canonical"),Key(index))) + "'>#" + index.toString + "</a>").mkString(" "))

    val path = c.fullPath(branch)
    val crossColumn = path.map(_.toString) match {
      case Seq("canonical") => None
      case _ => 
        Some(DataColumnDef[(Key, BEDigest)]("Canonical", 
          pair => canonicalKeysToHtml(pair._2.canonicalKeys),
          sorting = AlphaNumSorting))
    }

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
    ) ++ crossColumn
    /*, TODO
     DataColumn("1st pub", ie(_).firstPubInfo.year.toString, sorting = NumericSorting),
     DataColumn("1st author", ie(_).firstPubInfo.firstAuthor, sorting = StringSorting)
     ) */
    DataTable("branch", columnDefs, c.readDigests(branch).toSeq)
  }

  def db(pathStringSlashesDisplay: String) = DBAction { implicit request =>
    implicit val s = request.dbSession
    val c = compendium(s)

    def showBranch(branch: PathBranch) = 
      Ok(views.html.folder(branch, c.branches(branch).toSeq.sortBy(_.key), branchDataTable(c, branch)))

    def showExpressionDisplay(leaf: PathLeaf) = {
      val be = c.read(leaf)
      val crossRefs = leaf.path.map(_.toString) match {
        case Seq("canonical", _) =>
          val leaves = c.readCrossRefsCanonicalIndex(leaf.key.intOption.get).distinct
          leaves.map(l => (l.path.mkString("/") -> pathToUrl(l.path)))
        case _ =>
          val canonicalIndices = c.readCrossRefsLeaf(leaf)
          canonicalIndices.map(index => ("#" + index.toString -> pathToUrl(Seq(Key("canonical"), Key(index)))))
      }
      Ok(views.html.expressiondisplay(leaf, be, crossRefs))
    }

    def showExpressionDecomposition(leaf: PathLeaf) = {
      val be = c.read(leaf)
      val dec = be.decomposition.get.inCompendium(c)
      val (graph, _) = DecToGraph(c, dec, Some(leaf.path.mkString("/")))
      Ok(views.html.expressiondecomposition(leaf, graph))
    }

    def showExpressionYAML(leaf: PathLeaf) = {
      val yaml = c.readYAML(leaf)
      Ok(views.html.expressionyaml(leaf, yaml))
    }

    val (pathStringSlashes, display) = pathStringSlashesDisplay.split('.') match {
      case Array(pss, d) => (pss, d)
      case Array(pss) => (pss, "display")
    }
    val path = pathStringSlashes.split("/").filterNot(_=="").map(Key(_)).toSeq
    val node = c.nodeFromPath(path)
    (node, display) match {
      case (branch: PathBranch, _) => showBranch(branch)
      case (leaf: PathLeaf, "decomposition") => showExpressionDecomposition(leaf)
      case (leaf: PathLeaf, "yaml") => showExpressionYAML(leaf)
      case (leaf: PathLeaf, _) => showExpressionDisplay(leaf)
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
        val dec = BellExpression(expr = expr).withRemarkableMaximal.withDecomposition.decomposition.get.inCompendium(c)
        val (graph, _) = DecToGraph(c, dec, Some("Your expression"))
        Ok(views.html.wtfresult(graph))
      })
  }
}
