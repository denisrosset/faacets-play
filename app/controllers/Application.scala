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
    import play.api.libs.json._
    import DBConfig.driver.simple._
    import DBConfig.driver.simple.{Query => SlickQuery}

    val c = compendium(session)

    val branchID: Int = c.indexComp.nodeFromPath(branch.path).id.get

    implicit val unit = ()

    val path = c.fullPath(branch)

    def booleanToString(b: Boolean) = b match {
      case true => "yes"
      case false => "no"
    }

    def canonicalKeysToHtml(keys: Seq[Int]) =
      HtmlFormat.raw(keys.map(index => "<a href='" + pathToUrl(Seq(Key("canonical"),Key(index))) + "'>#" + index.toString + "</a>").mkString(" "))

    import com.faacets.db._
    import c.indexComp.{Nodes, Expressions, ScenarioInfos}

    object dataset extends DataTablesDataset with SlickDataset {
      type Record = (DatabaseNode, DBExpression, ScenarioInfo)
      type LiftedRecord = (Nodes, Expressions, ScenarioInfos)
      type Query = SlickQuery[LiftedRecord, Record]

      val rangeFormat = "{from} to {to}"

      abstract class StringCol(val title: String, val recordToData: (Record => String)) extends StringDataTablesSortFront[String] with SlickCol[String] {
        val dataToJson = JsString(_)
        val dataToSort = identity[String](_)
      }

      trait StringFilter extends TextDataTablesFiltFront[String]

      abstract class IntCol(val title: String, val recordToData: (Record => Int)) extends SlickCol[Int] with NumericDataTablesSortFront[Int] {
        val dataToJson: (Int => JsValue) = JsNumber(_)
        val dataToSort = identity[Int](_)
      }

      trait IntRangeFilter extends NumberRangeDataTablesFiltFront[Int]

      trait BooleanFilter extends SelectDataTablesFiltFront[String, String] {
        val stringToCond = identity[String](_)
        val selectValues = Seq("yes", "no")
      }

      object KeyCol extends StringCol("Key", _._1.key.toString) with StringFilter {
        val sSelector = "#keyFilter"
        override val dataToJson: (String => JsString) = (keyString => JsString("<a href='" + pathToUrl(path :+ Key(keyString)) + "'>" + keyString + "</a>"))
        def performFilt(q: Query, cond: String)(implicit session: DatasetSession) = q.filter(_._1.stringKey like "%" + cond + "%")
        def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
          case Asc => q.sortBy(_._1.intKey.asc).sortBy(_._1.stringKey.asc)
          case Desc => q.sortBy(_._1.intKey.desc).sortBy(_._1.stringKey.desc)
        }
      }
      
      object ScenarioCol extends StringCol("Scenario", _._3.text) {
        def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
          case Asc => q.sortBy(_._3.text.asc)
          case Desc => q.sortBy(_._3.text.desc)
        }
      }

      object NumPCol extends IntCol("#P", _._3.numOfParties) with IntRangeFilter {
        val sSelector = "#partiesFilter"
        def performFilt(q: Query, cond: (Option[Int], Option[Int]))(implicit session: DatasetSession) = cond match {
          case (None, None) => q
          case (Some(lb), None) => q.filter(_._3.numOfParties >= lb)
          case (None, Some(ub)) => q.filter(_._3.numOfParties <= ub)
          case (Some(lb), Some(ub)) => q.filter(_._3.numOfParties >= lb).filter(_._3.numOfParties <= ub)
        }
        def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
          case Asc => q.sortBy(_._3.numOfParties.asc)
          case Desc => q.sortBy(_._3.numOfParties.desc)
        }
      }

      object NumICol extends IntCol("#I", _._3.maxNumInputs) with IntRangeFilter {
        val sSelector = "#inputsFilter"
        def performFilt(q: Query, cond: (Option[Int], Option[Int]))(implicit session: DatasetSession) = cond match {
          case (None, None) => q
          case (Some(lb), None) => q.filter(_._3.maxNumInputs >= lb)
          case (None, Some(ub)) => q.filter(_._3.maxNumInputs <= ub)
          case (Some(lb), Some(ub)) => q.filter(_._3.maxNumInputs >= lb).filter(_._3.maxNumInputs <= ub)
        }
        def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
          case Asc => q.sortBy(_._3.maxNumInputs.asc)
          case Desc => q.sortBy(_._3.maxNumInputs.desc)
        }
      }

      object NumOCol extends IntCol("#O", _._3.maxNumOutputs) with IntRangeFilter {
        val sSelector = "#outputsFilter"
        def performFilt(q: Query, cond: (Option[Int], Option[Int]))(implicit session: DatasetSession) = cond match {
          case (None, None) => q
          case (Some(lb), None) => q.filter(_._3.maxNumOutputs >= lb)
          case (None, Some(ub)) => q.filter(_._3.maxNumOutputs <= ub)
          case (Some(lb), Some(ub)) => q.filter(_._3.maxNumOutputs >= lb).filter(_._3.maxNumOutputs <= ub)
        }
        def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
          case Asc => q.sortBy(_._3.maxNumOutputs.asc)
          case Desc => q.sortBy(_._3.maxNumOutputs.desc)
        }
      }

      object LiftCol extends StringCol("IO-Lifted?", pair => booleanToString(pair._2.isIOLifted)) with BooleanFilter {
        val sSelector = "#ioLiftedFilter"
        def performFilt(q: Query, cond: String)(implicit session: DatasetSession) = cond match {
          case "yes" => q.filter(_._2.isIOLifted === true)
          case "no" => q.filter(_._2.isIOLifted === false)
          case _ => q
        }
        def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
          case Asc => q.sortBy(_._2.isIOLifted)
          case Desc => q.sortBy(_._2.isIOLifted)
        }
      }

      object CompositeCol extends StringCol("Composite?", pair => booleanToString(pair._2.isComposite)) with BooleanFilter {
        val sSelector = "#compositeFilter"
        def performFilt(q: Query, cond: String)(implicit session: DatasetSession) = cond match {
          case "yes" => q.filter(_._2.isComposite === true)
          case "no" => q.filter(_._2.isComposite === false)
          case _ => q
        }
        def performSort(q: Query, dir: Dir)(implicit session: DatasetSession) = dir match {
          case Asc => q.sortBy(_._2.isComposite)
          case Desc => q.sortBy(_._2.isComposite)
        }
      }

//      object CrossCol extends StringCol("Canonical", pair => canonicalKeysToHtml(pair._2.canonicalKeys).body)

      val cols: Seq[DataTablesCol[_]] = Seq(KeyCol, ScenarioCol, NumPCol, NumICol, NumOCol, LiftCol, CompositeCol)
// ++ (path.map(_.toString) match {
//        case Seq("canonical") => Seq.empty[DataTablesCol[_]]
//        case _ => Seq(CrossCol)
//      })

      val id = "table"
      val jsonUrl = pathToUrl(branch.path) + ".json"
      val query: Query = c.indexComp.expressionByNodeParentID(branchID)
    }


    dataset
  }

  def db(pathStringSlashesDisplay: String, sEcho: Option[String]) = DBAction { implicit request =>
    import play.api.libs.json._

    implicit val s = request.dbSession
    implicit val unit = ()
    val c = compendium(s)

    def showBranchJson(branch: PathBranch) = {
      val dt = branchDataTable(c, branch)
      Ok(dt.requestJson(request.queryString))
    }

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
      val be = Yaml.loadString[BellExpression](yaml)
      Ok(views.html.expressionyaml(leaf, yaml, be.decomposition.isEmpty))
    }

    val (pathStringSlashes, display) = pathStringSlashesDisplay.split('.') match {
      case Array(pss, d) => (pss, d)
      case Array(pss) => (pss, "display")
    }
    val path = pathStringSlashes.split("/").filterNot(_=="").map(Key(_)).toSeq
    val node = c.nodeFromPath(path)
    (node, display) match {
      case (branch: PathBranch, "json") => showBranchJson(branch)
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
