package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._

object Application extends Controller {
/*  def checkWTF(scenarioString: String, coeffsString: String): Boolean = {
    val parsedScenario = Scenario.fromTextDump(scenarioString)
    if (parsedScenario.isEmpty)
      return false
    val scenario = parsedScenario.get
    val parseResult = VecParser.parse(VecParser.bra(scenario), coeffsString)
    parseResult.successful
  }
  val wtfForm = Form(
    tuple(
      "scenario" -> nonEmptyText,
      "coeffsString" -> nonEmptyText
    ) verifying("Invalid inequality", params => params match { 
      case (scenarioString, coeffsString) => checkWTF(scenarioString, coeffsString)
    })
  )

  val naturalOrdering = new Ordering[String] {
    def groupIt(str:String) =
      if (str.nonEmpty && str.head.isDigit) str.takeWhile(_.isDigit)
      else str.takeWhile(!_.isDigit)
    val dec="""(\d+)"""r
    def compare(str1: String, str2: String) = {
      (groupIt(str1), groupIt(str2)) match {
        case ("","") => 0
        case (dec(x),dec(y)) if (x.toInt==y.toInt) =>
          compare(str1.substring(x.size), str2.substring(y.size))
        case (dec(x),dec(y)) => (x.toInt - y.toInt)
        case (x,y) if (x == y) =>
          compare(str1.substring(x.size), str2.substring(y.size))
        case (x,y) => x compareTo y
      }
    }
  }

  def dbroot = db("")

  def pathToUrl(path: Main.Path) = path.elements match {
    case Nil => "/db"
    case _ => "/db/" + path.elements.reverse.mkString("/")
  }

  def db(pathStringSlashes: String) = Action {
    val path = if(pathStringSlashes == "") com.faacets.data.root else Main.Path(pathStringSlashes.split("/").toList.reverse)
    path.file.isDirectory match {
      case true => {
        val children = path.children.toList.sorted(naturalOrdering).map(
          child => path.apply(child)
        )
        Ok(views.html.folder(path, children))
      }
      case false => {
        val bundle = Main.loadBundle(path)
        bundle.originalForm match {
          case ineq: InequalityObject.InequalityData => {
            val can = bundle.canonicalForm.asInstanceOf[InequalityObject.InequalityData]
            val tr = bundle.transform.asInstanceOf[InequalityObject.InequalityTransformObject.InequalityTransform]
            Ok(views.html.inequality(path, ineq, can, tr))
          }/*
          case box: BoxData => {
            val can = new Box
            Ok((views.html.box(box, can, tr)) ) // + (play.api.templates.Html(box.toHTML)))
          }*/
        }
      }
    }
  }
  def wtf = Action {
    Ok(views.html.wtf(""))
  }*/
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}
