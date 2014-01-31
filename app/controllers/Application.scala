package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import com.faacets._
import impl._

object Application extends Controller {
  val root = RootFolder(new java.io.File("data"))

  def pathToUrl(path: Seq[Key]) = path.toList match {
    case Nil => "/db"
    case _ => "/db/" + path.mkString("/")
  }

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

 */
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
  def dbroot = db("")
  def db(pathStringSlashes: String) = Action {
    val item = getItem(pathStringSlashes.split("/").filterNot(_=="").map(Key(_)).toList)
    item match {
      case folder: Folder =>
        Ok(views.html.folder(folder))
      case ie: InequalityEntry =>
        Ok(views.html.inequality(ie))
    }
  }
/*
  def wtf = Action {
    Ok(views.html.wtf(""))
  }
 */
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}
