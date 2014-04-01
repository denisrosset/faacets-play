package controllers

import play.api.templates._
import play.api._
import play.api.mvc._
import com.faacets._

object Text extends Controller {
  val highlights = Seq(
    Highlight("randomness", "Randomness and Bell inequalities", "Randomness"),
    Highlight("loopholes", "Loopholes in Bell experiments", "Loopholes in experiments"),
    Highlight("characterization", "Bell inequalities and the characterization of non-locality", "Characterization of non-locality"),
    Highlight("freewill", "Measurement setting independence and experimental free will", "Measurement setting independence"),
    Highlight("randi", "The quantum Randi challenge", "The quantum Randi challenge"))

  def high(id: String) = Action {
    highlights.find(_.id == id) match {
      case Some(found) => Ok(views.html.highlight(found.title, fromMarkdownFile(s"${found.id}.md"), highlights))
      case None => NotFound
    }
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
