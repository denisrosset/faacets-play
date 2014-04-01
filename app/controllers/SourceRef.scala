package controllers

trait SourceRef {
  def url: String
  def key: String
}

object SourceRef {
  val DOIRegex = "doi:\\s?(10\\.\\d{4}\\/\\S+)".r
  val OldArXivRegex = """arXiv:(([\-a-zA-Z\.]+)/(\d+))""".r
  val NewArXivRegex = """arXiv:(\d+\.\d+(v\d+)?)""".r
  val URLRegex = "(https?://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|])".r
  def option(ref: String): Option[SourceRef] = ref match {
    case DOIRegex(key) => Some(DOIRef(key))
    case OldArXivRegex(key) => Some(ArXivRef(key))
    case NewArXivRegex(key) => Some(ArXivRef(key))
    case URLRegex(key) => Some(URLRef(key))
    case _ => None
  }
  def apply(ref: String): SourceRef = option(ref).get
}

case class URLRef(key: String) extends SourceRef {
  def url = key
}

case class ArXivRef(key: String) extends SourceRef {
  def url = "http://arxiv.org/abs/" + key
}

case class DOIRef(key: String) extends SourceRef {
  def url = "http://dx.doi.org/" + key
}
