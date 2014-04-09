package controllers

object AlphaNumOrdering extends Ordering[String] {
  def groupIt(str:String) =
    if (str.nonEmpty && str.head.isDigit) str.takeWhile(_.isDigit)
    else str.takeWhile(!_.isDigit)

  val Dec = """(\d+)""".r

  def compare(str1: String, str2: String): Int = {
    (groupIt(str1), groupIt(str2)) match {
      case ("", "") => 0
      case (Dec(x), Dec(y)) if (x.toInt == y.toInt) =>
        compare(str1.substring(x.size), str2.substring(y.size))
      case (Dec(x), Dec(y)) => (x.toInt - y.toInt)
      case (x, y) if (x == y) =>
        compare(str1.substring(x.size), str2.substring(y.size))
      case (x, y) => x compareTo y
    }
  }
}
