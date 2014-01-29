import sbt._
import Keys._
import play.Project._
import Keys._
 
object ApplicationBuild extends Build {

  val appName         = "FaacetsWebsite"
  val appVersion      = "0.14-SNAPSHOT"
  val appDependencies = Seq(
    "org.jbibtex" % "jbibtex" % "1.0.6",
    "org.yaml" % "snakeyaml" % "1.12",
    "org.spire-math" %% "spire" % "0.7.1"
  )

  val alasc = Project("alasc", file("modules/alasc"))
  val polyta = Project("polyta", file("modules/polyta")) dependsOn(alasc)
  val core = Project("core", file("modules/core")) dependsOn(alasc, polyta)

  val main = play.Project(
    appName, appVersion, appDependencies,
    settings = play.Project.defaultSettings
  ) dependsOn(alasc, polyta, core)
}
