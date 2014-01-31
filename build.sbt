import play.Project._

name := "faacets-website"

version := "0.14-SNAPSHOTx"

playScalaSettings

libraryDependencies ++= Seq(
  "org.jbibtex" % "jbibtex" % "1.0.6",
  "org.yaml" % "snakeyaml" % "1.12",
  "org.spire-math" %% "spire" % "0.7.1"
)

lazy val root = project.in(file(".")).aggregate(core, polyta, alasc).dependsOn(core, polyta, alasc)

lazy val alasc = project.in(file("modules/alasc"))

lazy val polyta = project.in(file("modules/polyta")).dependsOn(alasc).aggregate(alasc)

lazy val core = project.in(file("modules/core")).dependsOn(polyta, alasc).aggregate(polyta)
