import sbt._

class ScalazPlaygroundProject(info: ProjectInfo) extends DefaultProject(info)
{
  val specs = "org.scala-tools.testing" % "specs" % "1.6.0"
  
}