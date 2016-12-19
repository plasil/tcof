name := "mpm-ensembles"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  // Required for mpmens
  "org.scala-lang" % "scala-reflect" % "2.12.0",
  "org.choco-solver" % "choco-solver" % "4.0.0",

  // Required for Map2D
  "de.ummels" %% "scala-prioritymap" % "1.0.0",

  // Required for RoboRescue
  "org.apache.logging.log4j" % "log4j-api" % "2.7",
  "org.apache.logging.log4j" % "log4j-core" % "2.7",
  "org.apache.logging.log4j" % "log4j-1.2-api" % "2.7",
  "org.uncommons.maths" % "uncommons-maths" % "1.2.2a",
  "trove" % "trove" % "1.0.2"
)

cleanFiles += (baseDirectory(_ / "precomputed.data")).value
