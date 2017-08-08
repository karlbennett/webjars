lazy val root = (project in file(".")).enablePlugins(PlayScala)

name := "webjars"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  ws,
  ehcache,
  guice,
  filters,
  "io.monix" %% "shade" % "1.9.5",
  "commons-codec" % "commons-codec" % "1.10",
  "org.apache.commons" % "commons-compress" % "1.9",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.4.0.201606070830-r",
  "org.webjars" %% "webjars-play" % "2.6.1",
  "org.webjars.bower" % "bootstrap" % "3.3.4",
  "org.webjars.bower" % "select2" % "3.5.2",
  "org.webjars.bower" % "highlightjs" % "9.10.0",
  "org.webjars.bower" % "jquery-typewatch" % "2.2.1",
  specs2 % Test
)

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

scalacOptions ++= Seq("-unchecked", "-deprecation")

onLoad in Global := (onLoad in Global).value.andThen { state =>
  if (sys.props("java.specification.version") != "1.8") {
    sys.error("Java 8 is required for this project.")
    state.exit(ok = false)
  }
  else {
    state
  }
}

pipelineStages := Seq(gzip, digest)
