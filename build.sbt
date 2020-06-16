
import sbt.Resolver

name := "PlaygroundZ"

version := "0.1"

scalaVersion := "2.13.2"

lazy val zioVersion                         = "1.0.0-RC20"
lazy val catsVersion                        = "2.0.0"
lazy val specsVersion                       = "4.8.2"
lazy val slf4jVersion                       = "1.7.30"
lazy val logbackVersion                     = "1.2.3"
lazy val zioCatsVersion                     = "2.0.0.0-RC10"
lazy val log4CatsVersion                    = "0.4.0-M1"
lazy val shapelessVersion                   = "2.3.3"
lazy val parallelCollectionsVersion         = "0.2.0"

lazy val commonSettings = Seq(
  version             := "0.1-SNAPSHOT",
  organization        := "com.playground",
  scalaVersion        := "2.13.2",
  test in assembly    := {},
  resolvers           ++= Seq(
    Resolver.sonatypeRepo("releases"),
    "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2"
  )
)

libraryDependencies ++= Seq(
  "org.typelevel"                 %% "cats-core"                              % catsVersion,
  "com.chuusai"                   %% "shapeless"                              % shapelessVersion,
  "org.specs2"                    %% "specs2-core"                            % specsVersion % "test",
  "dev.zio"                       %% "zio"                                    % zioVersion,
  "dev.zio"                       %% "zio-streams"                            % zioVersion,
  "dev.zio"                       %% "zio-interop-cats"                       % zioCatsVersion,
  "org.scala-lang.modules"        %% "scala-parallel-collections"             % parallelCollectionsVersion,
  "io.chrisdavenport"             %% "log4cats-core"                          % log4CatsVersion,
  "io.chrisdavenport"             %% "log4cats-slf4j"                         % log4CatsVersion,
  "org.slf4j"                     % "slf4j-log4j12"                           % slf4jVersion,
  "org.slf4j"                     % "slf4j-api"                               % slf4jVersion
)

scalacOptions       ++= Seq(
  "-Ywarn-unused:imports",
  "-Yrangepos"
)

initialCommands in console :=
  """import zio._
    |import zio.console.{getStrLn, putStrLn}
    |import com.playground.strategy.Common._
    |import com.playground.strategy.Default._
  """.stripMargin

assemblyJarName   in assembly := "solution.jar"

mainClass in assembly := Some("com.playground.solution.Main")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}