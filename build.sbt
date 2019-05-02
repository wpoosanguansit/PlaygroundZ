

import AssemblyKeys._

name                := "PlaygroundZ"

version             := "1.0"

scalaVersion        := "2.12.8"

resolvers ++= Seq(
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2"
)

libraryDependencies ++= Seq(
  "org.typelevel"           %% "cats-collections-core"  % "0.7.0",
  "eu.timepit"              %% "refined"                % "0.9.5",
  "com.chuusai"             %% "shapeless"              % "2.3.3",
  "org.specs2"              %% "specs2-core"            % "4.1.0" % "test",
  "org.scalaz"              %% "scalaz-zio"             % "1.0-RC4",
  "org.scalaz"              %% "scalaz-zio-streams"     % "1.0-RC4"
)

scalacOptions       ++= Seq(
  "-feature",
  "-Ypartial-unification"
)

initialCommands in console :=
  """import scalaz.zio._
    |import cats.implicits._
    |import eu.timepit.refined._
    |import java.io.IOException
    |import scalaz.zio.clock.Clock
    |import eu.timepit.refined.api.Refined
    |import com.playground.strategy.Default._
    |import scalaz.zio.console.{Console, getStrLn, putStrLn}
    |import com.playground.strategy.Common.{Env, isCapitalLetter}
  """.stripMargin

jarName   in assembly := "solution.jar"

mainClass in assembly := Some("com.playground.solution.Main")

mergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}