package com.playground.solution

import zio._
import cats.implicits._
import eu.timepit.refined.refineV
import com.playground.strategy.Common._
import com.playground.strategy.Default._
import zio.console.{ getStrLn, putStrLn }

object Main extends App {

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    game.provideCustomLayer(ambience).fold(_ => ExitCode.failure, _ => ExitCode.success)
  }

  val game: ZIO[Ambience, Throwable, Unit] =
    (for {
      logger <- Logger.logger[this.type]
      _      <- logger.info(s"Main: game started")
      env    <- gameLoop
      result  = env.result.map(_.value).toList.sorted
      _      <- putStrLn(s"The agent seems to have completed the grid. Please start again with a new session. The result is K${result.mkString}")
    } yield ()).catchAllCause(causes => putStrLn(causes.prettyPrint))

  def gameLoop: ZIO[Ambience, Throwable, Env] =
    (for {
      Array(current, up, down, left, right) <- getValidInput
      env                                   <- Config.env
      result                                <- processInputsZ(current, up, down, left, right)(env)
      _                                     <- putStrLn(result.movement)
      _                                     <- Config.update(result)
    } yield {
      result
    }).repeat[Ambience, Env](Schedule.recurUntil[Env](env => { checkEndCondition(env) }))

  def getValidInput: ZIO[Ambience, Throwable, Array[CharCapitalSpacePound]] =
    (for {
      input  <- getStrLn
      either  = convertStringToArray(input)
      result <- ZIO.fromEither(either).map(_.toArray)
      _      <- if (result.length != 5) {
                  putStrLn(s"Input has to be of 5 chars and contains only valid characters Capital A-Z, <space>, <line feed> and #.")
                } else ZIO.succeed(())
    } yield {
      result
    }).repeat[Ambience, Array[CharCapitalSpacePound]](Schedule.recurUntil[Array[CharCapitalSpacePound]](array => { array.length == 5 }))

  def convertStringToArray(str: String): Either[IllegalArgumentException, List[CharCapitalSpacePound]] = {
    if (str.length == 5) {
      str.toList.map(refineV[CapitalSpacePound](_)).sequence.left.map(new IllegalArgumentException(_))
    } else {
      Left(new IllegalArgumentException("Error: String passed in is not of length 5."))
    }
  }

  def checkEndCondition(env: Env): Boolean = env.hasStarted && env.unexplored.isEmpty
}
