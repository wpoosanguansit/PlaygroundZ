package com.playground.solution

import zio._
import java.io.IOException

import zio.console.{getStrLn, putStrLn}
import com.playground.strategy.Common._
import com.playground.strategy.Default._

object Main extends App {

  def run(args: List[String]) : ZIO[ZEnv, Nothing, ExitCode] = {
    game.provideCustomLayer(ambience).fold(_ => ExitCode.failure, _ => ExitCode.success)
  }

  val game: ZIO[Ambience, Throwable, Unit] = (for {
    logger  <- Logging.logger[this.type]
    _       <- logger.info(s"Main: game started")
      env   <- gameLoop
      result = ((env.result filter { c => isCapitalLetter(c.toString) }).sorted)
      _     <- putStrLn(s"The agent seems to have completed the grid. Please start again with a new session. The result is K${result.mkString}")
  } yield ())

  def gameLoop: ZIO[Ambience, Throwable, Env] = (for {
    array   <- getValidInput
    Array(current, up, down, left, right)     = array
    env     <- Config.env
    result  <- processInputsZ(current, up, down, left, right)(env)
    _       <- putStrLn(result.movement)
    _       <- Config.update(result)
  } yield {
    result
  }).repeat(Schedule.doUntil(env => { checkEndCondition(env) }))

  def getValidInput: ZIO[Ambience, IOException, Array[String]] = (for {
    input   <- getStrLn
    array   = convertStringToArray(input)
    _       <- if (array.length != 5) {
      putStrLn("Input has to be of 5 chars and contains only valid charaters A-Z, <space>, <line feed> and #")
    } else ZIO.succeed(())
  } yield {
    array
  }).repeat(Schedule.doUntil(array => { array.length == 5 }))

  def convertStringToArray(str: String): Array[String] = {
    str.length match {
      case 5 => {
        val array: Array[String]                  = str.toArray.map(_.toString)
        val Array(current, up, down, left, right) = array
        if (!isValidInput(current)  || !isValidInput(up) || !isValidInput(down) || !isValidInput(left) || !isValidInput(right)) {
          Array.empty[String]
        } else {
          array
        }
      }
      case _ => {
        Array.empty[String]
      }
    }
  }

  def checkEndCondition(env: Env): Boolean = (env.hasStarted && env.unexplored.isEmpty)
}
