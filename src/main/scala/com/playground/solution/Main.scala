package com.playground.solution

import scalaz.zio._
import cats.implicits._
import eu.timepit.refined._
import java.io.IOException
import scalaz.zio.clock.Clock
import eu.timepit.refined.api.Refined
import com.playground.strategy.Default._
import scalaz.zio.console.{Console, getStrLn, putStrLn}
import com.playground.strategy.Common.{Env, isCapitalLetter}

object Main extends App {
  
  type Ambience   = Console with Clock with Config
  
  type ValidChar  = Refined[Char, CapitalSpacePound]

  def run(args: List[String]) : ZIO[Environment, Nothing, Int] = {
    game.provide(Config.Live).fold(_ => 1, _ => 0)
  }

  val game: ZIO[Ambience, Throwable, Unit] = for {
      env <- gameLoop
      result = ((env.result.foldLeft(List.empty[Char]) { (accum, c) => if (!isCapitalLetter(c)) accum else (accum :+ c) }).sorted)
      _ <- putStrLn(s"""The agent seems to have completed the grid. Please start again with a new session. The result is K${result.mkString}""")
  } yield ()

  def gameLoop: ZIO[Ambience, Throwable, Env] = (for {
    array   <- getValidInput
    Array(current, up, down, left, right) = array
    env     <- ZIO.accessM[Config](_.config.get)
    result  <- processInputsZ(current, up, down, left, right)(env)
    _       <- putStrLn(result.movement)
    _       <- ZIO.accessM[Config](_.config.update(result))
  } yield {
    result
  }).repeat(Schedule.doUntil(env => { checkEndCondition(env) }))

  def getValidInput: ZIO[Ambience, IOException, Array[ValidChar]] = (for {
    input <- getStrLn
    array = convertStringToArray(input)
    _     <- if (array.length != 5) {
      putStrLn("Input has to be of 5 chars and contains only valid charaters A-Z, <space>, <line feed> and #")
    } else ZIO.succeed(())
  } yield {
    array
  }).repeat(Schedule.doUntil(array => { array.length == 5 }))

  def convertStringToArray(str: String): Array[ValidChar] = {
    str.length match {
      case 5  => {
        str.toList.map(refineV[CapitalSpacePound](_)).sequence match {
          case Left(_)      => Array.empty[ValidChar]
          case Right(list)  => list.toArray
        }
      }
      case _ => {
        Array.empty[ValidChar]
      }
    }
  }

  def checkEndCondition(env: Env): Boolean = (env.hasStarted && env.unexplored.isEmpty)
}
