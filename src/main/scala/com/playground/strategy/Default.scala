package com.playground.strategy

import java.io.IOException

import shapeless.{::, HList, HNil}
import Common._
import zio._
import zio.console._

object Default {

  def processInputsZ(currentCell: String,
                     upCell: String,
                     downCell: String,
                     leftCell: String,
                     rightCell: String)(env: Env): ZIO[Console, IOException, Env] = {
    val cells: HList = (generateCell(currentCell) ::
      generateCell(upCell)                        ::
      generateCell(downCell)                      ::
      generateCell(leftCell)                      ::
      generateCell(rightCell)                     ::
      HNil)
    val x = env.currentPos.x
    val y = env.currentPos.y
    val top = Point(x, y - 1)
    val bottom = Point(x, y + 1)
    val left = Point(x - 1, y)
    val right = Point(x + 1, y)
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // Process the current
    ///////////////////////////////////////////////////////////////////////////////////////////////
    val preprocessed: Env = {
      val currentPos = env.currentPos
      val unexplored = (env.unexplored - env.currentPos)
      val isNewCell = !env.visited.toList.contains(currentPos)
      val result = if (isNewCell && isCapitalLetter(currentCell)) {
        (env.result ++ List[Char](currentCell.charAt(0)))
      } else {
        env.result
      }
      val visited = env.visited + currentPos
      val minTarget = if (!env.unexplored.isEmpty) unexplored map {
        env.distance(env.currentPos, _)
      } else {
        Set.empty[Int]
      }
      val targetPos = if (!minTarget.isEmpty) {
        val minTargetIndex = minTarget.zipWithIndex.minBy(_._1)._2
        Option(unexplored.toList(minTargetIndex))
      } else {
        None
      }
      (env.hasReachedTarget, env.hasNewCurrent) match {
        case (true, true) =>
          env.copy(
            visited       = visited,
            result        = result,
            unexplored    = unexplored,
            traceBack     = List.empty[Point],
            targetPos     = targetPos,
            hasStarted    = true
          )
        case (true, false) =>
          env.copy(
            visited       = visited,
            result        = result,
            unexplored    = unexplored,
            traceBack     = List.empty[Point],
            targetPos     = targetPos,
            hasStarted    = true
          )
        case (false, true) =>
          env.copy(
            visited       = visited,
            result        = result,
            unexplored    = unexplored,
            hasStarted    = true
          )
        case (false, false) =>
          env.copy(
            visited       = visited,
            result        = result,
            unexplored    = unexplored,
            hasStarted    = true
          )
      }
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // We greedily move to the unoccupied cell right -> left and build up the unexplored.
    //////////////////////////////////////////////////////////////////////////////////////////////
    val unexplored        = preprocessed.unexplored
    val visited           = preprocessed.visited
    val result: InProcessResult = cells match {
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: CharOrSpace(c4) :: HNil
        if (env.surroundedByNewCells) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](top, bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopBottomRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](top, bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: CharOrSpace(c2) :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopLeftRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](top, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](top, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: HNil
        if (env.hasNewBottomLeftRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewBottomRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c3) :: CharOrSpace(c4) :: HNil
        if (env.hasNewLeftRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: _ :: _ :: _ :: CharOrSpace(c4) :: HNil
        if (env.hasNewRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewTopBottomLeft) =>
        val nextPos = left
        val distinct = (unexplored ++ Set[Point](top, bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "L")
      case CharOrSpace(c) :: _ :: CharOrSpace(c2) :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewBottomLeft) =>
        val nextPos = left
        val distinct = (unexplored ++ Set[Point](bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "L")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewTopLeft) =>
        val nextPos = left
        val distinct = (unexplored ++ Set[Point](top, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "L")
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewLeft) =>
        val nextPos = left
        val distinct = (unexplored ++ Set[Point](left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "L")
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: _ :: _ :: HNil
        if (env.hasNewTopBottom) =>
        val nextPos = top
        val distinct = (unexplored ++ Set[Point](top, bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "U")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: _ :: HNil
        if (env.hasNewTop) =>
        val nextPos = top
        val distinct = (unexplored ++ Set[Point](top)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "U")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: _ :: HNil
        if (env.hasNewBottom) =>
        val nextPos = bottom
        val distinct = (unexplored ++ Set[Point](bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "D")
      //////////////////////////////////////////////////////////////////////////////////////////////
      // We move to target if it is next to the current cell
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: _ :: HNil
        if (env.targetIsToTop) =>
        val nextPos = top
        val distinct = (unexplored ++ Set[Point](top)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "U")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: _ :: HNil
        if (env.targetIsToBottom) =>
        val nextPos = bottom
        val distinct = (unexplored ++ Set[Point](bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "D")
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c1) :: _ :: HNil
        if (env.targetIsToLeft) =>
        val nextPos = left
        val distinct = (unexplored ++ Set[Point](left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "L")
      case CharOrSpace(c) :: _ :: _ :: _ :: CharOrSpace(c1) :: HNil
        if (env.targetIsToRight) =>
        val nextPos = right
        val distinct = (unexplored ++ Set[Point](right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), env.traceBack :+ env.currentPos, "R")
      //////////////////////////////////////////////////////////////////////////////////////////////
      // it has exhausted the cells around.  there is no new cell. we start to move towards
      // targetPos. We take all of the visited out of the unexplored as we go til it is
      // exhausted.
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(c) :: _ :: _ :: _ :: _ :: HNil =>
        findPathZ(preprocessed)
      case _ =>
        InProcessResult(env.currentPos, unexplored, env.targetPos, env.traceBack :+ env.currentPos, "This is invalid state. It should not have happened. No movement.")
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // we now processed and return.
    //////////////////////////////////////////////////////////////////////////////////////////////
    ZIO.succeed(preprocessed.copy(
      currentPos  = result.nextPos,
      unexplored  = result.distinct,
      targetPos   = result.targetPos,
      traceBack   = result.traceBack,
      movement    = result.movement))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////
  // We just find our way back to the targetPos
  //////////////////////////////////////////////////////////////////////////////////////////////
  private def movement(nextPos: Point, currentPos: Point): String = {
    if (nextPos isHigherThan currentPos) {
      "U"
    } else if (nextPos isLowerThan currentPos) {
      "D"
    } else if (nextPos isRightOf currentPos) {
      "R"
    } else if (nextPos isLeftOf currentPos) {
      "L"
    } else {
      "This should not happen. nextPos is the same as currentPos"
    }
  }
  def findPathZ(env: Env): InProcessResult = {
    val currentPos          = env.currentPos
    val neighbors           = env.getNeighbors
    val targetPos           = env.targetPos
    val unexplored          = env.unexplored
    targetPos match {
      case Some(target)     => {
        val steps = env.traceBack match {
          case Nil          => env.getPathFromCurrentToTarget
          case _            => env.traceBack
        }
        val next: List[Point] = steps filter { env.isNeighbor(_) }
        val nextPos         = next match {
          case Nil          => env.visited.filter(env.isNeighbor(_)).headOption.getOrElse(currentPos)
          case _ if (neighbors.toList contains target) => target
          case _            => next.head
        }
        val traceBack       = steps.dropWhile(p => p == nextPos || p == currentPos)
        InProcessResult(nextPos, unexplored, targetPos, traceBack, movement(nextPos, currentPos))
      }
      case _                => {
        val nextPos         = (neighbors intersect env.visited).headOption.getOrElse(neighbors.headOption.getOrElse(currentPos))
        val targetPos       = unexplored.headOption
        val traceBack       = env.traceBack.dropWhile(p => p == nextPos || p == currentPos)
        InProcessResult(nextPos, unexplored, targetPos, traceBack, movement(nextPos, currentPos))
      }
    }
  }
}
