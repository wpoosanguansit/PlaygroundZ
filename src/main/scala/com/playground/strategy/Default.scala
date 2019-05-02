package com.playground.strategy

import Common._
import scalaz.zio._
import cats.implicits._
import java.io.IOException
import eu.timepit.refined._
import scalaz.zio.console._
import eu.timepit.refined.auto._
import shapeless.{::, HList, HNil}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.generic.Equal
import cats.collections.{AvlSet, Dequeue}
import eu.timepit.refined.boolean.{And, Or}
import eu.timepit.refined.char.{Letter, UpperCase, Whitespace}


object Default {

  type UpperCaseLetter        = UpperCase And Letter
  type CapitalSpacePound      = Equal[W.`'#'`.T] Or UpperCaseLetter Or Whitespace

  def processInputsZ(currentCell: Char Refined CapitalSpacePound,
                     upCell     : Char Refined CapitalSpacePound,
                     downCell   : Char Refined CapitalSpacePound,
                     leftCell   : Char Refined CapitalSpacePound,
                     rightCell  : Char Refined CapitalSpacePound)(env: Env): ZIO[Console, IOException, Env] = {
    val cells: HList      = (generateCell(currentCell)  ::
                              generateCell(upCell)      ::
                              generateCell(downCell)    ::
                              generateCell(leftCell)    ::
                              generateCell(rightCell)   ::
                              HNil)
    val x                 = env.currentPos.x
    val y                 = env.currentPos.y
    val top               = Point(x, y - 1)
    val bottom            = Point(x, y + 1)
    val left              = Point(x - 1, y)
    val right             = Point(x + 1, y)
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // Process the current
    ///////////////////////////////////////////////////////////////////////////////////////////////
    val preprocessed: Env = {
      val currentPos      = env.currentPos
      val unexplored      = (env.unexplored remove env.currentPos)
      val isNewCell       = !env.visited.contains(currentPos)
      val result          = if (isNewCell && isCapitalLetter(currentCell)) {
        (env.result cons currentCell)
      } else {
        env.result
      }
      val visited         = (env.visited add currentPos)
      val minTarget       = unexplored map { p => (env.distance(currentPos, p), p)  }
      val targetPos       = minTarget.min.map(_._2)
      env.hasReachedTarget match {
        case true =>
          env.copy(
            visited       = visited,
            result        = result,
            unexplored    = unexplored,
            traceBack     = Dequeue.empty[Point],
            targetPos     = targetPos,
            hasStarted    = true
          )
        case false =>
          env.copy(
            visited       = visited,
            result        = result,
            unexplored    = unexplored,
            traceBack     = (env.traceBack snoc currentPos),
            hasStarted    = true
          )
      }
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // We greedily move to the unoccupied cell right -> left and build up the unexplored.
    //////////////////////////////////////////////////////////////////////////////////////////////
    val unexplored        = preprocessed.unexplored
    val visited           = preprocessed.visited
    val traceBack         = preprocessed.traceBack
    val result: InProcessResult = cells match {
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: CharOrSpace(c4) :: HNil
        if (env.surroundedByNewCells)   =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](top, bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopBottomRight)   =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](top, bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: CharOrSpace(c2) :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopLeftRight)     =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](top, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopRight)         =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](top, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: HNil
        if (env.hasNewBottomLeftRight)  =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewBottomRight)      =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c3) :: CharOrSpace(c4) :: HNil
        if (env.hasNewLeftRight)        =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: _ :: _ :: _ :: CharOrSpace(c4) :: HNil
        if (env.hasNewRight)            =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewTopBottomLeft)    =>
        val nextPos       = left
        val distinct      = (unexplored ++ AvlSet[Point](top, bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "L")
      case CharOrSpace(c) :: _ :: CharOrSpace(c2) :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewBottomLeft)       =>
        val nextPos       = left
        val distinct      = (unexplored ++ AvlSet[Point](bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "L")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewTopLeft)          =>
        val nextPos       = left
        val distinct      = (unexplored ++ AvlSet[Point](top, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "L")
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewLeft)             =>
        val nextPos       = left
        val distinct      = (unexplored ++ AvlSet[Point](left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "L")
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: _ :: _ :: HNil
        if (env.hasNewTopBottom)        =>
        val nextPos       = top
        val distinct      = (unexplored ++ AvlSet[Point](top, bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "U")
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: _ :: HNil
        if (env.hasNewTop)              =>
        val nextPos       = top
        val distinct      = (unexplored ++ AvlSet[Point](top)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "U")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: _ :: HNil
        if (env.hasNewBottom)           =>
        val nextPos       = bottom
        val distinct      = (unexplored ++ AvlSet[Point](bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "D")
      //////////////////////////////////////////////////////////////////////////////////////////////
      // We move to target if it is next to the current cell
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: _ :: HNil
        if (env.targetIsToTop)          =>
        val nextPos       = top
        val distinct      = (unexplored ++ AvlSet[Point](top)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "U")
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: _ :: HNil
        if (env.targetIsToBottom)       =>
        val nextPos       = bottom
        val distinct      = (unexplored ++ AvlSet[Point](bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "D")
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c1) :: _ :: HNil
        if (env.targetIsToLeft)         =>
        val nextPos       = left
        val distinct      = (unexplored ++ AvlSet[Point](left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "L")
      case CharOrSpace(c) :: _ :: _ :: _ :: CharOrSpace(c1) :: HNil
        if (env.targetIsToRight)        =>
        val nextPos       = right
        val distinct      = (unexplored ++ AvlSet[Point](right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack, "R")
      //////////////////////////////////////////////////////////////////////////////////////////////
      // it has exhausted the cells around.  there is no new cell. we start to move towards
      // targetPos. We take all of the visited out of the unexplored as we go til it is
      // exhausted.
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(c) :: _ :: _ :: _ :: _ :: HNil =>
        findPathZ(preprocessed)
      case _                          =>
        InProcessResult(env.currentPos, unexplored, env.targetPos, traceBack, "This is invalid state. It should not have happened. No movement.")
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // we now processed and return.
    //////////////////////////////////////////////////////////////////////////////////////////////
    ZIO.succeed(preprocessed.copy(
      currentPos  = result.nextPos,
      unexplored  = result.distinct,
      targetPos   = result.targetPos,
      traceBack   = result.traceBack,
      movement    = result.movement)
    )
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
    val currentPos            = env.currentPos
    val neighbors             = env.getNeighbors
    val targetPos             = env.targetPos
    val unexplored            = env.unexplored
    val visited               = env.visited
    val traceBack             = env.traceBack
    targetPos match {
      case Some(target)       => {
        val steps             = if (traceBack.nonEmpty) env.traceBack else env.getPathFromCurrentToTarget
        val next              = steps.collectSomeFold[Dequeue[Point]](p => {
          if (!env.isNeighbor(p)) None else Option(Dequeue[Point](p))
        })
        val nextPos           = next.backOption match {
          case None           => (neighbors intersect visited).min.getOrElse(currentPos)
          case _ if (neighbors contains target) => target
          case Some(point)    => point
        }
        val updatedTraceBack  = steps.collectSomeFold[Dequeue[Point]](p => {
          if (p == nextPos || p == currentPos) None else Option(Dequeue[Point](p))
        })
        InProcessResult(nextPos, unexplored, targetPos, updatedTraceBack, movement(nextPos, currentPos))
      }
      case _ => {
        val nextPos           = (neighbors intersect visited).min.getOrElse(neighbors.min.getOrElse(currentPos))
        val minTarget         = unexplored map { p => (env.distance(env.currentPos, p), p)  }
        val updatedTargetPos  = minTarget.min.map(_._2)
        val updatedTraceBack  = traceBack.collectSomeFold[Dequeue[Point]](p => {
          if (p == nextPos || p == currentPos) None else Option(Dequeue[Point](p))
        })
        InProcessResult(nextPos, unexplored, updatedTargetPos, updatedTraceBack, movement(nextPos, currentPos))
      }
    }
  }
}
