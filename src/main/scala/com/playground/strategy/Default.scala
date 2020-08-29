package com.playground.strategy

import zio.ZIO
import Common._
import cats.implicits._
import cats.collections._
import zio.console.Console
import shapeless.{ ::, HList, HNil }
import eu.timepit.refined.cats.CatsRefinedTypeOpsSyntax
import eu.timepit.refined.api.{ Refined, RefinedTypeOps, Validate }

object Default {

  final case class Capital()
  final case class CapitalSpace()
  final case class CapitalSpacePound()

  type CharCapital           = Char Refined Capital
  type CharCapitalSpace      = Char Refined CapitalSpace
  type CharCapitalSpacePound = Char Refined CapitalSpacePound

  object CharCapital      extends RefinedTypeOps[CharCapital, Char] with CatsRefinedTypeOpsSyntax
  object CharCapitalSpace extends RefinedTypeOps[CharCapitalSpace, Char] with CatsRefinedTypeOpsSyntax

  object CapitalSpacePound {
    implicit def charCapitalSpacePoundValidate: Validate.Plain[Char, CapitalSpacePound] =
      Validate.fromPredicate(
        (char: Char) => (char.isUpper && char.isLetter) || char == '#' || char.isSpaceChar,
        (char: Char) => s"isCapitalSpacePound('$char')",
        CapitalSpacePound())
  }

  object CapitalSpace      {
    implicit def charCapitalSpaceValidate: Validate.Plain[Char, CapitalSpace] =
      Validate.fromPredicate(
        (char: Char) => (char.isUpper && char.isLetter) || char.isSpaceChar,
        (char: Char) => s"isCapitalSpace('$char')",
        CapitalSpace())
  }

  object Capital           {
    implicit def charCapitalValidate: Validate.Plain[Char, Capital] =
      Validate.fromPredicate((char: Char) => char.isUpper && char.isLetter && !char.isSpaceChar, (char: Char) => s"isCapital('$char')", Capital())
  }

  def processInputsZ(
      currentCell: CharCapitalSpacePound,
      upCell: CharCapitalSpacePound,
      downCell: CharCapitalSpacePound,
      leftCell: CharCapitalSpacePound,
      rightCell: CharCapitalSpacePound
    )(env: Env): ZIO[Console, Throwable, Env] = {
    val cells: HList = generateCell(currentCell).select[CharOrSpace].getOrElse(Wall) ::
      generateCell(upCell).select[CharOrSpace].getOrElse(Wall)    ::
      generateCell(downCell).select[CharOrSpace].getOrElse(Wall)  ::
      generateCell(leftCell).select[CharOrSpace].getOrElse(Wall)  ::
      generateCell(rightCell).select[CharOrSpace].getOrElse(Wall) ::
      HNil
    val x: Int                    = env.currentPos.x
    val y: Int                    = env.currentPos.y
    val top: Point                = Point(x, y - 1)
    val bottom: Point             = Point(x, y + 1)
    val left: Point               = Point(x - 1, y)
    val right: Point              = Point(x + 1, y)
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // Process the current
    ///////////////////////////////////////////////////////////////////////////////////////////////
    val preprocessed: Env         = {
      val currentPos: Point               = env.currentPos
      val unexplored: AvlSet[Point]       = env.unexplored remove env.currentPos
      val isNewCell: Boolean              = !env.visited.contains(currentPos)
      val isNotSpace: Boolean             = !isSpace(currentCell)
      val result: Dequeue[CharCapital]    = if (isNewCell && isNotSpace) {
        env.result cons CharCapital.unsafeFrom(currentCell.value)
      } else {
        env.result
      }
      val visited: AvlSet[Point]          = env.visited add currentPos
      val minTarget: AvlSet[(Int, Point)] = unexplored map { (point: Point) => (env.distance(currentPos, point), point) }
      val targetPos: Option[Point]        = minTarget.min.map(_._2)
      if (env.hasReachedTarget) {
        env.copy(
          visited    = visited,
          result     = result,
          unexplored = unexplored,
          traceBack  = Dequeue.empty[Point],
          targetPos  = targetPos,
          hasStarted = true)
      } else {
        env.copy(visited = visited, result = result, unexplored = unexplored, traceBack = env.traceBack snoc currentPos, hasStarted = true)
      }
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // We greedily move to the unoccupied cell right -> left and build up the unexplored.
    //////////////////////////////////////////////////////////////////////////////////////////////
    val unexplored: AvlSet[Point] = preprocessed.unexplored
    val visited: AvlSet[Point]    = preprocessed.visited
    val traceBack: Dequeue[Point] = preprocessed.traceBack
    val result: InProcessResult   = cells match {
      case CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: HNil if env.surroundedByNewCells =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](top, bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: _ :: CharOrSpace(_) :: HNil if env.hasNewTopBottomRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](top, bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: CharOrSpace(_) :: _ :: CharOrSpace(_) :: CharOrSpace(_) :: HNil if env.hasNewTopLeftRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](top, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: CharOrSpace(_) :: _ :: _ :: CharOrSpace(_) :: HNil if env.hasNewTopRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](top, right)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: _ :: CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: HNil if env.hasNewBottomLeftRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: _ :: CharOrSpace(_) :: _ :: CharOrSpace(_) :: HNil if env.hasNewBottomRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: _ :: _ :: CharOrSpace(_) :: CharOrSpace(_) :: HNil if env.hasNewLeftRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](left, right)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: _ :: _ :: _ :: CharOrSpace(_) :: HNil if env.hasNewRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored + right) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      case CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: _ :: HNil if env.hasNewTopBottomLeft =>
        val nextPos: Point          = left
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](top, bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "L")
      case CharOrSpace(_) :: _ :: CharOrSpace(_) :: CharOrSpace(_) :: _ :: HNil if env.hasNewBottomLeft =>
        val nextPos: Point          = left
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "L")
      case CharOrSpace(_) :: CharOrSpace(_) :: _ :: CharOrSpace(_) :: _ :: HNil if env.hasNewTopLeft =>
        val nextPos: Point          = left
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](top, left)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "L")
      case CharOrSpace(_) :: _ :: _ :: CharOrSpace(_) :: _ :: HNil if env.hasNewLeft =>
        val nextPos: Point          = left
        val distinct: AvlSet[Point] = (unexplored + left) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "L")
      case CharOrSpace(_) :: CharOrSpace(_) :: CharOrSpace(_) :: _ :: _ :: HNil if env.hasNewTopBottom =>
        val nextPos: Point          = top
        val distinct: AvlSet[Point] = (unexplored ++ AvlSet[Point](top, bottom)) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "U")
      case CharOrSpace(_) :: CharOrSpace(_) :: _ :: _ :: _ :: HNil if env.hasNewTop =>
        val nextPos: Point          = top
        val distinct: AvlSet[Point] = (unexplored + top) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "U")
      case CharOrSpace(_) :: _ :: CharOrSpace(_) :: _ :: _ :: HNil if env.hasNewBottom =>
        val nextPos: Point          = bottom
        val distinct: AvlSet[Point] = (unexplored + bottom) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "D")
      //////////////////////////////////////////////////////////////////////////////////////////////
      // We move to target if it is next to the current cell
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(_) :: CharOrSpace(_) :: _ :: _ :: _ :: HNil if env.targetIsToTop =>
        val nextPos: Point          = top
        val distinct: AvlSet[Point] = (unexplored + top) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "U")
      case CharOrSpace(_) :: _ :: CharOrSpace(_) :: _ :: _ :: HNil if env.targetIsToBottom =>
        val nextPos: Point          = bottom
        val distinct: AvlSet[Point] = (unexplored + bottom) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "D")
      case CharOrSpace(_) :: _ :: _ :: CharOrSpace(_) :: _ :: HNil if env.targetIsToLeft =>
        val nextPos: Point          = left
        val distinct: AvlSet[Point] = (unexplored + left) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "L")
      case CharOrSpace(_) :: _ :: _ :: _ :: CharOrSpace(_) :: HNil if env.targetIsToRight =>
        val nextPos: Point          = right
        val distinct: AvlSet[Point] = (unexplored + right) diff visited
        InProcessResult(nextPos, distinct, Option[Point](nextPos), traceBack, "R")
      //////////////////////////////////////////////////////////////////////////////////////////////
      // it has exhausted the cells around.  there is no new cell. we start to move towards
      // targetPos. We take all of the visited out of the unexplored as we go til it is
      // exhausted.
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(_) :: _ :: _ :: _ :: _ :: HNil =>
        findPathZ(preprocessed)
      case _ =>
        InProcessResult(env.currentPos, unexplored, env.targetPos, traceBack, "This is invalid state. It should not have happened. No movement.")
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // we now processed and return.
    //////////////////////////////////////////////////////////////////////////////////////////////
    ZIO.succeed(
      preprocessed.copy(
        currentPos = result.nextPos,
        unexplored = result.distinct,
        targetPos  = result.targetPos,
        traceBack  = result.traceBack,
        movement   = result.movement))
  }

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
  //////////////////////////////////////////////////////////////////////////////////////////////
  // We just find our way back to the targetPos
  //////////////////////////////////////////////////////////////////////////////////////////////
  def findPathZ(env: Env): InProcessResult = {
    val currentPos: Point         = env.currentPos
    val neighbors: AvlSet[Point]  = env.getNeighbors
    val targetPos: Option[Point]  = env.targetPos
    val unexplored: AvlSet[Point] = env.unexplored
    val visited: AvlSet[Point]    = env.visited
    val traceBack: Dequeue[Point] = env.traceBack
    targetPos match {
      case Some(target) =>
        val steps: Dequeue[Point]            = if (traceBack.nonEmpty) env.traceBack else env.getPathFromCurrentToTarget
        val next: Dequeue[Point]             = steps.collectFold[Dequeue[Point]]((point: Point) => {
          if (!env.isNeighbor(point)) Dequeue.empty[Point] else Dequeue[Point](point)
        })
        val nextPos: Point                   = next.backOption match {
          case None                           => (neighbors intersect visited).min.getOrElse(currentPos)
          case _ if neighbors contains target => target
          case Some(point)                    => point
        }
        val updatedTraceBack: Dequeue[Point] = steps.collectFold[Dequeue[Point]]((point: Point) => {
          if (point == nextPos || point == currentPos) Dequeue.empty[Point] else Dequeue[Point](point)
        })
        InProcessResult(nextPos, unexplored, targetPos, updatedTraceBack, movement(nextPos, currentPos))
      case _            =>
        val nextPos: Point                   =
          (neighbors intersect visited).min.getOrElse(neighbors.min.getOrElse(currentPos))
        val minTarget: AvlSet[(Int, Point)]  = unexplored map { (point: Point) => (env.distance(env.currentPos, point), point) }
        val updatedTargetPos: Option[Point]  = minTarget.min.map(_._2)
        val updatedTraceBack: Dequeue[Point] = traceBack.collectFold[Dequeue[Point]]((point: Point) => {
          if (point == nextPos || point == currentPos) Dequeue.empty[Point] else Dequeue[Point](point)
        })
        InProcessResult(nextPos, unexplored, updatedTargetPos, updatedTraceBack, movement(nextPos, currentPos))
    }
  }
}
