package com.playground.strategy

import Default._
import cats.Order
import zio.ZLayer
import cats.implicits._
import zio.clock.Clock
import cats.collections._
import zio.console.Console
import zio.blocking.Blocking
import eu.timepit.refined.api.Refined
import shapeless.{ :+:, CNil, Coproduct }
import com.playground.solution.Config.Config
import com.playground.solution.Logger.Logging
import com.playground.solution.{ Config, Logger }

import scala.annotation.tailrec
import scala.collection.immutable.{ Map => SMap }

object Common {

  type Ambience = Console with Clock with Blocking with Config with Logging
  val ambience: ZLayer[Any, _, Ambience] = Console.live ++ Clock.live ++ Blocking.live ++ Config.live ++ Logger.live

  case class InProcessResult(
      nextPos: Point,
      distinct: AvlSet[Point]   = AvlSet.empty[Point],
      targetPos: Option[Point]  = None,
      traceBack: Dequeue[Point] = Dequeue.empty[Point],
      movement: String          = "")

  implicit val pointOrder: Order[Point]                                = (x: Point, y: Point) => {
    if (x == y) {
      0
    } else {
      val result: Int = x.x compare y.x
      result match {
        case 0 => x.y compare y.y
        case _ => result
      }
    }
  }

  implicit val capitalSpacePoundOrder: Ordering[CharCapitalSpacePound] = (x: Refined[Char, CapitalSpacePound], y: Refined[Char, CapitalSpacePound]) =>
    {
      x.value compare y.value
    }

  case class Point(x: Int, y: Int) {
    def isLeftOf(that: Point): Boolean     = this.x < that.x
    def isRightOf(that: Point): Boolean    = this.x > that.x
    def isHigherThan(that: Point): Boolean = this.y < that.y
    def isLowerThan(that: Point): Boolean  = this.y > that.y
  }

  case class Env(
      currentPos: Point            = Point(0, 0),
      visited: AvlSet[Point]       = AvlSet.empty[Point],
      unexplored: AvlSet[Point]    = AvlSet.empty[Point],
      traceBack: Dequeue[Point]    = Dequeue.empty[Point],
      result: Dequeue[CharCapital] = Dequeue.empty[CharCapital],
      targetPos: Option[Point]     = None,
      hasStarted: Boolean          = false,
      movement: String             = "") {

    type Path = List[Point]

    val x: Int        = currentPos.x
    val y: Int        = currentPos.y
    val top: Point    = Point(x, y - 1)
    val bottom: Point = Point(x, y + 1)
    val left: Point   = Point(x - 1, y)
    val right: Point  = Point(x + 1, y)
    val start: Point  = Point(0, 0)

    @tailrec
    private def dijkstra(lookup: SMap[Point, Path], fringe: List[Path], dest: Point, visited: AvlSet[Point]): Path =
      fringe match {
        case path :: fringeRest =>
          path match {
            case key :: _ =>
              if (key == dest) {
                path.reverse
              } else {
                val paths: List[Path] = lookup(key) flatMap { (key: Point) =>
                  if (!visited.contains(key)) List[Path](key :: path) else Nil
                }
                val temp: List[Path]  = paths ++ fringeRest
                dijkstra(lookup, temp, dest, visited + key)
              }
            case _ => List.empty[Point]
          }
        case Nil => List.empty[Point]
      }

    def getPathFromCurrentToTarget: Dequeue[Point]                                                                 = {
      val targets: AvlSet[Point]    = getTargetNeighbors
      val lookup: SMap[Point, Path] = getLookUpMap
      Dequeue.fromFoldable(dijkstra(lookup, List[Path](List[Point](currentPos)), targets.min.getOrElse(start), AvlSet.empty[Point]))
    }

    def isNeighbor(point: Point): Boolean                                                                          =
      point == top || point == left || point == right || point == bottom
    def getNeighbors: AvlSet[Point]                                                                                =
      visited intersect AvlSet(top, bottom, left, right)
    def getTargetNeighbors: AvlSet[Point]                                                                          = {
      targetPos match {
        case Some(target) => getNeighborsFor(target)
        case _            => AvlSet.empty[Point]
      }
    }

    def getNeighborsFor(current: Point): AvlSet[Point]                                                             = {
      visited intersect AvlSet(
        Point(current.x - 1, current.y),
        Point(current.x + 1, current.y),
        Point(current.x, current.y - 1),
        Point(current.x, current.y + 1))
    }

    def surroundedByNewCells: Boolean  = hasNewTop && hasNewBottom && hasNewLeft && hasNewRight
    def surroundedByOldCells: Boolean  = !surroundedByNewCells
    def hasNewTopBottomRight: Boolean  = hasNewTop && hasNewBottom && hasNewRight
    def hasNewTopBottomLeft: Boolean   = hasNewTop && hasNewBottom && hasNewLeft
    def hasNewTopBottom: Boolean       = hasNewTop && hasNewBottom
    def hasNewTopLeftRight: Boolean    = hasNewTop && hasNewLeft && hasNewRight
    def hasNewTopRight: Boolean        = hasNewTop && hasNewRight
    def hasNewTopLeft: Boolean         = hasNewTop && hasNewLeft
    def hasNewBottomLeftRight: Boolean = hasNewBottom && hasNewLeft && hasNewRight
    def hasNewBottomRight: Boolean     = hasNewBottom && hasNewRight
    def hasNewBottomLeft: Boolean      = hasNewBottom && hasNewLeft
    def hasNewLeftRight: Boolean       = hasNewLeft && hasNewRight
    def hasNewTop: Boolean             = !visited.toList.contains(top)
    def hasNewBottom: Boolean          = !visited.toList.contains(bottom)
    def hasNewLeft: Boolean            = !visited.toList.contains(left)
    def hasNewRight: Boolean           = !visited.toList.contains(right)
    def hasNewCurrent: Boolean         = !visited.toList.contains(currentPos)
    def hasReachedTarget: Boolean      = Option[Point](currentPos) == targetPos
    def targetIsToLeft: Boolean        = targetPos.contains(left)
    def targetIsToRight: Boolean       = targetPos.contains(right)
    def targetIsToTop: Boolean         = targetPos.contains(top)
    def targetIsToBottom: Boolean      = targetPos.contains(bottom)
    def targetIsToTopRight: Boolean    = targetIsToTop && targetIsToRight
    def targetIsToBottomRight: Boolean = targetIsToBottom && targetIsToRight
    def targetIsToTopLeft: Boolean     = targetIsToTop && targetIsToLeft
    def targetIsToBottomLeft: Boolean  = targetIsToBottom && targetIsToLeft
    def hasBeenExhausted: Boolean      = this.hasStarted && this.unexplored.isEmpty

    def distance(point1: Point, point2: Point): Int = Math.abs(point1.x - point2.x) + Math.abs(point1.y - point2.y)

    def getLookUpMap: SMap[Point, Path] = {
      visited.foldLeft(SMap.empty[Point, List[Point]])((acc, point) => {
        acc ++ SMap[Point, Path](point -> getNeighborsFor(point).toList)
      })
    }
  }

  val initial: Env = Env()
  def isWall(char: Char): Boolean                           = char == '#'
  def isWall(char: String): Boolean                         = isWall(char.charAt(0))
  def isWall(char: CharCapitalSpacePound): Boolean          = isWall(char.toString)
  def isCapitalLetter(char: Char): Boolean                  = char.isLetter && char.isUpper
  def isCapitalLetter(char: String): Boolean                = isCapitalLetter(char.charAt(0))
  def isCapitalLetter(char: CharCapitalSpacePound): Boolean = isCapitalLetter(char.toString)
  def isSpace(char: Char): Boolean                          = char == ' '
  def isSpace(char: String): Boolean                        = isSpace(char.charAt(0))
  def isSpace(char: CharCapitalSpacePound): Boolean         = isSpace(char.toString)

  type Cell = Wall.type :+: CharOrSpace :+: CNil

  case object Wall
  case class CharOrSpace(character: CharCapitalSpace)

  def generateCell(char: CharCapitalSpacePound): Cell = {
    char.value match {
      case '#'                           => Coproduct[Cell](Wall)
      case char if isCapitalLetter(char) => Coproduct[Cell](CharOrSpace(CharCapitalSpace.unsafeFrom(char)))
      case char if isSpace(char)         => Coproduct[Cell](CharOrSpace(CharCapitalSpace.unsafeFrom(' ')))
    }
  }
}
