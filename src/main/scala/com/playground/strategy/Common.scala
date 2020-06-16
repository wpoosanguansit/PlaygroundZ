package com.playground.strategy

import zio.clock.Clock
import zio.console.Console
import zio.blocking.Blocking
import com.playground.solution.Config.Config
import com.playground.solution.Logging.Logging
import com.playground.solution.{Config, Logging}

import scala.annotation.tailrec


object Common {

  type Ambience = Console with Clock with Blocking with Config with Logging
  val ambience  = Config.live ++ Logging.live

  case class InProcessResult(nextPos  : Point,
                             distinct : Set[Point],
                             targetPos: Option[Point],
                             traceBack: List[Point] = List.empty[Point],
                             movement: String = "")

  case class Point(x: Int, y: Int) {
    def isLeftOf(that: Point): Boolean      = this.x < that.x
    def isRightOf(that: Point): Boolean     = this.x > that.x
    def isHigherThan(that: Point): Boolean  = this.y < that.y
    def isLowerThan(that: Point): Boolean   = this.y > that.y
  }

  case class Env(currentPos   : Point = Point(0,0),
                 visited      : Set[Point]  = Set.empty[Point],
                 unexplored   : Set[Point]  = Set.empty[Point],
                 traceBack    : List[Point] = List.empty[Point],
                 result       : List[Char]  = List.empty[Char],
                 targetPos    : Option[Point] = None,
                 hasStarted   : Boolean   = false,
                 movement     : String = "") {
    val x                                 = currentPos.x
    val y                                 = currentPos.y
    val top                               = Point(x, y - 1)
    val bottom                            = Point(x, y + 1)
    val left                              = Point(x - 1, y)
    val right                             = Point(x + 1, y)


    type Path                             = List[Point]

    @tailrec
    private def dijkstra(lookup  : Map[Point, Path],
                         fringe    : List[Path],
                         dest      : Point,
                         visited   : Set[Point]): Path = fringe match {
      case path   :: fringeRest =>
        path match {
          case key :: pathRest  =>
            if (key == dest) {
              path.reverse
            } else {
              val paths         = lookup(key) flatMap {
                case key => if (!visited.contains(key)) List[Path](key :: path) else Nil

              }
              val temp          = paths ++ fringeRest
              dijkstra(lookup, temp, dest, visited + key)
            }
          case _                => List.empty[Point]
        }
      case Nil                  => List.empty[Point]
    }

    def getPathFromCurrentToTarget: List[Point] = {
      val targets               = getTargetNeighbors
      val lookup                = getLookUpMap
      dijkstra(lookup, List[Path](List[Point](currentPos)), targets.head, Set.empty[Point])
    }

    def isNeighbor(point: Point): Boolean = point == top || point == left  || point == right || point == bottom
    def getNeighbors: Set[Point]          = {
      (visited filter { p => p == top || p == bottom    || p == left || p == right })
    }
    def getTargetNeighbors: Set[Point]    = {
      targetPos match {
        case Some(target) => (visited filter { p => p == Point(target.x - 1, target.y) ||
            p == Point(target.x + 1, target.y) ||
            p == Point(target.x, target.y - 1) ||
            p == Point(target.x, target.y + 1)
          })
        case _            =>  Set.empty[Point]
      }
    }
    def getNeighborsFor(current :Point): Set[Point]   = {
      (visited filter { p => p == Point(current.x - 1, current.y) ||
        p == Point(current.x + 1, current.y) ||
        p == Point(current.x, current.y - 1) ||
        p == Point(current.x, current.y + 1)
      })
    }
    def surroundedByNewCells: Boolean     = hasNewTop    && hasNewBottom  && hasNewLeft   && hasNewRight
    def hasNewTopBottomRight: Boolean     = hasNewTop    && hasNewBottom  && hasNewRight
    def hasNewTopBottomLeft: Boolean      = hasNewTop    && hasNewBottom  && hasNewLeft
    def hasNewTopBottom: Boolean          = hasNewTop    && hasNewBottom
    def hasNewTopLeftRight: Boolean       = hasNewTop    && hasNewLeft    && hasNewRight
    def hasNewTopRight: Boolean           = hasNewTop    && hasNewRight
    def hasNewTopLeft: Boolean            = hasNewTop    && hasNewLeft
    def hasNewBottomLeftRight: Boolean    = hasNewBottom && hasNewLeft    && hasNewRight
    def hasNewBottomRight: Boolean        = hasNewBottom && hasNewRight
    def hasNewBottomLeft: Boolean         = hasNewBottom && hasNewLeft
    def hasNewLeftRight: Boolean          = hasNewLeft   && hasNewRight
    def hasNewTop: Boolean                = !visited.toList.contains(top)
    def hasNewBottom: Boolean             = !visited.toList.contains(bottom)
    def hasNewLeft: Boolean               = !visited.toList.contains(left)
    def hasNewRight: Boolean              = !visited.toList.contains(right)
    def hasNewCurrent: Boolean            = !visited.toList.contains(currentPos)
    def hasReachedTarget: Boolean         = Option(currentPos) == targetPos
    def targetIsToLeft: Boolean           = targetPos.map(_ == left).getOrElse(false)
    def targetIsToRight: Boolean          = targetPos.map(_ == right).getOrElse(false)
    def targetIsToTop: Boolean            = targetPos.map(_ == top).getOrElse(false)
    def targetIsToBottom: Boolean         = targetPos.map(_ == bottom).getOrElse(false)
    def targetIsToTopRight: Boolean       = targetIsToTop     && targetIsToRight
    def targetIsToBottomRight: Boolean    = targetIsToBottom  && targetIsToRight
    def targetIsToTopLeft: Boolean        = targetIsToTop     && targetIsToLeft
    def targetIsToBottomLeft: Boolean     = targetIsToBottom  && targetIsToLeft
    def hasBeenExhausted: Boolean         = this.hasStarted && this.unexplored.isEmpty

    def distance(p1: Point, p2: Point): Int = Math.abs(p1.x-p2.x) + Math.abs(p1.y-p2.y)

    def getLookUpMap: Map[Point, Path]    = {
      visited.foldLeft(Map.empty[Point, List[Point]])((acc, p) => {
        acc ++ Map[Point, Path](p -> getNeighborsFor(p).toList)
      })
    }
  }

  val initial                             = Env()

  def isValidInput(s: String): Boolean    = isLineSeparator(s) || isCapitalLetter(s) || isSpace(s) || isWall(s)

  def isWall(s: String): Boolean          = s.equals("#")

  def isCapitalLetter(s: String): Boolean = s.matches("^([A-Z])+$")

  def isSpace(s: String): Boolean         = s.matches("\\s")

  val lineSeparator: String               = System.getProperty("line.separator")

  def isLineSeparator(s: String): Boolean = s.equals(lineSeparator)

  trait       Cell
  case object Wall                            extends Cell
  case class  CharOrSpace(character: Char)    extends Cell

  def generateCell(s: String): Cell = {
    s match {
      case "#"                            => Wall
      case c  if isCapitalLetter(c)       => CharOrSpace(c.charAt(0))
      case sp if isSpace(sp)              => CharOrSpace(' ')
    }
  }
}
