package com.playground.strategy

import Default._
import cats.Order
import cats.collections._
import cats.implicits._
import scala.annotation.tailrec
import eu.timepit.refined.api.Refined
import scala.collection.immutable.{Map => SMap}

object Common {

  case class InProcessResult(nextPos    : Point,
                             distinct   : AvlSet[Point]   = AvlSet.empty[Point],
                             targetPos  : Option[Point]   = None,
                             traceBack  : Dequeue[Point]  = Dequeue.empty[Point],
                             movement   : String          = "")

  implicit val pointOrder = new Order[Point] {
    override def compare(x: Point, y: Point): Int = {
      (x == y) match {
        case true   => 0
        case _      =>
          val result = x.x compare y.x
          result match {
            case 0  => x.y compare y.y
            case _  => result
          }
      }
    }
  }

  case class Point(x: Int, y: Int) {
    def isLeftOf(that: Point)     : Boolean  = this.x < that.x
    def isRightOf(that: Point)    : Boolean  = this.x > that.x
    def isHigherThan(that: Point) : Boolean  = this.y < that.y
    def isLowerThan(that: Point)  : Boolean  = this.y > that.y
  }

  case class Env(currentPos   : Point           = Point(0,0),
                 visited      : AvlSet[Point]   = AvlSet.empty[Point],
                 unexplored   : AvlSet[Point]   = AvlSet.empty[Point],
                 traceBack    : Dequeue[Point]  = Dequeue.empty[Point],
                 result       : Dequeue[Char]   = Dequeue.empty[Char],
                 targetPos    : Option[Point]   = None,
                 hasStarted   : Boolean         = false,
                 movement     : String          = "") {
    val x                     = currentPos.x
    val y                     = currentPos.y
    val top                   = Point(x, y - 1)
    val bottom                = Point(x, y + 1)
    val left                  = Point(x - 1, y)
    val right                 = Point(x + 1, y)


    type Path                 = List[Point]

    @tailrec
    private def dijkstra(lookup  : SMap[Point, Path],
                         fringe  : List[Path],
                         dest    : Point,
                         visited : AvlSet[Point]): Path = fringe match {
      case path :: fringeRest   =>
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

    def getPathFromCurrentToTarget: Dequeue[Point]  = {
      val targets                                   = getTargetNeighbors
      val lookup                                    = getLookUpMap
      Dequeue.fromFoldable(
        dijkstra(lookup, List[Path](List[Point](currentPos)), targets.min.getOrElse(Point(-1,-1)), AvlSet.empty[Point])
      )
    }

    def isNeighbor(point: Point): Boolean = point == top || point == left  || point == right || point == bottom
    def getNeighbors: AvlSet[Point]       = visited intersect AvlSet(top, bottom, left, right)
    def getTargetNeighbors: AvlSet[Point] = {
      targetPos match {
        case Some(target) => visited intersect AvlSet(Point(target.x - 1, target.y),
          Point(target.x + 1, target.y),
          Point(target.x, target.y - 1),
          Point(target.x, target.y + 1)
        )
        case _            =>  AvlSet.empty[Point]
      }
    }
    def getNeighborsFor(current :Point): AvlSet[Point] = {
      (visited intersect AvlSet(Point(current.x - 1, current.y),
        Point(current.x + 1, current.y),
        Point(current.x, current.y - 1),
        Point(current.x, current.y + 1))
      )
    }
    def surroundedByNewCells: Boolean     = hasNewTop    && hasNewBottom  && hasNewLeft   && hasNewRight
    def surroundedByOldCells: Boolean     = !surroundedByNewCells
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

    def getLookUpMap: SMap[Point, Path]   = {
      visited.foldLeft(SMap.empty[Point, List[Point]])((acc, p) => {
        acc ++ SMap[Point, Path](p -> getNeighborsFor(p).toList)
      })
    }
  }

  val initial                             = Env()

  def isValidInput(c: Char): Boolean      = isLineSeparator(c.toString) || isCapitalLetter(c) || isSpace(c) || isWall(c)

  def isWall(c: Char): Boolean            = c == '#'

  def isCapitalLetter(c: Char): Boolean   = c.isLetter && c.isUpper

  def isSpace(c: Char): Boolean           = c == ' '

  val lineSeparator: String               = System.getProperty("line.separator")

  def isLineSeparator(c: String): Boolean = c.equals(lineSeparator)

  trait       Cell
  case object Wall                            extends Cell
  case class  CharOrSpace(character: Char)    extends Cell

  def generateCell(c: Char Refined CapitalSpacePound): Cell       = {
    c.value match {
      case '#'                            => Wall
      case c  if isCapitalLetter(c)       => CharOrSpace(c)
      case sp if isSpace(sp)              => CharOrSpace(' ')
    }
  }
}
