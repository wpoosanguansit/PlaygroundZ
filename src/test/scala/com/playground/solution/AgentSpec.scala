package com.playground.solution

import org.specs2._
import cats.implicits._
import eu.timepit.refined._
import scalaz.zio.DefaultRuntime
import com.playground.strategy.Common._
import com.playground.strategy.Default._
import cats.collections.{AvlSet, Dequeue}
import com.playground.strategy.Default.processInputsZ


/**
  *  Sample session
    R 2
    Env(Point(0,0),Set(),Set(),Set(),List(),None,false)
    Env(Point(1,0),Set(Point(0,0)),Set(Point(0,1), Point(-1,0), Point(1,0)),Set(),List(B),Some(Point(1,0)),true)
    R 5
    Env(Point(1,0),Set(Point(0,0)),Set(Point(0,1), Point(-1,0), Point(1,0)),Set(),List(B),Some(Point(1,0)),true)
    Env(Point(2,0),Set(Point(0,0), Point(1,0)),Set(Point(2,0), Point(1,1), Point(-1,0), Point(0,1), Point(1,0)),Set(),List(B, C),Some(Point(2,0)),true)
    D 14
    Env(Point(2,0),Set(Point(0,0), Point(1,0)),Set(Point(2,0), Point(1,1), Point(-1,0), Point(0,1), Point(1,0)),Set(),List(B, C),Some(Point(2,0)),true)
    Env(Point(2,1),Set(Point(0,0), Point(1,0), Point(2,0)),Set(Point(2,0), Point(1,1), Point(-1,0), Point(0,1), Point(2,1)),Set(),List(B, C, D),Some(Point(2,1)),true)
    L 11
    Env(Point(2,1),Set(Point(0,0), Point(1,0), Point(2,0)),Set(Point(2,0), Point(1,1), Point(-1,0), Point(0,1), Point(2,1)),Set(),List(B, C, D),Some(Point(2,1)),true)
    Env(Point(1,1),Set(Point(0,0), Point(1,0), Point(2,0), Point(2,1)),Set(Point(1,1), Point(-1,0), Point(0,1), Point(2,1)),Set(),List(B, C, D, E),Some(Point(1,1)),true)
    L 11
    Env(Point(1,1),Set(Point(0,0), Point(1,0), Point(2,0), Point(2,1)),Set(Point(1,1), Point(-1,0), Point(0,1), Point(2,1)),Set(),List(B, C, D, E),Some(Point(1,1)),true)
    Env(Point(0,1),Set(Point(0,0), Point(2,0), Point(1,1), Point(2,1), Point(1,0)),Set(Point(1,1), Point(-1,0), Point(0,1)),Set(),List(B, C, D, E, F),Some(Point(0,1)),true)
    U 21
    Env(Point(0,1),Set(Point(0,0), Point(2,0), Point(1,1), Point(2,1), Point(1,0)),Set(Point(1,1), Point(-1,0), Point(0,1)),Set(),List(B, C, D, E, F),Some(Point(0,1)),true)
    Env(Point(0,0),Set(Point(0,0), Point(2,0), Point(1,1), Point(0,1), Point(2,1), Point(1,0)),Set(Point(1,1), Point(-1,0)),Set(Point(0,1)),List(B, C, D, E, F, G),Some(Point(0,1)),true)
    L 11
    Env(Point(0,0),Set(Point(0,0), Point(2,0), Point(1,1), Point(0,1), Point(2,1), Point(1,0)),Set(Point(1,1), Point(-1,0)),Set(Point(0,1)),List(B, C, D, E, F, G),Some(Point(0,1)),true)
    Env(Point(-1,0),Set(Point(0,0), Point(2,0), Point(1,1), Point(0,1), Point(2,1), Point(1,0)),Set(Point(-1,0)),Set(Point(0,1)),List(B, C, D, E, F, G),Some(Point(-1,0)),true)
    R 19
    Env(Point(-1,0),Set(Point(0,0), Point(2,0), Point(1,1), Point(0,1), Point(2,1), Point(1,0)),Set(Point(-1,0)),Set(Point(0,1)),List(B, C, D, E, F, G),Some(Point(-1,0)),true)
    Env(Point(0,0),Set(Point(0,0), Point(2,0), Point(1,1), Point(-1,0), Point(0,1), Point(2,1), Point(1,0)),Set(),Set(Point(0,1), Point(-1,0)),List(B, C, D, E, F, G, A),Some(Point(-1,0)),true)

                      The result is KABCDEFG

    The agent seems to have completed the grid. Please start again with a new session.

  case class Env(currentPos   : Point = Point(0,0),
                 visited      : Set[Point]  = Set.empty[Point],
                 unexplored   : Set[Point]  = Set.empty[Point],
                 traceBack    : List[Point] = List.empty[Point],
                 result       : List[Char]  = List.empty[Char],
                 targetPos    : Option[Point] = None,
                 hasStarted   : Boolean   = false)

  */

class AgentSpec extends Specification {
  def is = s2"""
                shouldHaveFoundH                                              $shouldHaveFoundH
                shouldHaveEnded                                               $shouldHaveEnded
                shouldHaveCorrectLookupTable                                  $shouldHaveCorrectDijkstra
           """
  ///////////////////////////////////////////////////////////////////
  // generateNextMove takes current, top, bottom, left, right.
  ///////////////////////////////////////////////////////////////////
  val initial = Env(
    Point(0,1),
    AvlSet(Point(0,0), Point(2,0), Point(1,1), Point(2,1), Point(1,0)),
    AvlSet(Point(1,1), Point(-1,0), Point(0,1)),
    Dequeue.empty[Point],
    Dequeue('B', 'C', 'D', 'E', 'F'),
    Some(Point(1,2)),
    true
  )

  val runtime = new DefaultRuntime {}

  def shouldHaveFoundH = {
    val stateE     = (refineV[CapitalSpacePound]('G'), refineV[CapitalSpacePound]('B'), refineV[CapitalSpacePound]('#'),
      refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('F')).mapN(
      (first, second, third, fourth, fifth) => {
      processInputsZ(first, second, third, fourth, fifth)(initial)
    })
    println(initial)
    val result     = stateE.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result)
    val stateE1    = (refineV[CapitalSpacePound]('B'), refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('G'),
      refineV[CapitalSpacePound]('A'), refineV[CapitalSpacePound]('C')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(result)
      })
    val result1    = stateE1.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result1)
    val stateE2    = (refineV[CapitalSpacePound]('A'), refineV[CapitalSpacePound]('H'), refineV[CapitalSpacePound]('#'),
      refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('B')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(result1)
      })
    val result2    = stateE2.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result2)
    val stateE3    = (refineV[CapitalSpacePound]('H'), refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('A'),
      refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('#')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(result2)
      })
    val result3    = stateE3.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result3)
    result3.result.toList.sorted must_== List('B', 'C', 'D', 'E', 'F', 'G', 'A', 'H').sorted
  }

  def shouldHaveEnded = {
    val stateE     = (refineV[CapitalSpacePound]('G'), refineV[CapitalSpacePound]('B'), refineV[CapitalSpacePound]('#'),
      refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('F')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(initial)
      })
    println(initial)
    val result     = stateE.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result)
    val stateE1    = (refineV[CapitalSpacePound]('B'), refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('G'),
      refineV[CapitalSpacePound]('A'), refineV[CapitalSpacePound]('C')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(result)
      })
    val result1    = stateE1.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result1)
    val stateE2    = (refineV[CapitalSpacePound]('A'), refineV[CapitalSpacePound]('H'), refineV[CapitalSpacePound]('#'),
      refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('B')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(result1)
      })
    val result2    = stateE2.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result2)
    val stateE3    = (refineV[CapitalSpacePound]('H'), refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('A'),
      refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('#')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(result2)
      })
    val result3    = stateE3.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result3)
    val stateE4    = (refineV[CapitalSpacePound]('A'), refineV[CapitalSpacePound]('H'), refineV[CapitalSpacePound]('#'),
      refineV[CapitalSpacePound]('#'), refineV[CapitalSpacePound]('B')).mapN(
      (first, second, third, fourth, fifth) => {
        processInputsZ(first, second, third, fourth, fifth)(result3)
      })
    val result4    = stateE4.fold(error => throw new RuntimeException(error), state => runtime.unsafeRun(state))
    println(result4)
    result4.unexplored must_== AvlSet.empty[Point]
  }

  def shouldHaveCorrectDijkstra = {
    val initial = Env(
      Point(-14,12),
      AvlSet(Point(-18,12), Point(-15,12), Point(-13,15), Point(-4,18), Point(-3,2), Point(-3,4), Point(-12,7), Point(-23,17), Point(-20,8), Point(-6,4), Point(-14,12), Point(-23,8), Point(-23,20), Point(-6,18), Point(-5,19), Point(-8,12), Point(-4,13), Point(-18,17), Point(-4,8), Point(-17,2), Point(-18,3), Point(-12,8), Point(-4,20), Point(-5,13), Point(-7,4), Point(-23,6), Point(-4,15), Point(-1,6), Point(-20,13), Point(-5,12), Point(-2,12), Point(-5,11), Point(-19,20), Point(-6,12), Point(-2,0), Point(-2,16), Point(-6,22), Point(0,17), Point(-7,19), Point(-1,8), Point(-13,6), Point(-7,15), Point(-5,16), Point(-17,11), Point(-20,15), Point(-1,22), Point(-3,5), Point(-10,4), Point(-3,0), Point(0,2), Point(0,0), Point(-18,23), Point(-3,10), Point(-16,15), Point(-23,5), Point(-9,7), Point(-7,22), Point(-19,15), Point(-20,16), Point(-8,16), Point(-6,3), Point(-3,3), Point(-22,7), Point(-9,16), Point(-9,6), Point(-23,22), Point(-7,17), Point(0,19), Point(-8,23), Point(-9,23), Point(-8,2), Point(-2,8), Point(-13,22), Point(-13,7), Point(-20,18), Point(-11,4), Point(-5,5), Point(-4,12), Point(-3,12), Point(-3,8), Point(0,22), Point(-12,2), Point(-3,1), Point(-21,15), Point(-2,4), Point(-3,22), Point(-9,17), Point(-17,20), Point(-15,16), Point(-8,0), Point(-10,22), Point(0,10), Point(-18,22), Point(-17,12), Point(-11,22), Point(-6,0), Point(-8,11), Point(-16,22), Point(-20,22), Point(-22,23), Point(-3,16), Point(-16,18), Point(0,20), Point(-11,18), Point(-14,10), Point(-11,2), Point(-4,4), Point(-6,23), Point(-6,11), Point(0,18), Point(-6,15), Point(-20,6), Point(-9,19), Point(-18,2), Point(-16,2), Point(-10,15), Point(-8,19), Point(-2,20), Point(-11,8), Point(-16,8), Point(-23,12), Point(-10,20), Point(-3,20), Point(-21,5), Point(-2,3), Point(-5,6), Point(-3,13), Point(-2,13), Point(-11,11), Point(-17,22), Point(-9,11), Point(-5,10), Point(-10,23), Point(-2,1), Point(-9,21), Point(-15,6), Point(-10,17), Point(-17,15), Point(0,7), Point(0,3), Point(-19,5), Point(-13,12), Point(-5,20), Point(-17,5), Point(-10,21), Point(-17,16), Point(-5,0), Point(-7,10), Point(-9,2), Point(-17,17), Point(-22,18), Point(-6,20), Point(-4,10), Point(-18,20), Point(-14,7), Point(-4,21), Point(-7,11), Point(-17,6), Point(-7,23), Point(-23,16), Point(-3,15), Point(-1,11), Point(-13,10), Point(-5,4), Point(-11,7), Point(0,5), Point(-5,1), Point(-9,18), Point(-18,10), Point(-23,7), Point(-7,2), Point(-22,8), Point(-8,9), Point(-13,17), Point(-4,2), Point(-5,15), Point(-7,12), Point(-6,19), Point(-15,22), Point(-5,18), Point(-16,7), Point(0,15), Point(-10,10), Point(-13,20), Point(-5,17), Point(-10,12), Point(-2,22), Point(-8,21), Point(-17,7), Point(-5,23), Point(-8,22), Point(-2,18), Point(-16,14), Point(-6,6), Point(-1,23), Point(-18,7), Point(-7,8), Point(-4,22), Point(-6,2), Point(-6,16), Point(-10,19), Point(-9,12), Point(-20,10), Point(0,8), Point(-2,7), Point(-6,17), Point(-15,21), Point(-4,11), Point(-8,6), Point(-14,17), Point(-23,23), Point(-10,0), Point(-11,3), Point(-18,13), Point(-1,7), Point(-7,1), Point(-7,0), Point(-9,3), Point(-23,18), Point(-1,17), Point(-5,2), Point(-20,12), Point(0,4), Point(-15,11), Point(-8,7), Point(0,23), Point(-8,17), Point(-5,21), Point(-6,7), Point(-1,18), Point(-16,12), Point(-1,0), Point(-15,10), Point(-10,5), Point(-2,15), Point(-9,10), Point(-1,16), Point(-6,8), Point(-23,10), Point(-4,6), Point(-15,15), Point(-1,4), Point(-17,21), Point(-10,1), Point(-8,15), Point(-11,16), Point(-11,21), Point(-8,8), Point(-2,23), Point(-10,3), Point(-4,19), Point(-4,1), Point(-8,5), Point(0,12), Point(0,16), Point(-2,17), Point(-7,3), Point(-22,17), Point(0,13), Point(-2,21), Point(-6,21), Point(-10,16), Point(-5,8), Point(-5,3), Point(-15,5), Point(-4,17), Point(-20,21), Point(-1,20), Point(-9,22), Point(-9,8), Point(-13,5), Point(-19,10), Point(-11,17), Point(-3,19), Point(-13,16), Point(-21,10), Point(-9,0), Point(-22,13), Point(-1,15), Point(0,11), Point(-7,6), Point(-4,23), Point(-3,7), Point(-10,6), Point(-11,10), Point(-16,17), Point(-6,5), Point(-1,2), Point(-1,3), Point(-3,23), Point(-11,13), Point(0,1), Point(-11,0), Point(-15,17), Point(-10,11), Point(0,21), Point(-11,6), Point(-2,2), Point(-7,21), Point(-3,11), Point(-1,1), Point(-8,10), Point(-2,6), Point(-18,19), Point(-3,21), Point(-14,20), Point(-7,13), Point(-9,13), Point(-8,20), Point(-8,18), Point(-12,17), Point(-4,7), Point(-23,11), Point(-16,13), Point(-12,12), Point(-1,12), Point(-11,5), Point(-6,13), Point(-3,6), Point(-13,21), Point(-18,5), Point(-20,23), Point(-12,23), Point(-15,7), Point(-3,18), Point(-13,11), Point(-3,17), Point(-9,5), Point(-11,23), Point(-12,10), Point(-9,4), Point(-9,20), Point(-16,19), Point(-20,5), Point(-6,10), Point(-2,19), Point(-18,18), Point(-23,15), Point(-5,7), Point(-23,21), Point(-22,22), Point(-9,1), Point(-22,12), Point(-20,20), Point(-7,18), Point(-8,4), Point(-10,14), Point(-2,11), Point(-22,10), Point(-14,15), Point(-20,11), Point(-1,13), Point(-7,20), Point(-1,21), Point(-10,18), Point(-1,5), Point(-15,20), Point(-2,5), Point(-14,22), Point(-8,13), Point(-11,19), Point(-10,8), Point(-4,0), Point(-8,1), Point(-20,7), Point(-9,15), Point(-2,10), Point(-22,20), Point(-11,1), Point(-15,2), Point(-10,13), Point(-18,8), Point(-10,2), Point(-4,5), Point(-14,5), Point(-7,16), Point(-5,22), Point(-14,2), Point(-22,15), Point(-11,20), Point(-4,3), Point(-1,19), Point(-16,23), Point(-13,2), Point(-18,4), Point(-10,7), Point(-21,20), Point(-4,16), Point(-7,5), Point(0,6), Point(-18,15), Point(-23,13), Point(-11,15), Point(-12,22), Point(-1,10), Point(-17,10), Point(-6,1), Point(-20,17), Point(-11,12), Point(-22,5), Point(-7,7), Point(-8,3)),
      AvlSet(Point(-17,1), Point(-15,1), Point(-16,3), Point(-13,1)),
      Dequeue.empty[Point],
      Dequeue('B', 'C', 'D', 'E', 'F'),
      Some(Point(-21,10)),
      true
    )

    val result = initial.getPathFromCurrentToTarget

    println(result)

    result must_!= List.empty[Point]
  }
}