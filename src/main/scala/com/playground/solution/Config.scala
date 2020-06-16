package com.playground.solution

import zio.interop.catz._
import com.playground.strategy.Common.Env
import zio.{Has, Layer, Task, ZIO, ZLayer}
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import scala.reflect.{ClassTag, classTag}


object Config {
  trait Service {
    def env: Task[Env]
    def update(newEnv: Env): Task[Unit]
  }

  type Config                                           = Has[Config.Service]

  val live: Layer[Nothing, Config]                      = ZLayer.succeed(
    new Service {
      var instance                                      = Env()
      override def env: Task[Env]                       = Task.effectTotal(instance)
      override def update(newEnv: Env): Task[Unit]      = Task.effectTotal { instance = newEnv }
    }
  )

  //accessor methods
  def env: ZIO[Config, Throwable, Env]                  = ZIO.accessM(_.get.env)
  def update(newEnv: Env): ZIO[Config, Throwable,Unit]  = ZIO.accessM(_.get.update(newEnv))
}

object Logging {
  trait Service {
    def getLogger[T: ClassTag]: Task[SelfAwareStructuredLogger[Task]]
  }

  type Logging                                                  = Has[Logging.Service]
  val cache                                                     = scala.collection.mutable.Map.empty[Class[_], Task[SelfAwareStructuredLogger[Task]]]
  val live: Layer[Nothing, Logging]                             = ZLayer.fromFunction(_ =>
    new Service {
      def getLogger[T: ClassTag]: Task[SelfAwareStructuredLogger[Task]]   = {
        val runtimeClass                              = classTag[T].runtimeClass
        cache.getOrElseUpdate(runtimeClass, Slf4jLogger.fromClass[Task](runtimeClass))
      }
    }
  )

  def logger[T: ClassTag]: ZIO[Logging, Throwable, SelfAwareStructuredLogger[Task]] = {
    (for {
      logger    <- ZIO.accessM[Logging](_.get.getLogger[T])
    } yield logger)
  }
}