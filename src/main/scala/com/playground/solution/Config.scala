package com.playground.solution

import scalaz.zio.Task
import scalaz.zio.clock.Clock
import scalaz.zio.console.Console
import com.playground.strategy.Common.Env

trait Config extends Serializable {
  val config: Config.Service
}

object Config extends Serializable {
  trait Service extends Serializable {
    def get: Task[Env]
    def update(newEnv: Env): Task[Unit]
  }

  trait Live extends Serializable with Config {
    var env    = Env()
    val config = new Service {
      override def get: Task[Env]                   = Task(env)

      override def update(newEnv: Env): Task[Unit]  = Task.effectTotal { env = newEnv }
    }
  }

  object Live extends Live with Console.Live with Clock.Live
}
