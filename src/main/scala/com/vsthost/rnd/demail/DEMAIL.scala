package com.vsthost.rnd.demail

import cats.effect.IO
import com.vsthost.rnd.demail.generic.Application

import scala.language.higherKinds

/**
  * Provides the application entry point.
  */
object DEMAIL extends Application {
  /**
    * Returns the program to be run.
    *
    * @return An [[IO]] of [[Either]] a [[Throwable]] in failure case, or [[Int]] as exit code otherwise.
    */
  override def program: IO[Either[Throwable, Int]] = DEMAILPrograms.getProgram[IO](args)
}
