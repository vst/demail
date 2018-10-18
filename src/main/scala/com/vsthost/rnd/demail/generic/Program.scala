package com.vsthost.rnd.demail.generic

import org.rogach.scallop.ScallopConf

import scala.language.higherKinds

/**
  * Provides a trait for standardising the preparation of programs.
  *
  * This is quite useful if we have relatively tedious steps to prepare program instances, such as interpreting
  * command line arguments and options, picking the right program instance for a given algebra, etc...
  *
  * @tparam M Type parameter for the effect.
  */
trait Program[M[_]] { this: ScallopConf =>
  /**
    * Returns the purpose of the program.
    *
    * @return The purpose of the program.
    */
  def purpose: String

  /**
    * Compiles and returns the program to be executed eventually.
    *
    * @return The program to be executed.
    */
  def compile: M[Either[Throwable, Int]]

  // Set the banner:
  banner(purpose)
}
