package com.vsthost.rnd.demail.generic

import cats.effect.IO

/**
  * Provides an abstract class extending the [[App]] to fascilitate convenience when writing `tagree` applications.
  */
abstract class Application extends App {
  /**
    * Returns the program to be run.
    *
    * @return An [[cats.effect.Effect]] of [[Either]] a [[Throwable]] in failure, or [[Int]] as exit code otherwise.
    */
  def program: IO[Either[Throwable, Int]]

  /**
    * Runs the program and exits the application.
    */
  def run(): Unit = {
    // Run the program, handle the return value and exit accordingly:
    program.attempt.unsafeRunSync match {
      case Left(error) => exitOnError(new Throwable("Unexpected error occured. Reach out the programmer!", error), 2)
      case Right(rets) => rets match {
        case Left(error) => exitOnError(error, 1)
        case Right(code) => exitWithCode(code)
      }
    }
  }

  /**
    * Provides a function to exit the application with the given code.
    *
    * @param code Application exit code.
    */
  protected def exitWithCode(code: Int): Unit = System.exit(code)

  /**
    * Provides a function to print the error message and exit the application with code `1`.
    *
    * @param error Error message caught.
    */
  protected def exitOnError(error: Throwable, code: Int): Unit = {
    // Print the stack trace:
    error.printStackTrace()

    // Print the error message:
    Console.err.println(s"${error.getMessage}\nExiting...")

    // Exit the application:
    exitWithCode(code)
  }

  // Call run:
  run()
}
