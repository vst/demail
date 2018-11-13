package com.vsthost.rnd.demail.programs

import java.time.LocalDate

import cats.data.EitherT
import cats.effect.Effect
import com.vsthost.rnd.demail.generic.Program
import com.vsthost.rnd.demail.imap.{DefaultMailRepository, MailRepository}
import org.rogach.scallop.{Subcommand, ValueConverter, singleArgConverter}

import scala.language.higherKinds


/**
  * Provides a command line sub-command to tabulate emails in a given remote folder.
  *
  * @param command The name of the sub-command.
  * @tparam M Type parameter for the effect.
  */
class TabulateMails[M[_] : Effect](command: String) extends Subcommand(command) with Program[M] {
  /**
    * Returns the purpose of the program.
    *
    * @return The purpose of the program.
    */
  override def purpose: String = "Tabulate emails stored in the remote folder"

  // Define local date converter:
  implicit val dateConverter: ValueConverter[LocalDate] = singleArgConverter[LocalDate](LocalDate.parse)

  // Add options:
  private val host = opt[String]("host", required = true, descr = "IMAP server host", noshort = true)
  private val port = opt[Int]("port", required = true, descr = "IMAP server port", noshort = true)
  private val ussl = toggle("ssl", default = Some(false), descrYes = "Use SSL", descrNo = "No SSL", noshort = true)
  private val user = opt[String]("user", required = true, descr = "IMAP server username", noshort = true)
  private val pass = opt[String]("pass", required = true, descr = "IMAP server password", noshort = true)
  private val folder = opt[String]("folder", required = true, descr = "Folder on IMAP server", noshort = true)
  private val since = opt[LocalDate]("since", required = false, descr = "Date since (inc)", noshort = true)
  private val until = opt[LocalDate]("until", required = false, descr = "Date until (inc)", noshort = true)

  /**
    * Defines (lazily) the mail repository which we will be working on.
    */
  private lazy val repo: MailRepository[M] = new DefaultMailRepository[M](
    host = host(),
    port = port(),
    ssl = ussl(),
    user = user(),
    pass = pass(),
  )

  /**
    * Compiles and returns the program to be executed eventually.
    *
    * @return The program to be executed.
    */
  override def compile: M[Either[Throwable, Int]] = {
    for {
      // Attempt to connect to the repository:
      _ <- EitherT(repo.connect)

      // Get the folder:
      folder <- EitherT(repo.openFolder(folder(), write = false))

      // Get message list:
      messages <- EitherT.right[Throwable](repo.messages(folder, since.toOption, until.toOption))

      // Print each message on the console:
      _ = messages.foreach(showMessage)

      // Close the folder:
      _ <- EitherT.right[Throwable](repo.closeFolder(folder))

      // Disconnect (TODO: Resource leak on shortcircuit above?):
      _ <- EitherT.right[Throwable](repo.disconnect)
    } yield 0
  }.value
}
