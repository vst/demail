package com.vsthost.rnd.demail.programs

import javax.mail.{Folder, Message}

import cats.data.EitherT
import cats.effect.Effect
import com.vsthost.rnd.demail.generic.Program
import com.vsthost.rnd.demail.imap.{DefaultMailRepository, MailRepository}
import org.rogach.scallop.Subcommand

import scala.language.higherKinds


/**
  * Provides a command line sub-command to list remote folders.
  *
  * @param command The name of the sub-command.
  * @tparam M Type parameter for the effect.
  */
class ListFolders[M[_] : Effect](command: String) extends Subcommand(command) with Program[M] {
  /**
    * Returns the purpose of the program.
    *
    * @return The purpose of the program.
    */
  override def purpose: String = "List remote folders"

  // Add options:
  private val host = opt[String]("host", required = true, descr = "IMAP server host", noshort = true)
  private val port = opt[Int]("port", required = true, descr = "IMAP server port", noshort = true)
  private val ussl = toggle("ssl", default = Some(false), descrYes = "Use SSL", descrNo = "No SSL", noshort = true)
  private val user = opt[String]("user", required = true, descr = "IMAP server username", noshort = true)
  private val pass = opt[String]("pass", required = true, descr = "IMAP server password", noshort = true)

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
      folders <- EitherT.right(repo.listFolders)

      // Print folders:
      _ = folders.foreach(printFolder)

      // Disconnect (TODO: Resource leak on shortcircuit above?):
      _ <- EitherT.right[Throwable](repo.disconnect)
    } yield 0
  }.value

  /**
    * Provides a utility function to print the folder.
    *
    * @param folder The folder to print to the console.
    * @return [[Folder]] printed on the console.
    */
  private def printFolder(folder: Folder): Folder = {
    println(fansi.Color.Blue(s"${folder.getFullName} (${folder.getURLName})").render)
    folder
  }
}
