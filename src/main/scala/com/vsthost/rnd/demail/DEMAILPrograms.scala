package com.vsthost.rnd.demail

import cats.effect.Effect
import cats.implicits._
import com.vsthost.rnd.demail.generic.Program
import com.vsthost.rnd.demail.programs.{DownloadAttachments, DownloadMessages, ListFolders, TabulateMails}
import org.rogach.scallop.{ScallopConf, ScallopConfBase}

import scala.language.higherKinds

/**
  * Provides the command line arguments and options parser.
  *
  * @param arguments List of strings, possibly arguments consumed directly from the command line.
  */
class DEMAILPrograms[M[_] : Effect](arguments: Seq[String]) extends ScallopConf(arguments) {
  // Define the version:
  version(s"${BuildInfo.name} / v${BuildInfo.version} / Copyright (c) 2018 Vehbi Sinan Tunalioglu <vst@vsthost.com>")

  // Define the banner:
  banner(s"\n${BuildInfo.name} command line interface\n")

  // Define the footer for additional notes for questions and feedback:
  footer("\nPlease contact the author in case you have any questions of feedback.")

  // Add subcommands:
  addSubcommand(new ListFolders[M]("list-folders"))
  addSubcommand(new TabulateMails[M]("list-messages"))
  addSubcommand(new DownloadAttachments[M]("download-attachments"))
  addSubcommand(new DownloadMessages[M]("download-messages"))

  // Done, let's verify:
  verify()
}

/**
  * Provides the companion object to [[DEMAILPrograms]] for convenince.
  */
object DEMAILPrograms {
  /**
    * Returns the program instructions to be executed.
    *
    * @param args Command-line arguments.
    * @tparam M Parameter for the [[Effect]] type.
    * @return Program instructions.
    */
  def getProgram[M[_] : Effect](args: Seq[String]): M[Either[Throwable, Int]] = {
    // Parse the configuration:
    val conf = new DEMAILPrograms(args)

    // Build the program and return (or return the error message if something is wrong):
    conf.subcommand match {
      case None => new Throwable("No command provided. Consider running with --help flag.").asLeft[Int].pure[M]
      case Some(cmd) => cmd match {
        case x: ScallopConfBase with Program[M @unchecked] => x.compile
        case _ => new Throwable("Not implemented. What a pitty!").asLeft[Int].pure[M]
      }
    }
  }
}
