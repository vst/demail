package com.vsthost.rnd.demail.programs

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.time.LocalDate
import java.util.Base64

import cats.data.EitherT
import cats.effect.Effect
import cats.implicits._
import com.vsthost.rnd.demail.generic.Program
import com.vsthost.rnd.demail.imap.{DefaultMailRepository, MailRepository}
import javax.mail.{Folder, Message}
import org.rogach.scallop.{Subcommand, ValueConverter, singleArgConverter}

import scala.language.higherKinds


/**
  * Provides a command line sub-command to download emails in a given remote folder and
  * optionally archiving these emails.
  *
  * @param command The name of the sub-command.
  * @tparam M Type parameter for the effect.
  */
class DownloadMessages[M[_] : Effect](command: String) extends Subcommand(command) with Program[M] {
  /**
    * Returns the purpose of the program.
    *
    * @return The purpose of the program.
    */
  override def purpose: String = "Download emails (and optionally archive emails)"

  // Define local date converter:
  implicit val dateConverter: ValueConverter[LocalDate] = singleArgConverter[LocalDate](LocalDate.parse)

  // Add options:
  private val host = opt[String]("host", required = true, descr = "IMAP server host", noshort = true)
  private val port = opt[Int]("port", required = true, descr = "IMAP server port", noshort = true)
  private val ussl = toggle("ssl", default = Some(false), descrYes = "Use SSL", descrNo = "No SSL", noshort = true)
  private val user = opt[String]("user", required = true, descr = "IMAP server username", noshort = true)
  private val pass = opt[String]("pass", required = true, descr = "IMAP server password", noshort = true)
  private val folder = opt[String]("folder", required = true, descr = "Folder on IMAP server", noshort = true)
  private val archive = opt[String]("archive", descr = "Archive folder on IMAP server (if to archive)", noshort = true)
  private val directory = opt[File]("directory", required = true, descr = "Local directory", noshort = true,
    validate = f => f.exists && f.isDirectory
  )
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

  def optionalOutbox(arc: Option[String]): M[Either[Throwable, Option[Folder]]] = {
    if (arc.isEmpty) {
      none[Folder].asRight[Throwable].pure[M]
    }
    else {
      repo.openFolder(arc.get, write = true).map(_.map(_.some))
    }
  }

  /**
    * Compiles and returns the program to be executed eventually.
    *
    * @return The program to be executed.
    */
  override def compile: M[Either[Throwable, Int]] = {
    for {
      // Attempt to connect to the repository:
      _ <- EitherT(repo.connect)

      // Get the inbox folder:
      folderInbox <- EitherT(repo.openFolder(folder(), write = archive.toOption.isDefined))

      // Get the inbox folder:
      folderOutbox <- EitherT(optionalOutbox(archive.toOption.filter(_.nonEmpty)))

      // Get message list:
      messages <- EitherT.right[Throwable](repo.messages(folderInbox, since.toOption, until.toOption))

      // Download attachments:
      downloaded = messages.map { message =>
        // Print the message first:
        showMessage(message)

        // Log what we are doing:
        println(fansi.Color.Yellow("    Attempting to download mail...").render)

        // Download messages:
        val path = downloadMessage(message)

        // Tell that:
        println(fansi.Color.Green(s"    Downloaded: $path").render)

        // Return:
        message
      }

      // Archive messages, if possible:
      _ <- folderOutbox match {
        case None => EitherT.pure[M, Throwable](println(fansi.Color.LightRed("Skipping archive operation...")))
        case Some(arch) => EitherT.right[Throwable](repo.move(downloaded, folderInbox, arch))
      }

      // Close the folder:
      _ <- EitherT.right[Throwable](repo.closeFolder(folderInbox))

      // Disconnect (TODO: Resource leak on shortcircuit above?):
      _ <- EitherT.right[Throwable](repo.disconnect)
    } yield 0
  }.value

  /**
    * Attempts to download the message and returns the [[Path]] to the downloaded file.
    *
    * @param message Message to download
    * @return Path to the downloaded message.
    */
  private def downloadMessage(message: Message): Path = {
    // We need a name for the target temporary file:
    val tempfileName = getTempFilename()

    // Define the target path:
    val targetPath = directory().toPath.resolve(tempfileName)

    // Create the output stream with the temporary file:
    val ostr = new FileOutputStream(targetPath.toFile)

    // Write the message to the temporary file:
    message.writeTo(ostr)

    // Close the stream:
    ostr.close()

    // Get the new file name:
    val filename = getMailFilename(targetPath, message)

    // Get the new file path:
    val newTargetPath = directory().toPath.resolve(filename)

    // Move the file:
    Files.move(targetPath, newTargetPath, StandardCopyOption.ATOMIC_MOVE)

    // Return the target path:
    newTargetPath
  }

  /**
    * Creates a target file name for the downloaded mail.
    *
    * @param path The path to the downloaded mail.
    * @param message Original message.
    * @return A unique-ish [[String]] for the target filename.
    */
  private def getMailFilename(path: Path, message: Message): String = {
    // Get the MD5 sum:
    val md5 = MessageDigest.getInstance("MD5").digest(Files.readAllBytes(path)).map("%02X".format(_)).mkString

    // Get the message ID (or use md5 instead, if it does not exist):
    val id = message.getHeader("Message-ID").headOption.map(i => Base64.getEncoder.encodeToString(i.getBytes(StandardCharsets.UTF_8))).getOrElse(md5)

    // Get the date/time message is sent:
    val datetime = formatDateTime(message.getSentDate)

    // Construct the name and return:
    s"${datetime}_${id}_$md5.eml"
  }
}
