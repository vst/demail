package com.vsthost.rnd.demail.programs

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.time.LocalDate
import java.util.{Base64, Date, TimeZone, UUID}

import javax.mail.internet.MimeBodyPart
import javax.mail.{Folder, Message, Multipart, Part}
import cats.data.EitherT
import cats.implicits._
import cats.effect.Effect
import com.vsthost.rnd.demail.generic.Program
import com.vsthost.rnd.demail.imap.{DefaultMailRepository, MailRepository}
import org.rogach.scallop.{Subcommand, ValueConverter, singleArgConverter}

import scala.language.higherKinds


/**
  * Provides a command line sub-command to download email attachments in a given remote folder and
  * optionally archiving these emails.
  *
  * @param command The name of the sub-command.
  * @tparam M Type parameter for the effect.
  */
class DownloadAttachments[M[_] : Effect](command: String) extends Subcommand(command) with Program[M] {
  /**
    * Returns the purpose of the program.
    *
    * @return The purpose of the program.
    */
  override def purpose: String = "Download attachments (and optionally archive emails)"

  // Define local date converter:
  implicit val dateConverter: ValueConverter[LocalDate] = singleArgConverter[LocalDate](LocalDate.parse)

  // Add options:
  private val host = opt[String]("host", required = true, descr = "IMAP server host", noshort = true)
  private val port = opt[Int]("port", required = true, descr = "IMAP server port", noshort = true)
  private val ussl = toggle("ssl", default = Some(false), descrYes = "Use SSL", descrNo = "No SSL", noshort = true)
  private val mail = toggle("mail", default = Some(false), descrYes = "Download the mail itself, too", descrNo = "Don't download the mail itself", noshort = true)
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
        printMessage(message)

        // Are we downloading the mail, too?
        if (mail()) {
          // Log what we are doing:
          println(fansi.Color.Yellow("    Attempting to download mail...").render)

          // Download messages:
          val path = downloadMessage(message)

          // Tell that:
          println(fansi.Color.Green(s"    Downloaded: $path").render)
        }

        // Log what we are doing:
        println(fansi.Color.Yellow("    Attempting to download attachments...").render)

        // Download messages:
        val paths = downloadAttachments(message)

        // Log it:
        paths match {
          case Nil => {
            println(fansi.Color.Red("    No attachments found to be downloaded...").render)
            None
          }
          case _ => {
            paths.foreach(p => println(fansi.Color.Green(s"    Downloaded: $p").render))
            Some(message)
          }
        }
      }

      // Archive messages, if possible:
      _ <- folderOutbox match {
        case None => EitherT.pure[M, Throwable](println(fansi.Color.LightRed("Skipping archive operation...")))
        case Some(arch) => EitherT.right[Throwable](repo.move(downloaded.flatten, folderInbox, arch))
      }

      // Close the folder:
      _ <- EitherT.right[Throwable](repo.closeFolder(folderInbox))

      // Disconnect (TODO: Resource leak on shortcircuit above?):
      _ <- EitherT.right[Throwable](repo.disconnect)
    } yield 0
  }.value

  /**
    * Provides a utility function to print the message.
    *
    * @param message The message to print to the console.
    * @return [[Message]] printed on the console.
    */
  private def printMessage(message: Message): Message = {
    println(fansi.Color.Cyan(s"Subject : ${message.getSubject}").render)
    println(fansi.Color.Cyan(s"From    : ${message.getFrom.map(_.toString).mkString(", ")}").render)
    println(fansi.Color.Cyan(s"Sent    : ${message.getSentDate}").render)
    message
  }

  /**
    * Attempts to download all attachments of the message and returns a list of [[Path]]s to the downloaded files.
    *
    * @param message Message which to download attachments of.
    * @return A [[List]] of [[Path]]s to the downloaded files.
    */
  private def downloadAttachments(message: Message): List[Path] = {
    getAttachments(message).map(downloadPart(_, message))
  }

  /**
    * Downloads the given part from the given message and returns the path of the download.
    *
    * @param part     The part to download.
    * @param message  Original message the part belongs to.
    * @return The path to the downloaded attachment.
    */
  private def downloadPart(part: MimeBodyPart, message: Message): Path = {
    // We need a name for the target temporary file:
    val tempfileName = getTempFilename

    // Define the target path:
    val targetPath = directory().toPath.resolve(tempfileName)

    // Download the part:
    part.saveFile(targetPath.toFile)

    // Get the new file name:
    val filename = getFilename(targetPath, part, message)

    // Get the new file path:
    val newTargetPath = directory().toPath.resolve(filename)

    // Move the file:
    Files.move(targetPath, newTargetPath, StandardCopyOption.ATOMIC_MOVE)

    // Return the target path:
    newTargetPath
  }

  /**
    * Attempts to download the message and returns the [[Path]] to the downloaded file.
    *
    * @param message Message to download
    * @return Path to the downloaded message.
    */
  private def downloadMessage(message: Message): Path = {
    // We need a name for the target temporary file:
    val tempfileName = getTempFilename

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
    * Returns a unique, hidden file name for the temporary file to be used during downloading.
    *
    * @return A unique, hidden file name.
    */
  private def getTempFilename: String = s"._${UUID.randomUUID.toString}.download"

  /**
    * Creates a target file name for the downloaded attachment.
    *
    * @param path The path to the downloaded attachment.
    * @param part [[MimeBodyPart]] representing the attachment.
    * @param message Original message containing the attachment.
    * @return A unique-ish [[String]] for the target attachment filename.
    */
  private def getFilename(path: Path, part: MimeBodyPart, message: Message): String = {
    // Get the MD5 sum:
    val md5 = MessageDigest.getInstance("MD5").digest(Files.readAllBytes(path)).map("%02X".format(_)).mkString

    // Get the message ID:
    val id = message.getHeader("Message-ID").headOption.map(i => Base64.getEncoder.encodeToString(i.getBytes(StandardCharsets.UTF_8))).getOrElse(md5)

    // Get the date/time message is sent:
    val datetime = formatDate(message.getSentDate)

    // Construct the name and return:
    s"${datetime}_${id}_${md5}_${part.getFileName}"
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
    val datetime = formatDate(message.getSentDate)

    // Construct the name and return:
    s"${datetime}_${id}_$md5.eml"
  }

  /**
    * Formats the given date/time in ISO format.
    *
    * @param datetime Date/time to format.
    * @return An ISO representation of the given datetime.
    */
  private def formatDate(datetime: Date): String = {
    // We will use UTC timezone:
    val timezone = TimeZone.getTimeZone("UTC")

    // Define the formatter:
    val formatter = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss'Z'")

    // Set the timezone of the formatter:
    formatter.setTimeZone(timezone)

    // Format the input and return:
    formatter.format(datetime)
  }

  /**
    * Returns all the attachments of the message as [[MimeBodyPart]], if any.
    *
    * @param message The message of which to find attachments to find and return.
    * @return A [[List]] of [[MimeBodyPart]], if any.
    */
  private def getAttachments(message: Message): List[MimeBodyPart] = {
    // Get the content type:
    Some(message)
      // We are only interested in "multipart" messages:
      .filter(_.getContentType.contains("multipart"))
      // Get the content as [[Multipart]]
      .map(_.getContent.asInstanceOf[Multipart])
      // Extract parts of interest:
      .map { parts =>
        (0 until parts.getCount)
          .map(i => parts.getBodyPart(i).asInstanceOf[MimeBodyPart])
          .filter(part => Part.ATTACHMENT.equalsIgnoreCase(part.getDisposition))
      }
      .getOrElse(Seq.empty).toList
  }
}
