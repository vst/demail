package com.vsthost.rnd.demail

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.{Date, TimeZone, UUID}

import javax.mail.Message

/**
  * Provides common definitions and auxiliary functions.
  */
package object programs {
  /**
    * defines the UTC timezone.
    */
  val tzUTC: TimeZone = TimeZone.getTimeZone("UTC")

  /**
    * Defines ISO compatible date/time format for UTC.
    */
  val isoDateTimeFormat: SimpleDateFormat = {
    val fmt = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    fmt.setTimeZone(tzUTC)
    fmt
  }

  /**
    * Formats the given [[Date]] into ISO formatted UTC datetime.
    */
  val formatDateTime: Date => String = (datetime: Date) => isoDateTimeFormat.format(datetime)

  /**
    * Returns a unique file name for temporary usage.
    */
  val getTempFilename: () => String = () => s"._${UUID.randomUUID.toString}.download"

  /**
    * Displays the given message on the console.
    */
  val showMessage: Message => Message = (msg: Message) => {
    println(fansi.Color.Cyan(s"Subject    : ${msg.getSubject}").render)
    println(fansi.Color.Cyan(s"From       : ${msg.getFrom.map(_.toString).mkString(", ")}").render)
    println(fansi.Color.Cyan(s"Recipients : ${msg.getAllRecipients.map(_.toString).mkString(", ")}").render)
    println(fansi.Color.Cyan(s"Sent       : ${msg.getSentDate}").render)
    println(fansi.Color.Cyan(s"Received   : ${msg.getReceivedDate}").render)
    println(fansi.Color.Cyan(f"Size       : ${BigDecimal(msg.getSize / (1024.0 * 1024)).setScale(2, BigDecimal.RoundingMode.HALF_UP)} MB").render)
    println("")
    msg
  }

  /**
    * Returns the md5 sum of the given string.
    */
  val md5FromString: String => String = (text: String) => {
    MessageDigest.getInstance("MD5").digest(text.getBytes()).map("%02X".format(_)).mkString
  }

  /**
    * Returns the md5 sum of the file at the given path.
    */
  val md5FromPath: Path => String = (path: Path) => {
    MessageDigest.getInstance("MD5").digest(Files.readAllBytes(path)).map("%02X".format(_)).mkString
  }
}
