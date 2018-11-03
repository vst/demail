package com.vsthost.rnd.demail.imap

import java.time.LocalDate

import javax.mail.{Folder, Message, Store}

import scala.language.higherKinds

/**
  * Provides a mail repository algebra.
  *
  * @tparam M Effect type parameter.
  */
trait MailRepository[M[_]] {
  /**
    * Indicates if the repository is connected.
    *
    * @return `true` if connected, `false` otherwise.
    */
  def isConnected: M[Boolean]

  /**
    * Connects to the remote [[Store]].
    *
    * @return [[Either]] a [[Throwable]] in case of failure, [[Unit]] otherwise.
    */
  def connect: M[Either[Throwable, Unit]]

  /**
    * Disconnects from the remote [[Store]].
    *
    * @return [[Unit]].
    */
  def disconnect: M[Unit]

  /**
    * Lists remote folders.
    *
    * @return A [[List]] of remote folders.
    */
  def listFolders: M[List[Folder]]

  /**
    * Attempts to open a [[Folder]] in the remote [[Store]] identified by the given name.
    *
    * @param name   Name of the [[Folder]].
    * @param write  Indicates if we are opening in read/write mode or only in read mode.
    * @return [[Some]] [[Folder]] if found, [[None]] otherwise.
    */
  def openFolder(name: String, write: Boolean): M[Either[Throwable, Folder]]

  /**
    * Closes the given [[Folder]].
    *
    * @param folder [[Folder]] to be closed.
    * @return [[Unit]].
    */
  def closeFolder(folder: Folder): M[Unit]

  /**
    * List the message contents of the [[Folder]].
    *
    * Note that the folder must be opened by the time it is given to the function.
    *
    * @param folder [[Folder]] which the contents list of.
    * @param since Date since.
    * @param until Date until.
    * @return An [[Array]] of [[Message]]s, if any.
    */
  def messages(folder: Folder, since: Option[LocalDate], until: Option[LocalDate]): M[Array[Message]]

  /**
    * Move messages from one folder to another.
    *
    * @param messages Messages to be moved.
    * @param source Source folder.
    * @param destination Target folder.
    * @return [[Unit]]
    */
  def move(messages: Array[Message], source: Folder, destination: Folder): M[Unit]
}
