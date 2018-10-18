package com.vsthost.rnd.demail.imap

import javax.mail._

import cats.effect.Effect
import cats.implicits._

import scala.language.higherKinds
import scala.util.Try


/**
  * Provides the default mail repository implementation.
  *
  * @param host Remote IMAP server host.
  * @param port Remote IMAP server port.
  * @param ssl  Use SSL?
  * @param user Username of the remote IMAP server connection credentials.
  * @param pass Password of the remote IMAP server connection credentials.
  * @param M    Hint for the final effect type.
  * @tparam M Effect type parameter.
  */
class DefaultMailRepository[M[_]](host: String,
                                  port: Int,
                                  ssl: Boolean,
                                  user: String,
                                  pass: String)
                                 (implicit M : Effect[M]) extends MailRepository[M] {
  /**
    * Defines the store over which we keep the IMAP connection to the remote.
    */
  lazy private val _store: Store = {
    // Define the system properties:
    val props = System.getProperties

    // Set parameters:
    props.setProperty("mail.store.protocol", if (ssl) "imaps" else "imap")

    // Get the session:
    val session = Session.getDefaultInstance(props, null)

    // Get the store and return:
    session.getStore(if (ssl) "imaps" else "imap")
  }

  /**
    * Indicates if the repository is connected.
    *
    * @return `true` if connected, `false` otherwise.
    */
  override def isConnected: M[Boolean] = _store.isConnected.pure[M]

  /**
    * Connects to the remote [[Store]].
    *
    * @return [[Either]] a [[Throwable]] in case of failure, [[Unit]] otherwise.
    */
  override def connect: M[Either[Throwable, Unit]] = for {
    // Get the connection status:
    status <- isConnected

    // Are we already connected?
    retval = if (status) {
      // Yep, return:
      ().asRight
    } else {
      // Nope, attempt to connect:
      Try(_store.connect(host, port, user, pass)).toEither
    }
  } yield retval

  /**
    * Disconnects from the remote [[Store]].
    *
    * @return [[Unit]].
    */
  override def disconnect: M[Unit] = for {
    // Get the connection status:
    status <- isConnected

    // Are we already connected?
    retval = if (status) {
      // Yep, close:
      _store.close().asRight
    } else {
      // Nope, not connected anyway. Return as is:
      ().asRight
    }
  } yield retval


  /**
    * Lists remote folders.
    *
    * @return A [[List]] of remote folders.
    */
  override def listFolders: M[List[Folder]] = M.delay {
    _store.getDefaultFolder.list("*").toList
  }

  /**
    * Attempts to open a [[Folder]] in the remote [[Store]] identified by the given name.
    *
    * @param name  Name of the [[Folder]].
    * @param write Indicates if we are opening in read/write mode or only in read mode.
    * @return [[Some]] [[Folder]] if found, [[None]] otherwise.
    */
  override def openFolder(name: String, write: Boolean): M[Either[Throwable, Folder]] = for {
    connected <- isConnected
    retval = if (!connected) {
      new Throwable("The connection is not established yet.").asLeft
    } else {
      Some(_store.getFolder(name)).filter(_.exists()).map(f => {
        // Attempt to open the folder:
        f.open(if (write) Folder.READ_WRITE else Folder.READ_ONLY)

        // Return:
        f
      }).toRight(new Throwable("Folder does not exist"))
    }
  } yield retval

  /**
    * Closes the given [[Folder]].
    *
    * @param folder [[Folder]] to be closed.
    * @return [[Unit]].
    */
  override def closeFolder(folder: Folder): M[Unit] = M.delay {
    if (folder.isOpen) {
      folder.close(false)
    } else {
      ()
    }
  }

  /**
    * List the contents of the [[Folder]].
    *
    * Note that the folder must be opened by the time it is given to the function.
    *
    * @param folder [[Folder]] which the contents list of.
    * @return An [[Array]] of [[Message]]s, if any.
    */
  override def listFolder(folder: Folder): M[Array[Message]] = M.delay {
    if (folder.isOpen) {
      folder.getMessages
    }
    else {
      Array.empty
    }
  }

  /**
    * Move messages from one folder to another.
    *
    * @param messages    Messages to be moved.
    * @param source      Source folder.
    * @param destination Target folder.
    * @return [[Unit]]
    */
  override def move(messages: Array[Message], source: Folder, destination: Folder): M[Unit] = M.delay {
    // Copy messages:
    source.copyMessages(messages, destination)

    // Mark as deleted under original directory:
    messages.foreach(_.setFlag(Flags.Flag.DELETED, true))

    // Expunge now:
    source.expunge()
  }
}