// Define versions for libraries:
val VersionCats       = "1.4.0"
val VersionCatsEffect = "1.0.0"
val VersionFansi      = "0.2.5"
val VersionScallop    = "3.1.3"
val VersionMail       = "1.6.2"


// Configure the root project:
lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging)
  .settings(
    // Top-level Settings:
    name := "demail",
    organization := "com.vsthost.rnd",
    scalaVersion := "2.12.7",
    version := "0.0.4-SNAPSHOT",

    // Scalac Options:
    scalacOptions += "-deprecation",

    // BuildInfo Settings:
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.vsthost.rnd.demail",

    // Libraries:
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % VersionCats,
      "org.typelevel" %% "cats-effect" % VersionCatsEffect,
      "com.lihaoyi"   %% "fansi"       % VersionFansi,
      "com.sun.mail"  %  "javax.mail"  % VersionMail,
      "com.sun.mail"  %  "gimap"       % VersionMail,
      "org.rogach"    %% "scallop"     % VersionScallop,
    ),

    // Debian packaging configuration:
    maintainer := "Vehbi Sinan Tunalioglu <vst@vsthost.com>",
    packageSummary := "CLI application to work with IMAP accounts.",
    packageDescription := """List remote IMAP folders, messages and download attachments""",
  )
