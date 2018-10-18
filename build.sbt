// Define versions for libraries:
val VersionCats       = "1.0.1"
val VersionCatsEffect = "0.8"
val VersionFansi      = "0.2.5"
val VersionScallop    = "3.1.1"
val VersionMail       = "1.4.1"


// Configure the root project:
lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    // Top-level Settings:
    name := "demail",
    organization := "com.vsthost.rnd",
    scalaVersion := "2.12.4",
    version := "0.0.1-SNAPSHOT",

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
      "javax.mail"    %  "mail"        % VersionMail,
      "org.rogach"    %% "scallop"     % VersionScallop,
    )
  )