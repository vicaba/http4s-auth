ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file(".")).settings(
  name := "http4s-auth"
)

val scala3Version = "3.2.2"

val Http4sVersion = "1.0.0-M40"
val JwtHttp4sVersion = "1.2.1"
val JwtScalaVersion = "9.4.3"

val http4sDsl = "org.http4s" %% "http4s-dsl" % Http4sVersion
val emberServer = "org.http4s" %% "http4s-ember-server" % Http4sVersion
val jwtHttp4s = "dev.profunktor" %% "http4s-jwt-auth" % JwtHttp4sVersion % VersionScheme.Always
val jwtScala = "com.github.jwt-scala" %% "jwt-core" % JwtScalaVersion
val jwtCirce = "com.github.jwt-scala" %% "jwt-circe" % JwtScalaVersion

libraryDependencies ++= Seq(
  emberServer,
  http4sDsl,
  jwtHttp4s,
  jwtScala,
  jwtCirce
)
