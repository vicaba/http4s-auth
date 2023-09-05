package rockthejvm.demo

import cats.data.*
import cats.effect.*
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.*
import com.comcast.ip4s.*
import org.http4s.*
import org.http4s.server.*
import org.http4s.implicits.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.*
import org.http4s.server.middleware.authentication.DigestAuth
import org.http4s.server.middleware.authentication.DigestAuth.Md5HashedAuthStore
import pdi.jwt.JwtAlgorithm.*
import pdi.jwt.*

import io.circe.*
import io.circe.parser.*

import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalTime}
import java.util.Base64
import scala.util.Try

case class User(id: Long, name: String)

object HttpAuthDemo extends IOApp.Simple {

  val routes: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "welcome" / user =>
        Ok(s"Welcome, $user")
    }

  // simple auth method - basic
  // Request[IO] => IO[Either[String, User]]
  // Kleisli[IO, Request[IO], Either[String, User]]
  // Kleisli[F, A, B] equivalent to A => F[B]
  val basicAuthMethod = Kleisli.apply[IO, Request[IO], Either[String, User]] { req =>
    // auth logic
    val authHeader = req.headers.get[Authorization]
    authHeader match {
      case Some(Authorization(BasicCredentials(creds))) =>
        IO(Right(User(1L, creds._1)))
      // Check your own password
      case Some(_) => IO(Left("No basic credentials"))
      case None => IO(Left("Unauthorized!"))
    }
  }

  val onFailure: AuthedRoutes[String, IO] = Kleisli { (req: AuthedRequest[IO, String]) =>
    OptionT.pure(Response[IO](status = Status.Unauthorized))
  }

  // Middleware
  val userBasicAuthMiddleware: AuthMiddleware[IO, User] =
    AuthMiddleware(basicAuthMethod, onFailure)

  val authRoutes =
    AuthedRoutes.of[User, IO] {
      case GET -> Root / "welcome" as user =>
        Ok(s"Welcome, $user")
    }

  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(userBasicAuthMiddleware(authRoutes).orNotFound)
    .build

  override def run: IO[Unit] = server.use(_ => IO.never).void
}

// 2 - Digest authentication

object HttpDigestAuthDemo extends IOApp.Simple {

  // query my database for a user and create digest
  val searchFunc: String => IO[Option[(User, String)]] = {
    case "daniel" => // need to return an IO(Some(User(1, Daniel)), hash of daniel)
      for {
        user <- IO.pure(User(1L, "daniel"))
        hash <- Md5HashedAuthStore
          .precomputeHash[IO]("daniel", "http://localhost:8080", "rockthejvm")
      } yield Some((user, hash))
    case _ => IO.pure(None) // User cannot be found
  }

  val authStore = Md5HashedAuthStore(searchFunc)
  val middleware: IO[AuthMiddleware[IO, User]] =
    DigestAuth.applyF[IO, User]("http://localhost:8080", authStore)

  val authRoutes =
    AuthedRoutes.of[User, IO] {
      case GET -> Root / "welcome" as user =>
        Ok(s"Welcome, $user")
    }

  val middlewareResource = Resource.eval(middleware)

  val serverResource =
    for {
      mw <- middlewareResource
      sv <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(mw(authRoutes).orNotFound)
        .build
    } yield sv

  override def run: IO[Unit] = serverResource.use(_ => IO.never).void
}

// 3 - sessions
object HttpSessionDemo extends IOApp.Simple {

  def today: String = LocalTime.now().toString

  def setToken(user: String, date: String) =
    Base64.getEncoder.encodeToString(s"$user:$date".getBytes(StandardCharsets.UTF_8))

  def getUser(token: String): Option[String] =
    Try(Base64.getDecoder.decode(token))
      .toOption
      .map(user => new String(user, StandardCharsets.UTF_8).split(":")(0))

  def authedRoutes = AuthedRoutes.of[User, IO] {
    case GET -> Root / "welcome" as user =>
      Ok(s"Welcome, $user").map(_.addCookie(
        ResponseCookie("sessioncookie", setToken(user.name, today), maxAge = Some(24 * 6000))))
  }

  // query my database for a user and create digest
  val searchFunc: String => IO[Option[(User, String)]] = {
    case "daniel" => // need to return an IO(Some(User(1, Daniel)), hash of daniel)
      for {
        user <- IO.pure(User(1L, "daniel"))
        hash <- Md5HashedAuthStore
          .precomputeHash[IO]("daniel", "http://localhost:8080", "rockthejvm")
      } yield Some((user, hash))
    case _ => IO.pure(None) // User cannot be found
  }

  val authStore = Md5HashedAuthStore(searchFunc)
  val middleware: IO[AuthMiddleware[IO, User]] =
    DigestAuth.applyF[IO, User]("http://localhost:8080", authStore)

  // digest auth end
  val cookieAccessRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "statement" =>
      Ok("This is your financial statement, $user")
    case GET -> Root / "logout" =>
      Ok("Logging Out").map(_.removeCookie("sessioncookie"))
  }

  def checkSessionCookie(cookie: Cookie): Option[RequestCookie] =
    cookie.values.toList.find(_.name == "sessioncookie")

  def modifyPath(user: String): Path = Uri.Path.unsafeFromString(s"/statement/$user")

  def cookieCheckerApp(app: HttpRoutes[IO]): HttpRoutes[IO] = Kleisli { req =>
    val authHeader: Option[Cookie] = req.headers.get[Cookie]
    OptionT.liftF(
      authHeader.fold(Ok("No cookies")) { cookie =>
        checkSessionCookie(cookie).fold(Ok("No token")) { token =>
          getUser(token.content).fold(Ok("Invalid token")) { user =>
            app.orNotFound.run(req.withPathInfo(modifyPath(user)))
          }
        }
      }
    )
  }

  val routerResource = middleware.map { mw =>
    Router(
      "/login" -> mw(authedRoutes), // login endpoint unauthed
      "/" -> cookieCheckerApp(cookieAccessRoutes) // authed endpoints
    )

  }

  val serverResource =
    for {
      router <- Resource.eval(routerResource)
      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(router.orNotFound)
        .build
    } yield server

  override def run: IO[Unit] = serverResource.use(_ => IO.never).void
}

// 4 - JWT
// Not working, JWT library not compatible
/*object HttpJwtDemo extends IOApp.Simple {

  def authedRoutes = AuthedRoutes.of[User, IO] {
    case GET -> Root / "welcome" as user =>
      Ok(s"Welcome, $user")
        .map(_.addCookie(ResponseCookie("token", token, maxAge = Some(24 * 6000))))
  }

  // query my database for a user and create digest
  val searchFunc: String => IO[Option[(User, String)]] = {
    case "daniel" => // need to return an IO(Some(User(1, Daniel)), hash of daniel)
      for {
        user <- IO.pure(User(1L, "daniel"))
        hash <- Md5HashedAuthStore
          .precomputeHash[IO]("daniel", "http://localhost:8080", "rockthejvm")
      } yield Some((user, hash))
    case _ => IO.pure(None) // User cannot be found
  }

  val authStore = Md5HashedAuthStore(searchFunc)
  val middleware: IO[AuthMiddleware[IO, User]] =
    DigestAuth.applyF[IO, User]("http://localhost:8080", authStore)

  case class TokenPayload(user: String, permsLevel: String)

  object TokenPayload {
    given decoder: Decoder[TokenPayload] = Decoder.instance { hCursor =>
      for {
        user <- hCursor.get[String]("user")
        permsLevel <- hCursor.get[String]("level")
      } yield TokenPayload(user, permsLevel)
    }
  }

  // JWT logic
  def claim(user: String, permsLevel: String) = JwtClaim(
    content = s"""
                 | {
                 |  "user": "$user",
                 |  "level": "$permsLevel"
                 | }
                 |""".stripMargin,
    expiration = Some(Instant.now().plusSeconds(10 * 24 * 3600).getEpochSecond),
    issuedAt = Some(Instant.now().getEpochSecond)
  )

  val key = "tobeconfigured"
  val algo = HS256
  val token = JwtCirce.encode(claim("Daniel", "basic"), key, algo) // build a manual Jwt

  val database = Map(
    "daniel" -> User(1L, "Daniel")
  )

  val authorizedFunction: _ => JwtClaim => IO[Option[User]] =
    token =>
      claim =>
        decode[TokenPayload](claim.content) match {
          case Left(_) => IO.pure(None)
          case Right(payload) => IO(database.get(payload.user))
        }

  val middleware = JwtAuthMiddleware[IO, User](JwtAuth)

  val routerResource = middleware.map { mw =>
    Router(
      "/login" -> mw(authedRoutes), // login endpoint unauthed
      "/" -> cookieCheckerApp(cookieAccessRoutes) // authed endpoints
    )

  }

  val serverResource =
    for {
      router <- Resource.eval(routerResource)
      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(router.orNotFound)
        .build
    } yield server

  override def run: IO[Unit] = serverResource.use(_ => IO.never).void
}*/
