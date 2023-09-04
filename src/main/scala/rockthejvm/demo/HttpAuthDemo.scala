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
import org.http4s.headers.Authorization

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
      case Some(Authorization(BasicCredentials(creds))) => IO(Right(User(1L, creds._1)))
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
