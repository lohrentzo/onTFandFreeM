package TF

import common._

import cats.data.{EitherK, EitherT}
import cats.{InjectK, Monad, ~>}
import cats.implicits._

import scala.collection.mutable

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// boilerplate

// The initial api we will be converting:
trait Database[T] {
  def create(t: T): Future[Boolean]
  def read(id: Long): Future[Either[DatabaseError, T]]
  def delete(id: Long): Future[Either[DatabaseError, Unit]]
}


// We just took the future out and moved it up to the trait declaration in the form of F[_]
trait DatabaseAlgebra [F[_], T] {
  def create(t: T): F[Boolean]
  def read(id: Long): F[Either[DatabaseError, T]]
  def delete(id: Long): F[Either[DatabaseError, Unit]]
}

// create interpreters to execute our actions using Futures
object DatabaseAlgebra {

  val FutureInterpreter: DatabaseAlgebra[Future, User] =
    new DatabaseAlgebra[Future, User] {
      val users: mutable.Map[Long, User] = mutable.Map.empty

      override def create(user: User): Future[Boolean] = {
        val inserted = users.put(user.id, user)
        Future.successful(inserted.isEmpty || inserted.isDefined)
      }

      override def read(id: Long): Future[Either[DatabaseError, User]] =
        Future.successful(users.get(id).toRight(ErrorFindingUser))

      override def delete(id: Long): Future[Either[DatabaseError, Unit]] = {
        import cats.syntax.either._ // for the .asLeft[]. This is also another smart constructor to help the compiler along.
        val deleted = users.remove(id)
        Future.successful(
          deleted.fold(ErrorDeletingUser(s"User with Id($id) was not there").asLeft[Unit])(_ => Right(())))
      }
    }

}

// let’s write some repos to wrap our low-level DB code:
class UserRepo[F[_]](DB: DatabaseAlgebra[F, User])(implicit M: Monad[F]) {

  def getUser(id: Long): F[Either[DatabaseError, User]] = DB.read(id)
  def addUser(user: User): F[Boolean] = DB.create(user)

  def updateUser(user: User): F[Either[DatabaseError, Boolean]] = {
    (for {
      userFromDB <- EitherT(getUser(user.id))
      successfullyAdded <- EitherT.liftF[F, DatabaseError, Boolean](addUser(user))
    } yield successfullyAdded).value
  }

}

// The code calling all of this
object UserRepoRunner extends App {

  val repo = new UserRepo(DatabaseAlgebra.FutureInterpreter)

  println(Await.result(
    (for {
      _ <- repo.addUser(User(1, "Bob", 31))
      dbErrorOrSuccessfullyUpdated <- repo.updateUser(User(1, "Bobby", 31))
    } yield dbErrorOrSuccessfullyUpdated),
    1 second))

}
