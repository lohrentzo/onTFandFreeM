package FreeM

import common._

import cats.data.{EitherK, EitherT}
import cats.free.Free
import cats.{InjectK, Monad, ~>}
import cats.implicits._

import scala.collection.mutable

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait Database[T] {
  def create(t: T): Future[Boolean]
  def read(id: Long): Future[Either[DatabaseError, T]]
  def delete(id: Long): Future[Either[DatabaseError, Unit]]
}

/** This is your ADT - specifically, a GADT for the astute reader ;-) */
sealed trait DBFreeAlgebraT[T]
case class Create[T](t: T) extends DBFreeAlgebraT[Boolean]
case class Read[T](id: Long) extends DBFreeAlgebraT[Either[DatabaseError, T]]
case class Delete[T](id: Long) extends DBFreeAlgebraT[Either[DatabaseError, Unit]]
/********************************************************/

object DBFreeAlgebraT {
  type DBFreeAlgebra[T] = Free[DBFreeAlgebraT, T]

  // Smart constructors  
  def create[T](t: T): DBFreeAlgebra[Boolean] =
    Free.liftF[DBFreeAlgebraT, Boolean](Create(t))

  def read[T](id: Long): DBFreeAlgebra[Either[DatabaseError, T]] =
    Free.liftF[DBFreeAlgebraT, Either[DatabaseError, T]](Read(id))

  def delete[T](id: Long): DBFreeAlgebra[Either[DatabaseError, Unit]] =
    Free.liftF[DBFreeAlgebraT, Either[DatabaseError, Unit]](Delete(id))
    
  val FutureInterpreter = new (DBFreeAlgebraT ~> Future) {
    /**
    * The above is equivalent to the signature below
    * `val FutureInterpreter = new FunctionK[DBFreeAlgebraT, Future]`
    * It's like a function on values from A => B
    * The only difference this works on "Kinds" hence `FunctionK`
    * This basically says you are creating a function from F[A] => F[B]
    * We're converting our free structure(F[A]) to a known Monad(F[B])
    * In this case, we're converting DBFreeAlgebra[T] => Future[T]
    */
        val users: mutable.Map[Long, User] = mutable.Map.empty
    
        override def apply[A](fa: DBFreeAlgebraT[A]): Future[A] =
          fa match {
            case Create(user) => //F[A]
              val castedUser = user.asInstanceOf[User]
              val inserted = users.put(castedUser.id, castedUser)
              Future.successful(inserted.isEmpty || inserted.isDefined).asInstanceOf[Future[A]] //F[B]
            case Read(id) => //F[A]
              Future.successful(users.get(id).toRight(ErrorFindingUser)).asInstanceOf[Future[A]] //F[B]
            case Delete(id) => { //F[A]
              import cats.syntax.either._
              val deleted = users.remove(id)
              Future.successful(
                deleted.fold(ErrorDeletingUser(s"User with Id($id) was not there").asLeft[Unit])(_ => Right(()))
              ).asInstanceOf[Future[A]] //F[B]
            }
        }
    }
}

class FreeUserRepo {
  val DB = DBFreeAlgebraT
  import DBFreeAlgebraT.DBFreeAlgebra

  def getUser(id: Long): DBFreeAlgebra[Either[DatabaseError, User]] = DB.read(id) //this is a program
  def addUser(user: User): DBFreeAlgebra[Boolean] = DB.create(user) //this is a program

//this is a program
  def updateUser(user: User): DBFreeAlgebra[Either[DatabaseError, Boolean]] = (for {
    userFromDB <- EitherT(getUser(user.id))
    successfullyAdded <- EitherT.liftF[DBFreeAlgebra, DatabaseError, Boolean](addUser(user))
  } yield successfullyAdded).value
}

object DBFreeAlgebraRunner extends App {

  val repo = new FreeUserRepo
  
  println(Await.result(
    (for {
      _ <- repo.addUser(User(2, "Bob", 31))
      dbErrorOrSuccessfullyUpdated <- repo.updateUser(User(2, "Bobby", 31))
    } yield dbErrorOrSuccessfullyUpdated).foldMap(DBFreeAlgebraT.FutureInterpreter), //notice the foldMap(..) here
    1 second))

}