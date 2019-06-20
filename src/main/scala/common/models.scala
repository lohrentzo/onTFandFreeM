package common

case class User(id: Long, name: String, age: Int)

class DatabaseError extends Throwable
case object ErrorFindingUser extends DatabaseError
case object ErrorUpdatingUser extends DatabaseError
case class ErrorDeletingUser(msg: String) extends DatabaseError
