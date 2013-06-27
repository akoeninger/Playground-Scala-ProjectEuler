package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 4/12/13
  * Time: 9:21 AM
  *
  * == Change Log ==
  * 4/12/13 - initial creation
  */
sealed abstract class Try[+T] {

  def isFailure: Boolean

  def isSuccess: Boolean

  def getOrElse[U >: T](default: => U): U =
    if (isSuccess) get else default

  def orElse[U >: T](default: => Try[U]): Try[U] =
    try if (isSuccess) this else default
    catch {
      case e: Exception => Failure(e)
    }

  def get: T

  def foreach[U](f: T => U): Unit

  def flatMap[U](f: T => Try[U]): Try[U]

  def map[U](f: T => U): Try[U]

  def filter(p: T => Boolean): Try[T]

  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U]

  def recover[U >: T](f: PartialFunction[Throwable, U]): Try[U]

  def toOption: Option[T] = if (isSuccess) Some(get) else None

  /**
   * Transforms a nested `Try`, ie, a `Try` of type `Try[Try[T]]`,
   * into an un-nested `Try`, ie, a `Try` of type `Try[T]`.
   */
  def flatten[U](implicit ev: T <:< Try[U]): Try[U]

  def failed: Try[Throwable]

  def transform[U](s: T => Try[U], f: Throwable => Try[U]): Try[U] =
    try this match {
      case Success(v) => s(v)
      case Failure(e) => f(e)
    } catch {
      case e: Exception => Failure(e)
    }

}

object Try {
  def apply[T](r: => T): Try[T] =
    try Success(r) catch {
      case e: Exception => Failure(e)
    }
}

final case class Failure[+T](exception: Throwable) extends Try[T] {
  def isFailure: Boolean = true

  def isSuccess: Boolean = false

  def get: T = throw exception

  def foreach[U](f: (T) => U) {}

  def flatMap[U](f: (T) => Try[U]): Try[U] = this.asInstanceOf[Try[U]]

  def map[U](f: (T) => U): Try[U] = this.asInstanceOf[Try[U]]

  def filter(p: (T) => Boolean): Try[T] = this

  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U] =
    try {
      if (f isDefinedAt exception) f(exception) else this
    } catch {
      case e: Exception => Failure(e)
    }

  def recover[U >: T](f: PartialFunction[Throwable, U]): Try[U] = null

  /**
   * Transforms a nested `Try`, ie, a `Try` of type `Try[Try[T]]`,
   * into an un-nested `Try`, ie, a `Try` of type `Try[T]`.
   */
  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = this.asInstanceOf[Try[U]]

  def failed: Try[Throwable] = Success(exception)
}

final case class Success[+T](value: T) extends Try[T] {
  def isFailure: Boolean = false

  def isSuccess: Boolean = true

  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U] = this

  def get: T = value

  def flatMap[U](f: (T) => Try[U]): Try[U] =
    try f(value)
    catch {
      case e: Exception => Failure(e)
    }

  /**
   * Transforms a nested `Try`, ie, a `Try` of type `Try[Try[T]]`,
   * into an un-nested `Try`, ie, a `Try` of type `Try[T]`.
   */
  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = value

  def foreach[U](f: T => U) {
    f(value)
  }

  def filter(p: (T) => Boolean): Try[T] = {
    try {
      if (p(value)) this
      else Failure(new NoSuchElementException("Predicate does not hold for " + value))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  def failed: Try[Throwable] = Failure(new UnsupportedOperationException("Success.failed"))

  def recover[U >: T](rescueException: PartialFunction[Throwable, U]): Try[U] = this

  def map[U](f: T => U): Try[U] = Try[U](f(value))
}