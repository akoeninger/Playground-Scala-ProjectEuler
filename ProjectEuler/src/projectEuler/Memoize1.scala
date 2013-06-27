package projectEuler

import collection.mutable

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 11/9/12
  * Time: 10:24 AM
  *
  * == Change Log ==
  * 11/9/12 - initial creation
  */
class Memoize1[-T, +R](f: T => R) extends (T => R) {
  private[this] val values = mutable.Map.empty[T, R]

  def apply(x: T): R = if (values.contains(x)) {
    values(x)
  } else {
    val y = f(x)
    values += ((x, y))
    y
  }
}

object Memoize1 {
  def apply[T, R](f: T => R) = new Memoize1(f)

  def Y[T, R](f: (T, T => R) => R): (T => R) = {
    var yf: T => R = null
    yf = Memoize1(f(_, yf(_)))
    yf
  }
}

object Y {
  def apply[T, R](f: (T, T => R) => R): (T => R) = {
    var yf: T => R = null
    yf = f(_, yf(_))
    yf
  }
}