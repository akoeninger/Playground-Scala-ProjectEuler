package projectEuler

import collection.mutable
import scala.collection.generic.CanBuildFrom

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/24/12
  * Time: 8:37 AM
  *
  * == Change Log ==
  * 8/24/12 - initial creation
  */
class PrefixMap[T]
  extends mutable.Map[String, T]
  with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get s(0) flatMap { _.get(s substring 1) }

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None => suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) {
    withPrefix(s).value = Some(elem)
  }

  override def remove(s: String): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get (s(0)) flatMap { _.remove(s substring 1) }

  def iterator: Iterator[(String, T)] = (for (v <- value.iterator) yield ("", v)) ++
    (for {
      (chr, m) <- suffixes.iterator
      (s, v) <- m.iterator
    } yield (chr +: s, v)                                        )

  def +=(kv: (String, T)): this.type = { update(kv._1, kv._2); this }

  def -=(key: String): this.type = { remove(key); this }

  override def empty = new PrefixMap[T]
}

object PrefixMap extends {
  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }

  def newBuilder[T]: mutable.Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]:
  CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]
      def apply() = newBuilder[T]
    }
}