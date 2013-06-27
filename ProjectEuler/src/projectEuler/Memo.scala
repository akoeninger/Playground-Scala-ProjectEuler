package projectEuler

import collection.mutable

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/14/12
  * Time: 3:18 PM
  *
  * == Change Log ==
  * 8/14/12 - initial creation
  */


class Memo[K, V](f: (K => V)) extends mutable.HashMap[K, V] {
  override def apply(k: K) = getOrElseUpdate(k, f(k))
}
