package projectEuler

import collection.mutable

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 1/23/13
  * Time: 2:47 PM
  *
  * == Change Log ==
  * 1/23/13 - initial creation
  */
object Problem411 {
  def main(args: Array[String]) {
    val test = stations(math.pow(30, 5).toInt) sortBy {
      case (x, y) => x * y
    }

    val s = traversePathMemo(test)
    println("s = " + s)

  }

  def stations(n: Int) = {
    def makePoint(i: Int, n: Int) = (BigInt(2).modPow(BigInt(i), BigInt(n)).toInt, BigInt(3).modPow(BigInt(i), BigInt(n)).toInt)
    val points = for (i <- 0 to 2 * n) yield makePoint(i, n)
    points.distinct
  }

  def pathCount(st: Seq[(Int, Int)]) = for {
    s <- st
    p = st.filter(o => o._1 <= s._1 && o._2 <= s._2)
  } yield (s, p.size)

  def validStation(cur: (Int, Int), other: (Int, Int)) = (cur != other) && (other._1 <= cur._1) && (other._2 <= cur._2)

  def traversePath(stationPoints: Seq[(Int, Int)]) = {

    /** For a given station, pick the station that comes before it with the most stops along its route from the origin
      * For all the stations that can come before the current one, calculate their max paths from origin,
      * then choose the station with the greatest number of stops along the route.
      *
      * @param current
      * @param others
      * @param visited
      * @return
      */
    def nextStation(current: (Int, Int), others: Seq[(Int, Int)], visited: Int): Int = {


      val validStations = others.filter(s => validStation(current, s)) // Get stations that can come before current
      if (validStations.isEmpty) // first station from origin
        visited + 1
      else {
        //        val counts = pathCount(validStations)
        //        val next = counts.maxBy(_._2)._1
        //        nextStation(next, validStations, visited + 1)
        val temp = validStations.map(v => nextStation(v, validStations, 0)) // Calculate stops for stations

        temp.max + 1
      }
    }



    val c = pathCount(stationPoints)
    val start = c.maxBy(_._2)._1

    nextStation(start, stationPoints.filter(validStation(start, _)), 0)
  }

  def traversePathMemo(stationPoints: Seq[(Int, Int)]): Int = {
    val hashStations = mutable.Map.empty[(Int, Int), Int]


    def pathMemo(current: (Int, Int), others: Seq[(Int, Int)]): Int = if (others.isEmpty) {
      hashStations.getOrElseUpdate(current, 1)
    } else {
      val paths = for {
        newStart <- others
      } yield (hashStations.getOrElseUpdate(newStart, pathMemo(newStart, others.filter(t => validStation(newStart, t)))))

      paths.max + 1
    }

    val paths = for (st <- stationPoints) yield hashStations.getOrElseUpdate(st, pathMemo(st, stationPoints.filter(t => validStation(st, t))))
    paths.max
  }
}
