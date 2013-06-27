package test.scala

import org.scalatest.FunSpec
import projectEuler.Problem1

/** Created with IntelliJ IDEA.
 * User: dmak83
 * Date: 11/13/12
 * Time: 9:01 AM
 *
 * == Change Log == 
 * 11/13/12 - initial creation
 */
class Problem1Test extends FunSpec {
  describe("Problem1") {

    it("should sum all natural numbers below 1000 that are multiples of 3 and 5") {
      val sum = Problem1.sum(1000, 3, 5)

      assert(sum === 233168)
    }
  }

}
