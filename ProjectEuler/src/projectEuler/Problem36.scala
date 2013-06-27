package projectEuler

/**
 * Created with IntelliJ IDEA.
 * User: dmak83
 * Date: 7/16/12
 * Time: 8:59 AM
 * finished submitted
 */

object Problem36 {
  def isPalindromic(n: Int): Boolean = n.toString == n.toString.reverse

  def binaryPalindrome(n: Int): Boolean = Integer.toString(n, 2) == Integer.toString(n, 2).reverse

  def main(args: Array[String]) {
    val decPalindromes = 1 until 1000000 filter isPalindromic
    val palindromicSum = (decPalindromes filter binaryPalindrome).sum

    println("palindromicSum = " + palindromicSum)
  }
}
