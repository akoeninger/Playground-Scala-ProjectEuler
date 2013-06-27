package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/9/12
  * Time: 8:27 AM
 * solved submitted
  */

/** Finds the largest palindrome number that is the product of two 3-digit numbers
  *
  */
object Problem4 {
  def isPalindrome(s: String): Boolean = s == s.reverse

  def largestPalindrome: Int = {
    val palindromes = (100 to 999).view flatMap { i =>
      i to 999  map (i *)
    } filter { n =>
      isPalindrome(n.toString)
    }
    palindromes.max
  }
}
