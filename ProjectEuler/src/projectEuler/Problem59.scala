package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 11/2/12
  * Time: 10:26 AM
  * Decryption problem
  * == Change Log ==
  * 11/2/12 - initial creation
  * 11/5/12 - solved and submitted
  */
object Problem59 {
  val file: List[String] = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\cipher1.txt").getLines().toList
  val words: List[String] = file flatMap {
    s => s.split(",")
  }

  val wordBytes: List[Byte] = words.map(_.toByte)
  val keys = "abcdefghijklmnopqrstuvwxyz".map(_.toByte).combinations(3).toStream
  lazy val keyPermutations = keys.flatMap(k => k.permutations.toStream)

  def decrypt(cipher: Seq[Byte]): String = {
    @tailrec
    def decryptR(group: List[List[Byte]], ci: Seq[Byte], text: String): String = group match {
      case Nil => text
      case h :: t => {
        val g = h.zip(ci).map(i => (i._1 ^ i._2).toChar)
        decryptR(t, ci, text + g.mkString)
      }
    }
    decryptR(wordBytes.grouped(3).toList, cipher, "")
  }

  def main(args: Array[String]) {
    val code = keyPermutations collectFirst {
      case ci if decrypt(ci).contains(" the ") => ci
    }
    val originalText = decrypt(code.get)
    println(decrypt(code.get) + " " + words.size)
    val byteSum = code flatMap {
      c => Some(decrypt(c).map(_.toInt).sum)
    }
    println("byteSum = " + byteSum)
  }
}
