package common.cct.solvers

def EncryptionIIVigenereCipher(s: String, t: String): String = {
  def toN(c: Char): Int = c - 'A'
  def toC(n: Int): Char = ('A' + (n % 26)).toChar
  s.zip(Iterator.continually(t).flatten).map((a, b) => toC(toN(a) + toN(b))).mkString
}
