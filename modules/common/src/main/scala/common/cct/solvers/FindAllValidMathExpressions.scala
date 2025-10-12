package common.cct.solvers

import scala.scalajs.js
import scala.scalajs.js.JSConverters.given
import scala.collection.mutable.ArrayBuffer

def FindAllValidMathExpressions(s: String, n: Long): js.Array[String] = {

  extension (c: Char) {
    def toN: Int = c - '0'
  }

  val answer = ArrayBuffer.empty[String]

  def rec(cs: List[Char], acc: String, head: Long, mult: Long, sum: Long): Unit =
    cs match {
      case Nil      => if (head * mult + sum == n) { answer.addOne(acc) }
      case c :: ncs =>
        rec(ncs, s"${acc}+${c}", c.toN, 1, sum + head * mult)
        rec(ncs, s"${acc}-${c}", c.toN, -1, sum + head * mult)
        rec(ncs, s"${acc}*${c}", c.toN, mult * head, sum)
        if (head != 0) { rec(ncs, s"${acc}${c}", head * 10 + c.toN, mult, sum) }
    }

  rec(s.toList.drop(1), s.head.toString, s.head.toN, 1, 0)

  answer.toJSArray
}
