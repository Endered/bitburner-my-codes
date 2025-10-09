package common.cct.solvers
import scala.scalajs.js
import util.min
import cats.kernel.Semigroup

def ArrayJumpingGameII(data: Array[Int]): Double = {
  val n = data.length
  val dp = Array.fill(n)(None: Option[Int])
  dp(0) = Some(0)

  for {
    i <- 0 to (n - 1)
    p <- (i + 1) to (n - 1).min(i + data(i))
    v <- dp(i)
    nv = Some(v + 1)
  } {
    if (dp(p).isDefined) {
      dp(p) = Ordering[Option[Int]].min(dp(p), nv)
    } else {
      dp(p) = nv
    }
  }

  dp(n - 1).getOrElse(0).toDouble
}
