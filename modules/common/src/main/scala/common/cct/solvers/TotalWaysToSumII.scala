package common.cct.solvers

def TotalWaysToSumII(sum: Int, xs: Array[Int]) = {
  val dp = Array.fill(sum + 1)(0)
  dp(0) = 1
  for {
    v <- xs
    i <- 0 until sum
  } {
    if (i + v <= sum) {
      dp(i + v) += dp(i)
    }
  }
  dp(sum)
}
