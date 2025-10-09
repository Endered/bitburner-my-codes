package common.cct.solvers

def MinimumPathSumInATriangle(data: Vector[Vector[Int]]): Int = {
  val n = data.length
  val max = data.flatten.max
  val dp = Array.fill(n, n)(max * n * 2)
  dp(0)(0) = data(0)(0)

  for {
    y <- 0 until (n - 1)
    x <- 0 to y
    nx <- x to x + 1
  } {
    dp(y + 1)(nx) = dp(y + 1)(nx).min(dp(y)(x) + data(y + 1)(nx))
  }

  dp(n - 1).min
}
