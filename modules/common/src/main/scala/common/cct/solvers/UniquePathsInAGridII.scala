package common.cct.solvers

def UniquePathsInAGridII(map: Vector[Vector[Int]]) = {
  val height = map.length
  val width = map.head.length

  def dp = collection.mutable.Map.empty[(Int, Int), Int]

  def rec(y: Int, x: Int): Int = (for {
    _ <- Either.cond(!(y + 1 == height && x + 1 == width), (), 1)
    _ <- Either.cond(y != height && x != width, (), 0)
    _ <- Either.cond(map(y)(x) == 0, (), 0)
    _ <- dp.get((y, x)).toLeft(())
  } yield {
    val res = rec(y + 1, x) + rec(y, x + 1)
    dp.addOne((y, x), res)
    res
  }).merge

  rec(0, 0)
}
