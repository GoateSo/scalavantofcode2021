import Utils.MinPq

class Day15(lines: IndexedSeq[String]):
  val arr1 = lines.toArray.map(_.map(_ - '0').toArray)
  val nar = Array.fill(5 * arr1.size, 5 * arr1.size)(0)

  def neighbors(i: Int, j: Int)(arr: Array[Array[Int]]) =
    List((i + 1, j), (i, j + 1), (i, j - 1), (i - 1, j))
      .filter { (a, b) => a >= 0 && b >= 0 && a < arr.size && b < arr.size }
  for
    i <- 0 until nar.size
    j <- 0 until nar.size
  do {
    val n = arr1(i % arr1.size)(j % arr1.size) + i / arr1.size + j / arr1.size
    nar(i)(j) = (n - 1) % 9 + 1
  }
  val arr = nar

  def h(v: (Int, Int))(arr: Array[Array[Int]]): Int =
    arr.size - v._1 + arr(0).size - v._2
  //println(arr.map(_.mkString).mkString("\n"))

  def findpath(arr: Array[Array[Int]]) =
    val dists = Array.fill(arr.size, arr.size)(Integer.MAX_VALUE)
    val prevs = Array.fill[(Int, Int)](arr.size, arr.size)(null)
    val fscor =
      Array.fill(arr.size, arr.size)(Integer.MAX_VALUE) // heuristic + dist
    dists(0)(0) = 0
    fscor(0)(0) = h((0, 0))(arr)
    val vs = MinPq((0, 0), fscor(0)(0))
    var gotGoal = false
    while vs.map.nonEmpty && !gotGoal do
      val (cur @ (a, b), di) = vs.pop
      if a == arr.length - 1 && b == arr.length - 1 then gotGoal = true
      else
        for nei @ (i, j) <- neighbors(a, b)(arr) do
          val tenDist = dists(a)(b) + arr(i)(j)
          if tenDist < dists(i)(j) then
            prevs(i)(j) = cur
            dists(i)(j) = tenDist
            fscor(i)(j) = tenDist + h(nei)(arr)
            if !vs.map.contains(nei) then vs += ((i, j), fscor(i)(j))
    dists(arr.size - 1)(arr.size - 1)

  val run = findpath(arr1)

  val run2 = findpath(arr)
