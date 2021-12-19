package solutions
import utils.Utils.MinPq

/**
 * parse inputs and pathfind using Dijkstra's algorithm w/ Binary Heap
 */
class Day15(lines: IndexedSeq[String]) extends Solution(lines):
  private val arr1 = lines.toArray.map(_.map(_ - '0').toArray)
  private val nar = Array.fill(5 * arr1.size, 5 * arr1.size)(0)

  private def neighbors(i: Int, j: Int)(arr: Array[Array[Int]]) =
    List((i + 1, j), (i, j + 1), (i, j - 1), (i - 1, j))
      .filter { (a, b) => a >= 0 && b >= 0 && a < arr.size && b < arr.size }

  for
    i <- 0 until nar.size
    j <- 0 until nar.size
  do 
    val n = arr1(i % arr1.size)(j % arr1.size) + i / arr1.size + j / arr1.size
    nar(i)(j) = (n - 1) % 9 + 1

  private val arr = nar

  private def findpath(arr: Array[Array[Int]]) =
    val dists = Array.fill(arr.size, arr.size)(Integer.MAX_VALUE)
    dists(0)(0) = 0
    val vs = MinPq((0, 0), dists(0)(0))
    var gotGoal = false
    while vs.map.nonEmpty && !gotGoal do
      val (cur @ (a, b), di) = vs.pop
      if a == arr.length - 1 && b == arr.length - 1 then gotGoal = true
      else
        for nei @ (i, j) <- neighbors(a, b)(arr) do
          val tenDist = dists(a)(b) + arr(i)(j)
          if tenDist < dists(i)(j) then
            dists(i)(j) = tenDist
            if !vs.map.contains(nei) then vs += ((i, j), dists(i)(j))
    dists(arr.size - 1)(arr.size - 1)

  val run = findpath(arr1).toString

  val run2 = findpath(arr).toString
