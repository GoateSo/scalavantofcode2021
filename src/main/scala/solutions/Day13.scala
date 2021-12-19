package solutions
import scala.collection.mutable.HashSet

/**
 * parse inputs and fold by symmetrically removing and readding dots across fold line using set
 */
class Day13(lines : IndexedSeq[String]) extends Solution(lines):
  val set = HashSet[(Int,Int)]()
  val (dots, _ +: folds) = lines.span(_ != "")
  
  for Array(a,b) <- dots.map(_.split(",")) do
    set += ((a.toInt,b.toInt))

  var fst : Option[Int] = None

  // process all the folds
  for Array(axis, num) <- folds.map(_.substring(11).split("=")) do
    val n = num.toInt
    // fold along x axis
    if axis == "x" then
      val vs = set.filter((a,_) => a >= n)    // dots to be folded
      set --= vs                              // remove them from set
      for (a,b) <- vs do                      // get their folded coordinates
        set += ((2*n-a,b))                    // add back to set
    // fold along y axis
    else
      val vs = set.filter((_,b) => b >= n)    // dots to be folded
      set --= vs                              // remove them from set
      for (a,b) <- vs do                      // get their folded coordinates
        set += ((a,2*n-b))                    // add back to set
    if fst.isEmpty then fst = Some(set.size)

  val arr = Array.fill(6,39)(" ")

  for (a,b) <- set do
    arr(b)(a) = "#"
  

  // # points after first fold
  val run = fst.get.toString

  // text after all fold
  val run2 = "\n" ++ arr.map(_.mkString).mkString("\n")
