import scala.collection.mutable.HashSet

class Day13(lines : IndexedSeq[String]):
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
  lazy val run = fst.get

  // text after all fold
  lazy val run2 = "\n" ++ arr.map(_.mkString).mkString("\n")
