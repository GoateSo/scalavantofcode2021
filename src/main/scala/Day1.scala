object Day1:
  def solve(xs : Seq[Int]) : Int =
    xs.sliding(2).filter(xs => xs(0) < xs(1)).length
  
  // pt1 solution 
  def run(lines : IndexedSeq[String]) =
    solve(lines.map(_.toInt))
  
  // pt2 solution
  def run2(lines : IndexedSeq[String]) = 
    solve(lines.map(_.toInt).sliding(3).map(_.sum).toSeq)
