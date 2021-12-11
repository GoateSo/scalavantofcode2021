class Day1(lines: IndexedSeq[String]):
  val inputs = lines.map(_.toInt)
  private def solve(xs : Seq[Int]) : Int =
    xs.sliding(2).filter(xs => xs(0) < xs(1)).length
  
  // pt1 solution 
  def run = solve(inputs)
  
  // pt2 solution
  def run2 = solve(inputs.sliding(3).map(_.sum).toSeq)
