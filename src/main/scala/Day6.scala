object Day6:
  def solve(steps : Int)(lines : IndexedSeq[String]) = 
    var xs = Array.fill(9)(0).map(_.toLong)
    lines.head.split(",").foreach(s => xs(s.toInt) += 1)
    for i <- 0 until steps do
      val n = xs(0) 
      for j <- 0 until 8 do
        xs(j) = xs(j+1)
      xs(8) = n
      xs(6) += n
    xs.sum
    
  def run = solve(80)

  def run2 = solve(256)
    