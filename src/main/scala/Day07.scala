class Day7(lines : IndexedSeq[String]):
  //get crab positions
  private val xs = lines.head.split(",").map(_.toInt)
    
  // solve problem given a cost function for each distance
  private def solve(cost : Int => Int) =
    (xs.min to xs.max).map { i =>
      xs.map {x => cost(Math.abs(i-x))}.sum 
    }.min      
  
  // solves w/ linear distance
  lazy val run = solve(identity)

  // solves w/ summed distance
  lazy val run2 = solve(n => n * (n + 1) / 2)