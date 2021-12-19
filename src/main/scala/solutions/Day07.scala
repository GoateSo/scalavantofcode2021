package solutions

/**
 * find culmulative costs for movement w/ uniform and summed costs 
 */
class Day7(lines : IndexedSeq[String]) extends Solution(lines):
  //get crab positions
  private val xs = lines.head.split(",").map(_.toInt)
    
  // solve problem given a cost function for each distance
  private def solve(cost : Int => Int) =
    (xs.min to xs.max).map { i =>
      xs.map {x => cost(Math.abs(i-x))}.sum 
    }.min      
  
  // solves w/ linear distance
  val run = solve(identity).toString

  // solves w/ summed distance
  val run2 = solve(n => n * (n + 1) / 2).toString