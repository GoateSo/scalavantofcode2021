package solutions
class Day11(lines : IndexedSeq[String]) extends Solution(lines):
  private val grid = lines.map(_.map(_-'0').toArray).toArray
  private val l = grid.size
  private val w = grid(0).size
  private def n2(i : Int, j: Int) = 
    List(
      (i+1,j-1),(i+1,j),(i+1,j+1),
      (i,j-1),          (i,j+1),
      (i-1,j-1),(i-1,j),(i-1,j+1)
    ) filter { (a,b) => 
      a >= 0 && b >= 0 && a < l && b < w
    }

  private def neighbors(i : Int, j : Int) = 
    n2(i,j) map (grid(_)(_))

  // 1 turn w/ brute forced flashing
  def flash = 
    var flashes = 0
    for i <- 0 until l
        j <- 0 until w
    do grid(i)(j) += 1
      while grid.exists(_.exists(_ >= 10)) do                      // while stuff hasnt flashed
        for i <- 0 until l
            j <- 0 until w if grid(i)(j) >= 10 do             // go through everything that should be flashed
              flashes += 1
              grid(i)(j) = 0                                       // it resets
              for (a,b) <- n2(i,j) if grid(a)(b) != 0 do // increment not already flashed neighbors
                grid(a)(b) += 1
    flashes
  
  // how many flashes after 100 turns
  val run = 
    (1 to 100).map(_ => flash).sum.toString

  // amount of turns until homogenous 
  val run2 =  
    var times = 0    
    // flash until exhausted
    while grid.flatten.toSet.size > 1 do
      flash
      times += 1
    (times + 100).toString // account for flashes in part 1