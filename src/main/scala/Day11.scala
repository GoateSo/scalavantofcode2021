class Day11(lines : IndexedSeq[String]):
  val grid = lines.map(_.toIndexedSeq.map(_-'0').toArray).toArray

  def n2(i : Int, j: Int) = 
      List((i-1,j-1),(i-1,j),(i-1,j+1),(i+1,j-1),(i+1,j),(i+1,j+1),(i,j-1),(i,j+1)).filter{
        case (a,b) => a >= 0 && b >= 0 && a < grid.length && b < grid(0).length
      }
  def neighbors(i : Int, j : Int) = 
    n2(i,j).map(x => grid(x._1)(x._2))

  def flash = 
    var flashes = 0
    for i <- 0 until grid.length
        j <- 0 until grid(i).length 
    do grid(i)(j) += 1
      while grid.exists(_.exists(_ >= 10)) do                  // while stuff hasnt flashed
        for i <- 0 until grid.length
            j <- 0 until grid(i).length if grid(i)(j) >= 10 do // go through everything that should be flashed
              flashes += 1
              grid(i)(j) = 0                                   // it resets
              for (a,b) <- n2(i,j) if grid(a)(b) != 0 do       // increment not already flashed neighbors
                grid(a)(b) += 1
    flashes
  
  def run = 
    (1 to 100).map(_ => flash).sum

  def run2 =  
    var times = 0    
    while grid.flatten.toSet.size > 1 do
      flash
      times += 1
    times