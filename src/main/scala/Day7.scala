class Day7(lines : IndexedSeq[String]):
  val xs = lines.head.split(",").map(_.toInt)
    
  def solv(cost : Int => Int) =
    val mi = xs.min
    val ma = xs.max
    (mi to ma).map { i =>
      xs.map {x => 
        cost(Math.abs(i-x))
      }.sum 
    }.min      
  
  def run = 
    solv(identity)

  def run2 =
    solv(n => n * (n + 1) / 2)