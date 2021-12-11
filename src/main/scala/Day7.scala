object Day7:
  def parse(lines : IndexedSeq[String]) = 
    lines.head.split(",").map(_.toInt)
    
  def solv(xs : Array[Int], cost : Int => Int) =
    val mi = xs.min
    val ma = xs.max
    (mi to ma).map { i =>
      xs.map {x => 
        cost(Math.abs(i-x))
      }.sum 
    }.min      
  
  def run(lines : IndexedSeq[String]) = 
    solv(parse(lines), identity)

  def run2(lines : IndexedSeq[String]) =
    solv(parse(lines), n => n * (n + 1) / 2)