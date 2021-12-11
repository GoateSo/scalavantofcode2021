object Day5:
  val r = """(-?\d+),(-?\d+) -> (-?\d+),(-?\d+)""".r 
  
  def parse(lines : IndexedSeq[String]) = 
    lines.map {case r(x,y,a,b) => (x.toInt,y.toInt,a.toInt,b.toInt)}

  def fillWith(xs : IndexedSeq[(Int,Int,Int,Int)]) = 
    val map = Array.fill(1000,1000)(0)
    for ps <- xs do
      var (x,y,a,b) = ps
      var dx = (a-x).sign
      var dy = (b-y).sign
      while 
        val cond = x != a || y != b
        map(x)(y) += 1
        x += dx
        y += dy 
        cond
      do {} 
    map
  def run(lines : IndexedSeq[String]) =
    fillWith(parse(lines).filter((x,y,a,b) => a==x || y==b)).flatten.count(_ > 1)

  def run2(lines : IndexedSeq[String]) = 
    fillWith(parse(lines)).flatten.count(_ > 1)
