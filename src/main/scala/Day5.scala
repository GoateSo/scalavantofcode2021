class Day5(lines : IndexedSeq[String]):
  private val r = """(-?\d+),(-?\d+) -> (-?\d+),(-?\d+)""".r 
  private val inputs = lines.map {case r(x,y,a,b) => (x.toInt,y.toInt,a.toInt,b.toInt)}
  private val map = Array.fill(1000,1000)(0)

  private def comp(x : Int, y : Int, a : Int, b : Int) = a==x || y==b

  private def fillWith(xs : IndexedSeq[(Int,Int,Int,Int)]) = 
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

  // implementation relies on running run2 after run
  lazy val run =
    fillWith(inputs.filter(comp)).flatten.count(_ > 1)

  lazy val run2 = 
    fillWith(inputs.filterNot(comp)).flatten.count(_ > 1)
