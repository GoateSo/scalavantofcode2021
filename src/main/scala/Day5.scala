import scala.annotation.meta.companionClass
class Day5(lines : IndexedSeq[String]):
  val r = """(-?\d+),(-?\d+) -> (-?\d+),(-?\d+)""".r 
  val inputs = lines.map {case r(x,y,a,b) => (x.toInt,y.toInt,a.toInt,b.toInt)}
  val map = Array.fill(1000,1000)(0)

  def comp(x : Int, y : Int, a : Int, b : Int) = a==x || y==b

  def fillWith(xs : IndexedSeq[(Int,Int,Int,Int)]) = 
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

  def run =
    fillWith(inputs.filter(comp)).flatten.count(_ > 1)

  def run2 = 
    fillWith(inputs.filterNot(comp)).flatten.count(_ > 1)
