package solutions
import scala.collection.mutable.HashMap

class Day8(lines : IndexedSeq[String]) extends Solution(lines):
  private val inputs = 
    lines.map(_.split(" \\| ").map(_.trim.split(" ").toList))

  private def commons(a : String, b : Int)(using xs : HashMap[Int,Set[Char]]) =
    (a.toSet & xs(b)).size

  private def find(p : String => Boolean)(using xs : List[String]) : Set[Char] =
    xs.find(p).get.toSet

  private def len(len : Int*) =
    (x : String) => len.toSet(x.length)

  val run = 
    inputs.map(_(1).filter(len(2,3,4,7)).length).sum.toString

  val run2 =
    (for Array(left, right) <- inputs yield
      //val Array(left,right)= line.split(" \\| ").map(_.trim.split(" ").toList)
      given zs : List[String] = left
      given xs : HashMap[Int, Set[Char]] = HashMap(
        1 -> find(len(2)),
        7 -> find(len(3)),
        4 -> find(len(4)),
        8 -> find(len(7))
      )
      // 9,0,6 (length 6) to account for
      var six = zs.filter(len(6))
      xs(9) = find(commons(_,4) == 4)(using six)
      xs(0) = find(s => s.toSet != xs(9) && commons(s,7) == 3)(using six)
      xs(6) = find(s => s.toSet != xs(9) && s.toSet != xs(0))(using six)
      // 2,3,5 (length 5) to account for
      var five = zs.filter(len(5))
      xs(2) = find(commons(_,4) == 2)(using five)
      xs(3) = find(commons(_,2) == 4)(using five)
      xs(5) = find(commons(_,2) == 3)(using five)

      // convert to a Set-Int mapping 
      val ys = xs map {(a,b) => (b,a)}
      // convert 4 digit values to 4 digit ints
      right.map(x => ys(x.toSet)).mkString.toInt
    ).sum.toString