package solutions
import scala.collection.mutable.HashSet

class Day9(lines : IndexedSeq[String]) extends Solution(lines):
  type ISeq[T] = IndexedSeq[T]
  
  private val arr : ISeq[ISeq[Char]] = lines.map(_.toIndexedSeq)

  private val xs = for 
    i <- 0 until arr.size
    j <- 0 until arr(i).size if neighbors(i,j).forall(_ > arr(i)(j))
  yield (i,j)

  // get neighbors of (i,j) with List of 4 directions and a bound check
  private def n2(i : Int, j: Int) = 
      List((i-1,j),(i+1,j),(i,j-1),(i,j+1)) filter {(a,b) => 
        a >= 0 && b >= 0 && a < arr.size && b < arr(0).size
      }

  // get neighbors of (i,j) and map to array values 
  private def neighbors(i : Int, j : Int) = 
    n2(i,j).map(arr(_)(_))

  // performs depth first search starting at (i,j), returning the number of nodes encountered, stopped at arr(i)(i) = 9
  private def dfs (i : Int, j : Int, visited : HashSet[(Int,Int)]) : Int = 
    if visited(i,j) then 0
      else 
        visited += ((i,j))
        val n = n2(i,j).filter(arr(_)(_) != '9')
        1 + n.map(dfs(_,_,visited)).sum

  // part 1 solution; find all minimums and sum their risks (value + 1)
  val run = 
    xs.map(arr(_)(_) - '0' + 1).sum.toString

  // part 2 solution; product of the sizes of largest 3 basins
  val run2 =
    xs.map(dfs(_,_,HashSet.empty))
        .sortWith(_ > _)
        .take(3).product.toString
