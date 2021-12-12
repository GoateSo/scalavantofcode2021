class Day9(lines : IndexedSeq[String]):
  import scala.collection.mutable.HashSet
  import scala.collection.mutable.ListBuffer
  type ISeq[T] = IndexedSeq[T]
  private val arr : ISeq[ISeq[Char]] = lines.map(_.toIndexedSeq)

  private val xs = for 
    i <- 0 until arr.length
    j <- 0 until arr(i).length if neighbors(i,j).forall(_ > arr(i)(j))
  yield (i,j)

  // get neighbors of (i,j) with List of 4 directions and a bound check
  private def n2(i : Int, j: Int) = 
      List((i-1,j),(i+1,j),(i,j-1),(i,j+1)) filter {
        case (a,b) => a >= 0 && b >= 0 && a < arr.length && b < arr(0).length
      }

  // get neighbors of (i,j) and map to array values 
  private def neighbors(i : Int, j : Int) = 
    n2(i,j).map(x => arr(x._1)(x._2))

  // performs depth first search starting at (i,j), returning the number of nodes encountered, stopped at arr(i)(i) = 9
  private def dfs (i : Int, j : Int, visited : HashSet[(Int,Int)]) : Int = 
    if (visited.contains((i,j))) 
      then 0
      else 
        visited += ((i,j))
        val n =  n2(i,j).filter(x => arr(x._1)(x._2) != '9')
        1 + n.map(x => dfs(x._1,x._2,visited)).sum

  // part 1 solution; find all minimums and sum their risks (value + 1)
  def run = 
    xs.map((a,b) => arr(a)(b) - '0' + 1).sum

  // part 2 solution; product of the sizes of largest 3 basins
  def run2 =
    xs.map(x => 
      dfs(x._1,x._2,HashSet.empty[(Int,Int)]))
        .sortWith(_ > _)
        .take(3).product
