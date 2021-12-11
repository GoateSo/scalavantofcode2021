object Day9:
  import scala.collection.mutable.HashSet
  import scala.collection.mutable.ListBuffer
  type ISeq[T] = IndexedSeq[T]

  // get neighbors of (i,j) with List of 4 directions and a bound check
  def n2(i : Int, j: Int)(using arr : ISeq[ISeq[Char]]) = 
      List((i-1,j),(i+1,j),(i,j-1),(i,j+1)) filter {
        case (a,b) => a >= 0 && b >= 0 && a < arr.length && b < arr(0).length
      }

  // get neighbors of (i,j) and map to array values 
  def neighbors(i : Int, j : Int)(using arr : ISeq[ISeq[Char]]) = 
    n2(i,j).map(x => arr(x._1)(x._2))

  // performs depth first search starting at (i,j), returning the number of nodes encountered, stopped at arr(i)(i) = 9
  def dfs (i : Int, j : Int, visited : HashSet[(Int,Int)])(using arr : ISeq[ISeq[Char]]) : Int = 
    if (visited.contains((i,j))) 
      then 0
      else 
        visited += ((i,j))
        val n =  n2(i,j).filter(x => arr(x._1)(x._2) != '9')
        1 + n.map(x => dfs(x._1,x._2,visited)).sum

  // part 1 solution; find all minimums and sum their risks (value + 1)
  def run(lines : IndexedSeq[String]) = 
    given arr : ISeq[ISeq[Char]] = lines.map(_.toIndexedSeq)
    val xs = for 
      i <- 0 until arr.length
      j <- 0 until arr(i).length if neighbors(i,j).forall(_ > arr(i)(j))
    yield arr(i)(j) - '0' + 1
    xs.sum

  // part 2 solution; product of the sizes of largest 3 basins
  def run2(lines : IndexedSeq[String]) =
    given arr : ISeq[ISeq[Char]] = lines.map(_.toIndexedSeq)
    val xs = for 
      i <- 0 until arr.length
      j <- 0 until arr(i).length if neighbors(i,j).forall(_ > arr(i)(j))
    yield (i,j)
    xs.map(x => dfs(x._1,x._2,HashSet.empty[(Int,Int)])).sortWith(_ > _).take(3).product
