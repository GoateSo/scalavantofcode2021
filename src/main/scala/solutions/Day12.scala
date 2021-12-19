package solutions
import scala.collection.mutable.{Buffer, HashMap, HashSet}

/**
 * DFS with different blocked path critera and while removing visited node 
 */
class Day12(lines : IndexedSeq[String]) extends Solution(lines):
  private val graph = HashMap[String,Buffer[String]]()

  // initlize adjacency list
  for Array(a,b) <- lines.map(_.split("-")) do
    graph.getOrElseUpdate(a,Buffer()) += b
    graph.getOrElseUpdate(b,Buffer()) += a
  
  // depth first traversal for finding all paths
  private def dfs(v : String, visited : HashMap[String, Int], used : Boolean) : Int = 
    // if at end; return successful path
    if (v == "end") 1
    else
      // increment visits if lowercase
      if (v(0).isLower) visited(v) += 1
      var n = 0
      for w <- graph(v) do
        // if its able to be used again (pt 2)
        val op = visited(w) == 1 && !used
        if visited(w) < 1 || op then
          n += dfs(w,visited, used || op)
      visited(v) -= 1
      n

  private def mkMap = 
    val map = HashMap[String,Int]()
    for (node,_) <- graph do
      map(node) = 0
    map("start") = 2
    map

  // visit each small cave once and only once
  val run = dfs("start", mkMap, true).toString

  // allowed to go to 1 small cave twice
  val run2 = dfs("start", mkMap, false).toString
