import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
class Day12(lines : IndexedSeq[String]):
  val r = "([a-zA-z]+)-([a-zA-z]+)".r

  val adjList = HashMap[String,ArrayBuffer[String]]()
  for r(a,b) <- lines do
    adjList.getOrElseUpdate(a,ArrayBuffer[String]()) += b
    adjList.getOrElseUpdate(b,ArrayBuffer[String]()) += a

  def dfs1(v : String, visited : HashSet[String]) : Int = if v == "end" then 1
    else 
      if v(0).isLower then visited += v
      var n = 0
      for w <- adjList(v) if !visited(w) do 
        n += dfs1(w,visited)
      visited -= v
      n
  
  def dfs2(v : String, visited : HashMap[String, Int], used : Boolean) : Int = if v == "end" then 1
    else
      if v(0).isLower then visited(v) += 1
      var n = 0
      for w <- adjList(v) do
        val op = visited(w) == 1 && !used
        if visited(w) < 1 || op then
          n += dfs2(w,visited, used || op)
      visited(v) -= 1
      n

  def run = dfs1("start",HashSet())
  def run2 = 
    val map = HashMap[String,Int]()
    for (node,_) <- adjList do
      map(node) = 0
    map("start") = 2
    dfs2("start", map, false)

