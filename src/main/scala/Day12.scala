import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

class Day12(lines : IndexedSeq[String]):
  val alph = "[a-zA-z]"
  val r = s"($alph+)-($alph+)".r

  val adjList = HashMap[String,ArrayBuffer[String]]()
  for Array(a,b) <- lines.map(_.split("-")) do
    adjList.getOrElseUpdate(a,ArrayBuffer[String]()) += b
    adjList.getOrElseUpdate(b,ArrayBuffer[String]()) += a
  
  def dfs(v : String, visited : HashMap[String, Int], used : Boolean) : Int = 
    if (v == "end") 1
    else
      if (v(0).isLower) visited(v) += 1
      var n = 0
      for w <- adjList(v) do
        val op = visited(w) == 1 && !used
        if visited(w) < 1 || op then
          n += dfs(w,visited, used || op)
      visited(v) -= 1
      n

  def mkMap = 
    val map = HashMap[String,Int]()
    for (node,_) <- adjList do
      map(node) = 0
    map("start") = 2
    map

  def run = dfs("start",mkMap, true)
  def run2 = dfs("start", mkMap, false)
