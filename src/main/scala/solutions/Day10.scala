package solutions
import scala.collection.mutable.Stack

/**
 * parsing each line and determining failure location or needed complement
 */
class Day10(lines : IndexedSeq[String]) extends Solution(lines): 

  private val comp = Map('(' -> ')','[' -> ']','{' -> '}','<' -> '>')
  private val sc = Map(')' -> 1,']' -> 2,'}' -> 3,'>' -> 4)
  private val scores = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  private def validParen(str : String) : (Char,Stack[Char]) = 
    var stk = Stack[Char]()
    for c <- str do
      if comp.contains(c) then 
        stk.push(c)
      if sc.contains(c) && c != comp(stk.pop) then
        return (c,stk)
    (if stk.isEmpty then 'p' else 'a', stk)


  val run = 
    // sum of scores
    lines.map(s => 
      scores.getOrElse(validParen(s)._1, 0)
    ).sum.toString

  val run2 =
    // get complements needed and tally up scores
    val xs = lines map validParen filter (_._1 == 'a') map (_._2)
    val ys = xs map (_.foldLeft(0l)((acc,c) => acc * 5 + sc(comp(c))))
    ys.sorted.apply(ys.length/2).toString