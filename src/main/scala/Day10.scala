class Day10(lines : IndexedSeq[String]): 
  import scala.collection.mutable.Stack

  val comp = Map('(' -> ')','[' -> ']','{' -> '}','<' -> '>')
  val sc = Map(')' -> 1,']' -> 2,'}' -> 3,'>' -> 4)
  val scores = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  def validParen(str : String) : (Char,Stack[Char]) = 
    var stk = Stack[Char]()
    for c <- str do
      if comp.contains(c) then 
        stk.push(c)
      if sc.contains(c) && c != comp(stk.pop) then
        return (c,stk)
    (if stk.isEmpty then 'p' else 'a', stk)


  def run = 
    // sum of scores
    lines.map(s => 
      scores.getOrElse(validParen(s)._1, 0)
    ).sum

  def run2 =
    // get complements needed
    val xs = lines map validParen filter (_._1 == 'a') map (_._2)
    // find scores of complements
    val ys = xs map (_.foldLeft(0l)((acc,c) => acc * 5 + sc(comp(c))))
    // return median
    ys.sorted.apply(ys.length/2)