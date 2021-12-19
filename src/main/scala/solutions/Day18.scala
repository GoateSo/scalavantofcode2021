package solutions
import solutions.Solution

/**
 * Parse to Pair ADT and apply operations on that parsed object
 */
class Day18 (lines : IndexedSeq[String]) extends Solution(lines):
  enum SnailNum:
    case V(value : Int)
    case Pair(left : SnailNum, right : SnailNum)
  import SnailNum._

  private def commaSplit(input : IndexedSeq[Char]): (IndexedSeq[Char], IndexedSeq[Char]) =
    var left, right, k = 0
    var i = 0
    for c <- input do
      i += 1
      c match 
        case '[' => left += 1
        case ']' => right += 1
        case ',' if left == right => k = i
        case _ =>()
    input.splitAt(k)

  private def parse(input : IndexedSeq[Char]): SnailNum =
    if input.head != '[' then
      V(input.takeWhile(_.isDigit).mkString.toInt)
    else
      val (left :+ _ , right :+ _) = commaSplit(input.tail)
      Pair(parse(left), parse(right))
      
  private def splits(num : SnailNum, hasSplit : Boolean = false): (SnailNum, Boolean) = 
    num match 
      case V(v) if v < 10 || hasSplit  => (V(v), hasSplit)
      case V(v)              => (Pair(V(v/2), V(v/2 + v%2)), true)
      case Pair(left, right) => 
        val (lef, sl) = splits(left,hasSplit)
        val (rit, sr) = splits(right, sl)
        (Pair(lef, rit), sr)

  private def canSplit(num : SnailNum): Boolean = 
    num match 
      case V(v)      => v > 9
      case Pair(l,r) => canSplit(l) || canSplit(r)

  private def addRight(num : SnailNum, adder : Int) : SnailNum = num match
    case V(x)      => V(x+adder)
    case Pair(l,r) => Pair(l,addRight(r, adder))

  private def addLeft(num : SnailNum, adder : Int) : SnailNum = num match
    case V(x)      => V(x+adder)
    case Pair(l,r) => Pair(addLeft(l, adder),r)

  private def explode(
    num : SnailNum, 
    depth : Int = 0, 
    exploded : Boolean = false, 
    leftAdder : Int = 0, 
    rightAdder : Int = 0
  ): (SnailNum, Int, Int, Boolean) =
    num match
      case sn @ V(v) => (sn,0,0,exploded)

      case Pair(l,r) if depth >= 3 && !exploded => 
        var bol = true 
        val (np,ladder,radder) = (l,r) match 
          case (Pair(V(a), V(b)), V(v)) => (Pair(V(0), V(b + v)), a, 0)
          case (V(v), Pair(V(a), V(b))) => (Pair(V(v + a), V(0)), 0, b)
          case (Pair(V(a), V(b)), Pair(V(c), V(d))) => (Pair(V(0), Pair(V(c + b), V(d))), a, 0)
          case (a,b) => 
            bol = false
            (Pair(a,b),0,0)
        (np, ladder, radder, bol)

      case Pair(l,r) if depth < 3 => 
        var (left, lef, rite, expl) = explode(l, depth+1, exploded)
        var (right, lef1, rite1, expl2) = explode(r, depth+1, expl)
        var (left1, ladder) = left match
          case V(x) if lef > 0   => (V(x+lef), 0)
          case _ if lef1 > 0     => (addRight(left, lef1), 0)
          case _                 => (left, lef + lef1)
        
        var (right1, radder) = right match
          case V(x) if rite1 > 0 => (V(x+rite1), 0)
          case _ if rite > 0     => (addLeft(right, rite), 0)
          case _                 => (right, rite + rite1)
        (Pair(left1,right1), ladder, radder , expl2)

      case x => 
        (x, leftAdder, rightAdder, exploded)

  private def reduce(num : SnailNum): SnailNum = 
    var temp = num
    var prev = V(1)
    while (prev != temp || canSplit(temp)) do
      while temp != prev do
        prev = temp
        temp = explode(temp)._1
      if canSplit(temp) then
        temp = splits(temp)._1
    temp

  private def magnitude(num : SnailNum) : Int = 
    num match 
      case V(x) => x
      case Pair(l,r) => 3 * magnitude(l) + 2 * magnitude(r)

  private val exprs = lines.map(x => parse(x.toIndexedSeq))
  val run =
    magnitude(exprs.reduce((a,b) => reduce(Pair(a,b)))).toString

  val run2 = 
    val sums = for
      x1 <- exprs
      x2 <- exprs
    yield magnitude(reduce(Pair(x1,x2)))
    sums.max.toString