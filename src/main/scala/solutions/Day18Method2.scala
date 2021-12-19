package solutions
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

/**
 * parse to flat array instead of mapping to actual tree
 * inspired by https://www.reddit.com/r/adventofcode/comments/rizw2c/2021_day_18_solutions/hp3s479/
 */
class Day18Method2(lines : IndexedSeq[String]) extends Solution(lines): 
  var depths, nums = ArrayBuffer[Int]()

  private def parse(input : List[Char], depth : Int = 0): Unit = input match
    case Nil             => ()
    case ',' :: rest     => parse(rest, depth)
    case '[' :: rest     => parse(rest, depth + 1)
    case ']' :: rest     => parse(rest, depth - 1)
    case num :: rest     => 
      val (num2, remainder) = rest.span(_.isDigit)
      nums += (num :: num2).mkString.toInt
      depths += depth
      parse(remainder, depth)
      
  private def explode(): Unit = 
    var i = -1
    while i < nums.size-1 do 
      i += 1
      if depths(i) == 5 then
        if i > 0 then 
          nums(i-1) += nums(i)
        if i < nums.size-2 then
          nums(i+2) += nums(i+1)
        nums(i) = 0
        depths(i) -= 1
        depths.remove(i+1)
        nums.remove(i+1)
        i -= 1

  private def split(): Unit = 
    var i = -1
    var canRun = true
    while i < nums.size-1 && canRun do
      i += 1
      if nums(i) > 9 then
        canRun = false
        val n = nums(i)
        nums(i) -= n/2
        depths(i) += 1
        depths.insert(i,depths(i))
        nums.insert(i, n/2)

  private def add(num : String) = 
    depths.mapInPlace(_ + 1)
    parse(num.toList, 1)

  private def score = 
    val ds = depths.clone
    val vs = nums.clone
    while vs.size > 1 do
      breakable {
        for i <- 0 until vs.length do
          if ds(i) == ds(i+1) then
            vs(i) = 3*vs(i) + 2*vs(i+1)
            vs.remove(i+1)
            ds.remove(i+1)
            if ds(i) > 0 then
              ds(i) -= 1
            break
      }
    vs.head
      
  private def reduce() = 
    while (nums.exists(_ > 9) || depths.exists(_ > 4)) do
      explode()
      split()

  val (a +: rest) = lines
  parse(a.toList)
  for x <- rest do
    add(x)
    reduce()
  val run = score.toString
  val run2 =
    var maxVal = 0
    for 
      x1 <- lines
      x2 <- lines
    do
      nums = ArrayBuffer()
      depths = ArrayBuffer()
      parse(x1.toList)
      add(x2)
      reduce()
      maxVal = Math.max(maxVal, score)
    maxVal.toString