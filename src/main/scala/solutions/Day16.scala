package solutions
import utils.Utils._
import scala.collection.mutable.ListBuffer

/**
 * parse packets recursively and operate on them
 */
class Day16(lines : IndexedSeq[String]) extends Solution(lines):
  case class ParseResult(v : Long, version : Int, remaining : List[Char])

  private def parseLiteral(chrs : List[Char])(using version : Int) : ParseResult =
    var litVal = StringBuilder()
    var (lit, rest) = chrs.splitAt(5)
    while lit(0) != '0' do
      litVal ++= lit.drop(1)
      val (x, y) = rest.splitAt(5)
      lit = x
      rest = y
    litVal ++= lit.drop(1)
    ParseResult(litVal.toString.toLong(2),version,rest)

  private def parse(chrs : List[Char]) : ParseResult =
    val (ver, xs) = chrs.splitAt(3)
    val (typ, ys) = xs.splitAt(3)
    val itype = typ.mkString.toInt(2)
    
    given version : Int = ver.mkString.toInt(2)
    if itype == 4 then
      parseLiteral(ys)  
    else 
      val (lenTyp :: rest) = ys
      val operands = ListBuffer[Long]()
      var versionSum = 0
      var remainder = if lenTyp == '1' then 
        // 11 bit length segment (n) followed by n operators
        var (len, rest2) = rest.splitAt(11)
        for i <- 1 to len.mkString.toInt(2) do
          val ParseResult(v, ver, y) = parse(rest2)
          versionSum += ver
          operands += v
          rest2 = y
        rest2
      else  // 0
        // 15 bit length segment (n) parsing all subpackets in n chars
        val (len, rest2) = rest.splitAt(15)
        var (op, leftover) = rest2.splitAt(len.mkString.toInt(2))
        while op.size > 7 do
          val ParseResult(v,ver,next) = parse(op)
          versionSum += ver
          operands += v
          op = next
        leftover

      val ret = itype match 
        case 0 => operands.sum
        case 1 => operands.product
        case 2 => operands.min
        case 3 => operands.max
        case 5 => if operands(0) > operands(1) then 1l else 0l
        case 6 => if operands(0) < operands(1) then 1l else 0l
        case 7 => if operands(0) == operands(1) then 1l else 0l
      ParseResult(ret, version + versionSum , remainder)

  private var binstr =
    lines(0)
      .map(_.toString.toInt(16).toBin.toInt)
      .map(x => f"$x%04d")
      .mkString

  val ParseResult(result, versionSum,  _) = parse(binstr.toList)

  val run = versionSum.toString

  val run2 = result.toString