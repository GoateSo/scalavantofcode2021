package solutions
class Day2(lines : IndexedSeq[String]) extends Solution(lines):
  private val inputs = lines.map { line =>
    val Array(a,b) = line.split(" ")
    (a, b.toInt)
  }
    
  val run =
    var x,y = 0
    for (cmd, n) <- inputs do
      cmd match
        case "up"   => 		y -= n
        case "down" => 		y += n
        case "forward" => x += n
    (x * y).toString
    
  val run2 =
    var x,y,aim = 0
    for (cmd, n) <- inputs do
      cmd match
        case "up"   => aim -= n
        case "down" => aim += n
        case "forward" =>
          x += n
          y += n * aim
    (x * y).toString