object Day2:
  // pt1 sol to be recreated
  def parse(lines : IndexedSeq[String]) = 
    lines.map { line =>
      val Array(a,b) = line.split(" ")
      (a, b.toInt)
    }
    
  def run(lines : IndexedSeq[String]) =
    var x,y = 0
    for (cmd, n) <- parse(lines) do
      cmd match
        case "up"   => 		y -= n
        case "down" => 		y += n
        case "forward" => x += n
    x * y
    
  def run2(lines : IndexedSeq[String]) =
    var x,y,aim = 0
    for (cmd, n) <- parse(lines) do
      cmd match
        case "up"   => aim -= n
        case "down" => aim += n
        case "forward" =>
          x += n
          y += n * aim
    x * y