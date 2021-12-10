object Day2:
  // pt1 sol to be recreated
  def run2(lines : IndexedSeq[String]) =
    var x = 0
    var y = 0
    var aim = 0
    for str <- lines do
      val Array(cmd, num) = str.split(" ")
      val n = num.toInt
      cmd match
        case "up"   => aim -= n
        case "down" => aim += n
        case "forward" =>
          x += n
          y += n * aim
    println(x * y)