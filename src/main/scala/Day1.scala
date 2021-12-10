object Day1:
  /**
   * solution for pt 1 of day 1
   */
  def run(lines : IndexedSeq[String]) =
    lines.map(_.toInt)
         .sliding(2)
         .toList
         .filter {
            case Seq(a, b) if a < b => true
            case _ => false
         }.length
  /**
   * solution for pt 2 of day 1
   */
  def run2(lines : IndexedSeq[String]) = 
    lines
      .map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .toList
      .sliding(2)
      .filter {
        case List(a, b) if a < b => true
        case _                   => false
      }
      .length
