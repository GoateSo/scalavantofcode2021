object Day4:
  import scala.collection.mutable.HashSet
  import scala.collection.mutable.ArrayBuffer
  import Utils.columns

  def parse (lines : IndexedSeq[String]) = 
     (lines.head.split(",").map(_.toInt),
      lines.tail.filter(_.length > 0)     // filter empty lines
      .sliding(5,5).toSeq       // take 5 lines at a time and convert back to sequences
      .map(x =>                     // for each line:
        x.map(_.trim.split("\\s+")  // trim spaces on either side and split by whitespace
          .map(_.toInt).toSeq       // convert to integers
        )
      ))

  def isFilled(board : Seq[Seq[Int]])(using set : HashSet[Int]) = // checking whether there is a bingo
      board.exists(_.filter(set.contains).length == 5)            // rows filled?
      || board.columns.exists(_.filter(set.contains).length == 5) // colums filled?

  def scores(lines : IndexedSeq[String])=
    var (nums,boards) = parse(lines)
    given set : HashSet[Int] = HashSet[Int]()                    //set for existing numbers
    val xs = ArrayBuffer[Int]()
    for n <- nums do
      set.add(n)
      val bord = boards.find(isFilled)        // check for filled boards (might be superfluous idc)
      boards = boards.filterNot(isFilled)     // remove already finished boards 

      if bord != None then 
        xs += n * (bord.get.flatten.filter(!set.contains(_)).sum) // print scores
    xs

  // solution for part 1; slower than necessary since it evaluates all boards before running
  def run(lines : IndexedSeq[String]) = 
    scores(lines).head

  // solution for part 2
  def run2(lines : IndexedSeq[String]) =
    scores(lines).last