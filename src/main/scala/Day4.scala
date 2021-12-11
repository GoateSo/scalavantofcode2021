class Day4(lines : IndexedSeq[String]):
  import scala.collection.mutable.HashSet
  import scala.collection.mutable.ArrayBuffer
  import Utils.columns

  val nums = lines.head.split(",").map(_.toInt)
  var boards =
    lines.tail.filter(_.length > 0)     // filter empty lines
    .sliding(5,5).toSeq           // take 5 lines at a time and convert back to sequences
    .map(x =>                     // for each line:
      x.map(_.trim.split("\\s+")  // trim spaces on either side and split by whitespace
        .map(_.toInt).toSeq       // convert to integers
      )
    )

  val set = HashSet[Int]() //set for existing numbers

  def isFilled(board : Seq[Seq[Int]]) = // checking whether there is a bingo
      board.exists(_.filter(set.contains).length == 5)            // rows filled?
      || board.columns.exists(_.filter(set.contains).length == 5) // colums filled?

  lazy val scores = {                 
    val xs = ArrayBuffer[Int]()
    for n <- nums do
      set.add(n)
      val bord = boards.find(isFilled)        // check for filled boards (might be superfluous idc)
      boards = boards.filterNot(isFilled)     // remove already finished boards 
      if bord != None then 
        xs += n * (bord.get.flatten.filter(!set.contains(_)).sum) // print scores
    xs
  }

  // solution for part 1
  def run = scores.head

  // solution for part 2
  def run2 = scores.last