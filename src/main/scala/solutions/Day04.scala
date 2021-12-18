package solutions
import scala.collection.mutable.{HashSet, ArrayBuffer}
import utils.Utils.columns

class Day4(lines : IndexedSeq[String]) extends Solution(lines):
  private val nums = lines.head.split(",").map(_.toInt)
  private val scores = ArrayBuffer[Int]()     
  private val set = HashSet[Int]() //set for existing numbers
  private var boards =
    lines.tail.filter(_.length > 0)     // filter empty lines
    .sliding(5,5).toSeq           // take 5 lines at a time and convert back to sequences
    .map(x =>                     // for each line:
      x.map(_.trim.split("\\s+")  // trim spaces on either side and split by whitespace
        .map(_.toInt).toSeq       // convert to integers
      )
    )

  private def isFilled(board : Seq[Seq[Int]]) = // checking whether there is a bingo
    board.exists(_.filter(set.contains).length == 5)            // rows filled?
    || board.columns.exists(_.filter(set.contains).length == 5) // colums filled?
    
  for n <- nums do
    set += n
    val bord = boards.find(isFilled)        // check for filled boards (might be superfluous idc)
    boards = boards.filterNot(isFilled)     // remove already finished boards 
    if bord != None then 
      scores += n * (bord.get.flatten.filterNot(set.contains).sum) // print scores

  // solution for part 1
  val run = scores.head.toString

  // solution for part 2
  val run2 = scores.last.toString