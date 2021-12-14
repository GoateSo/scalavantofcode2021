import java.io._
import scala.io._

var lines = Source.fromFile("""input.txt""").getLines.toIndexedSeq
import Utils.print
@main def hello: Unit =
  val c = Day4(lines)
  print(c.run, c.run2)