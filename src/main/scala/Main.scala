import java.io._
import scala.io._

var lines = Source.fromFile("""input.txt""").getLines.toIndexedSeq

@main def hello: Unit =
	println(Day11.run(lines))
	println(Day11.run2(lines))