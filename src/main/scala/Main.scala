import java.io._
import scala.io._

import utils.Utils.print
import solutions._

@main def main: Unit =
  for i <- 17 until 18 do
    var lines = 
      Source.fromFile(s"inputs\\input${i+1}.txt")
        .getLines
        .toIndexedSeq

    val solution = Solutions.solList(i)(lines)

    print(s"solution to problem ${i+1} is:")
    print(s"part 1: ${solution.run}")
    print(s"part 2: ${solution.run2}\n")

