package solutions
import scala.collection.mutable.HashMap


/**
 * simulate rounds of polymerization by keeping track of pair and individual character occurences
 */
class Day14(lines : IndexedSeq[String]) extends Solution(lines):
  var (str, _ +: pairs) = (lines.head,lines.tail)

  val hm = HashMap[String,String]()
  var ocs = HashMap[String,BigInt]()
  var ocs2 = HashMap[Char,BigInt]()

  // set up auxilliary data sturctures
  for Array(p,c) <- pairs.map(_.split(" -> ")) do
    hm(p) = c
    ocs(p) = 0
  for c <- 'A' to 'Z' do
    ocs2(c) = 0
  
  // get occurences of pairs
  str.sliding(2).foreach(ocs(_) += 1)
  // get occurrences of characters
  str.foreach(ocs2(_) += 1)

  private def polymerize = 
    // get copy of pair occurences and refresh all values to 0
    val cpy = ocs.clone.map((a,_) => (a,BigInt(0)))
    // go through all pairs with values
    for (pair, ocr) <- ocs if ocr != 0 do
      // find value to insert and add its occurences
      val between = hm(pair)
      ocs2(between(0)) += ocr
      // find new pairs formed and add their occurences
      cpy(pair(0) + between) += ocr
      cpy(between + pair(1)) += ocr
    ocs = cpy


  // nonlazy as it relies on solving pt1 before pt2
  // solve part 1, 
  val run = 
    for _ <- 1 to 10 do
      polymerize
    val ns = ocs2.toList.map(_._2).filter(_>0)
    (ns.max - ns.min).toString

  // solve part 2
  val run2 =
    for _ <- 1 to 30 do
      polymerize
    val ns = ocs2.toList.map(_._2).filter(_>0)
    (ns.max - ns.min).toString

