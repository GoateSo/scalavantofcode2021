package solutions

/**
 * simulate all plausible trajectories and get the max Y / number of trajectories
 */
class Day17(lines : IndexedSeq[String]) extends Solution(lines):
  val inp = raw"x=(\d+)..(\d+), y=(-\d+)..(-\d+)".r

  val inp(xa, xb, ya, yb) = lines(0).substring(13)
  val (x1, x2, y2, y1) = (xa.toInt, xb.toInt, ya.toInt, yb.toInt)

  private def cantReach(dx : Int): Boolean =
    dx * (dx + 1) / 2 < x1

  // check whether it hit the target  
  private def hit(x : Int, y : Int) = 
    x >= x1 && x <= x2 && y <= y1 && y >= y2

  // simulate trajectory for given velocity
  private def simulate(dxs : Int, dys : Int) =
    var x,y, maxy = 0
    var dx = dxs
    var dy = dys
    var hitd = false
    while !hitd && x <= x2 && y >= y2 do
      x += dx
      y += dy
      maxy = Math.max(maxy, y)
      hitd ||= hit(x, y)
      dx = if dx != 0 then dx - 1 * dx.sign else 0
      dy -= 1

    (hitd, maxy)

  // find all possible starting slopes
  private val xs = for 
    candidate <- (1 to x2).dropWhile(cantReach)
    ycand <- y2 to -y2
  yield simulate(candidate, ycand)

  val hits = xs.filter(_._1).map(_._2)

  val run = hits.max.toString

  val run2 = hits.size.toString