package utils
import scala.util._
import scala.math._
import scala.util.matching._
import scala.util.matching.Regex.Match
import scala.collection.mutable.{ArrayBuffer, HashMap}

/** collection of useful utility methods & shorthands
  */
object Utils:
  val alph = "[a-zA-Z]".r

  extension (n: Double) def **(m: Double) = Math.pow(n, m)

  extension (str: String)
    def apply(start: Int, end: Int) = str.substring(start, end)
    def toLong = java.lang.Long.parseLong(str)
    def toLong(radix: Int) = java.lang.Long.parseLong(str, radix)
    def toInt = Integer.parseInt(str)
    def toInt(radix: Int) = Integer.parseInt(str, radix)
    def gsub(reg: Regex, f: Seq[String] => String) =
      reg.replaceAllIn(
        str,
        _ match {
          case reg(xs*) => f(xs)
        }
      )
    def gsub(reg: Regex, f: Seq[String] => String, times: Int) =
      var occurs = 0
      reg.replaceSomeIn(
        str,
        _ match {
          case reg(xs*) if occurs < times =>
            occurs += 1
            Some(f(xs))
          case _ => None
        }
      )
    def gsub(reg: Regex, rep: String) =
      reg.replaceAllIn(str, rep)
    def gsub(reg: Regex, rep: String, times: Int) =
      var occurs = 0
      reg.replaceSomeIn(
        str,
        _ match {
          case reg(_*) if occurs < times =>
            occurs += 1
            Some(rep)
          case _ => None
        }
      )

  extension [T](grid: Seq[Seq[T]])
    def columns: Seq[Seq[T]] =
      for i <- 0 until grid(0).length yield grid.map(_(i))
  inline def print(xs: Any*) =
    println(xs.mkString(" "))

  def gcd(x: Int, y: Int): Int = if y == 0 then x
  else gcd(y, x % y)

  extension (i: Int)
    def toBin = i.toBinaryString
    def toHex = i.toHexString

  def randInt(b1In: Int, b2In: Int) =
    val (mi, ma) = if b1In > b2In then (b2In, b1In) else (b1In, b2In)
    Random.between(mi, ma + 1)

  class MinPq[T](xs: T, priority: Int):
    var arr = ArrayBuffer(null, (xs, priority))
    val map = HashMap(xs -> 1)

    override def toString =
      arr.mkString("[", ",", "]") ++ "\n" ++ map.mkString("[", ",", "]")

    def +=(n: T, priority: Int): Unit =
      arr += ((n, priority))
      map(n) = arr.length - 1
      swim(arr.length - 1)

    def top = arr(1)

    def pop: (T, Int) =
      val ret = arr(1)
      swap(1, arr.length - 1)
      map -= ret._1
      arr = arr.take(arr.length - 1)
      sink(1)
      ret

    def decKey(n: T, nDist: Int) =
      val i = map(n)
      arr(i) = (n, nDist)
      swim(i)

    def swap(i: Int, j: Int): Unit =
      map(arr(i)._1) = j
      map(arr(j)._1) = i
      val n = arr(i)
      arr(i) = arr(j)
      arr(j) = n

    def swim(i: Int): Unit =
      var p = i
      while p > 1 && arr(p / 2)._2 > arr(p)._2 do
        swap(p / 2, p)
        p /= 2

    def sink(i: Int): Unit =
      val l = i * 2
      val r = l + 1
      var smol = i
      if l < arr.length && arr(l)._2 < arr(smol)._2 then
        smol = l
      if r < arr.length && arr(r)._2 < arr(smol)._2 then
        smol = r
      if smol != i then
        swap(i, smol)
        sink(smol)
