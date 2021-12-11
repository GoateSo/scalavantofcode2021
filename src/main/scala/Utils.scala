import scala.util._
import scala.math._
import scala.util.matching._
import scala.util.matching.Regex.Match

/**
 *  collection of useful utility methods & shorthands
 */
object Utils:
  val alph = "[a-zA-Z]".r

  extension (n : Double)
    def **(m : Double) = Math.pow(n,m)

  extension (str : String)
    def toInt(radix : Int) = Integer.parseInt(str,radix)
    def gsub(reg : Regex, f : Seq[String] => String) = 
      reg.replaceAllIn(str, _ match {
        case reg(xs*) => f(xs)
      })
    def gsub(reg : Regex, f : Seq[String] => String,times : Int ) = 
      var occurs = 0
      reg.replaceSomeIn(str, _ match {
        case reg(xs*) if occurs < times => 
          occurs += 1
          Some(f(xs))
        case _ => None
      })
    def gsub(reg : Regex, rep : String) =
      reg.replaceAllIn(str, rep)
    def gsub(reg : Regex, rep : String, times : Int) = 
      var occurs = 0
      reg.replaceSomeIn(str, _ match {
        case reg(_*) if occurs < times =>
          occurs += 1
          Some(rep)
        case _ => None
      })

  extension [T](grid : Seq[Seq[T]])
    def columns: Seq[Seq[T]] = 
      for i <- 0 until grid(0).length yield
        grid.map(_(i))
  inline def print(xs: Any*) =
    println(xs.mkString(" "))

  def gcd(x: Int, y: Int): Int = if y == 0 
    then x
    else gcd(y, x % y)
  
  extension (i : Int)
    def toBin = i.toBinaryString
    def toHex = i.toHexString
     
  def randInt(b1In : Int, b2In : Int) =
    val (mi,ma) = if b1In > b2In then (b2In,b1In) else (b1In,b2In)
    Random.between(mi,ma + 1)
