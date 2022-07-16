package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    var count = 0;  var iter = 0
    while(count >= 0 && iter < chars.length)
      if chars(iter) == '(' then count += 1
      else if chars(iter) == ')' then count -= 1
      iter += 1
    count == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    // @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if idx >= until then
        (arg1, if arg2 > 0 then arg2 else 0)
      else
        val res = if chars(idx) == '(' then arg2 + 1 else if chars(idx) == ')' then arg2 - 1 else arg2
        traverse(idx + 1, until, if res < 0 then math.abs(res) else arg1 , res)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if until - from <= threshold then
        traverse(from, until, 0,0)
      else
        val mid = (from + until)/2
        val (l,r) = parallel(reduce(from, mid), reduce(mid, until))
        if l._2 >= r._1 then (l._1, l._2 + r._2 - r._1) else (l._1 + r._1 - l._2, r._2)
    }

    reduce(0, chars.length) == (0,0)

  val th = 4

  def scanLeft(inp: Array[Int], a0: Int, f: (Int,Int) => Int, out: Array[Int]) = 

    def reduceSeg1(inp: Array[Int], left: Int, right: Int, a0: Int, f: (Int,Int) => Int): Int = 
      var res = a0
      var i = left
      while (i < right)
        res = f(res, inp(i))
        i += 1
      res


    def mapSeg(inp: Array[Int], left: Int, right: Int, fi : (Int,Int) => Int, out: Array[Int]): Unit = 
      if right <= left then
        out(left) = fi(left,0)
      else
        val mid = (right + left) / 2
        parallel(mapSeg(inp, left, mid, fi, out), mapSeg(inp, mid+1, right, fi, out))

    val fi = { (i:Int,v:Int) => reduceSeg1(inp, 0, i, a0, f) }
    mapSeg(inp, 0, inp.length - 1, fi, out)
    val last = inp.length - 1
    out(last + 1) = f(out(last), inp(last))
