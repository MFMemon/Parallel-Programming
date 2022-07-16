package scalashop

import java.util.concurrent.*
import scala.util.DynamicVariable
import scala.util.Random

import org.scalameter.*

/** The value of every pixel is represented as a 32 bit integer. */
type RGBA = Int

/** Returns the red component. */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): RGBA = data(y * width + x)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA =
  val max_x = x + radius ; val max_y = y + radius ; val min_x = x - radius ; val min_y = y - radius
  var r = 0 ; var g = 0 ; var b = 0 ; var a = 0 ;

  var i = min_x; var j = min_y
  var pixCount = 0

  while(i <= max_x)
      j = min_y
      while(j <= max_y)
          if clamp(i, 0, src.width - 1) == i && clamp(j, 0, src.height - 1) == j  then
              val rgbaVal = src(i,j)
              r += red(rgbaVal); g += green(rgbaVal); b += blue(rgbaVal); a += alpha(rgbaVal);
              pixCount += 1
          j += 1
      i += 1


  rgba(r / pixCount, g / pixCount, b / pixCount, a / pixCount)

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)




/*--TEST*/
def testFun(w: Int, h: Int, n: Int) = 
  def randGen = (new Random().nextDouble * 1000).toInt % 128
  val a = Img(w,h)
  
  for
    i <- 0 until w
    j <- 0 until h
  do
    a.update(i,j,rgba(randGen, randGen, randGen, randGen/1000))
  
  val b = a
  println(VerticalBoxBlur.blur(a,b,1,2,1))
  println(HorizontalBoxBlur.parBlur(a,b,n,1))


def mergeSort(orig: Array[Int], begin: Int, end: Int): Array[Int] = 

  if (begin >= end)
    Array(orig(end))
  
  else
    val mid = (math.floor((begin + end) / 2.0)).toInt 
    val left = mergeSort(orig, begin, mid)
    val right = mergeSort(orig, mid + 1, end)
    merge(left, right)


def merge(left: Array[Int], right: Array[Int]): Array[Int] = 
  val arrLen = left.length + right.length
  val res = new Array[Int](arrLen)
  var i = 0; var j = 0

  for
   k <- 0 until arrLen
   if i < left.length && j < right.length
  do
    if left(i) < right(j) then
      res(k) = left(i)
      i += 1
    else
      res(k) = right(j)
      j += 1
  
  var(remInd, remArr) = if (i == left.length) then (j, right) else (i, left)
  
  for l <- i+j until arrLen
  do 
    res(l) = remArr(remInd)
    remInd += 1
 
  res
  