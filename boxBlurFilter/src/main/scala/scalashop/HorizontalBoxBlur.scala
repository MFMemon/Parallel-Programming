package scalashop

import org.scalameter.*

object HorizontalBoxBlurRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val radius = 3
    val width = 1920
    val height = 1080
    val src = Img(width, height)
    val dst = Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 2
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface:

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =

  
    def memLoc(x: Int,y: Int) = y * src.width + x
    var iter = from

    while(iter < end)
      var x = 0; var y = iter
      while( x < src.width)
        val rgbaVal = boxBlurKernel(src, x, y, radius)
        dst.update(x, y, rgbaVal)
        x += 1
        val memLoc = y*src.width + x
      iter += 1


  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =

    val remPixels = src.height % numTasks

    val step = if (src.height > numTasks) then src.height/numTasks else 1
    var splitRange = 0 to (if step == 1 then src.height else (src.height - remPixels)) by step

    var stripList = splitRange zip splitRange.tail

    if remPixels != 0 && step != 1 then
      val lastStrip = stripList(stripList.length - 1)
      stripList = stripList.updated(stripList.length - 1, (lastStrip._1, lastStrip._2 + remPixels))
    
    val taskList = 
      for
        strip <- stripList
      yield
        task(blur(src, dst, strip._1, strip._2, radius))
   
    for
     task <- taskList
    do
      task.join()  



