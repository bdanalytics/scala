
import common._

package object scalashop {

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
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops

    val dbg = false // ((src.width == 4) & (src.height == 3))

    if (dbg)
      println(s"scalashop: boxBlurKernel: src[${src.width}, ${src.height}]; radius: $radius; " +
              s"data(x = $x, y = $y): ${src(x,y)}")

    val xIx = (for (xb <- (x - radius) to (x + radius)) yield clamp(xb, 0, src.width  - 1)).distinct
    val yIx = (for (yb <- (y - radius) to (y + radius)) yield clamp(yb, 0, src.height - 1)).distinct
//
    if (dbg) {
      println(s"scalashop: boxBlurKernel: xIx: $xIx")
      println(s"scalashop: boxBlurKernel: yIx: $yIx")
    }

    val rVtr = for (xb <- xIx; yb <- yIx) yield  red(src(xb, yb))
////    println(s"scalashop: boxBlurKernel: rVtr: $rVtr")
////    println(s"scalashop: boxBlurKernel: rVtr.mean: ${rVtr.sum / rVtr.size}")

    val gVtr = for (xb <- xIx; yb <- yIx) yield green(src(xb, yb))
    val bVtr = for (xb <- xIx; yb <- yIx) yield  blue(src(xb, yb))
    val aVtr = for (xb <- xIx; yb <- yIx) yield alpha(src(xb, yb))

    if (dbg) println(s"scalashop: boxBlurKernel: size: ${rVtr.size}")
    rgba(rVtr.sum / rVtr.size, gVtr.sum / gVtr.size, bVtr.sum / bVtr.size, aVtr.sum / aVtr.size)
  }

}
