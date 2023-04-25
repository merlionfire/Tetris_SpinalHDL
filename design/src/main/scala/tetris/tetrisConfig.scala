package tetris
import spinal.lib.graphic.RgbConfig
import spinal.core._
object tetrisConfig {


    val rgbConfig = RgbConfig(4, 4, 4)

    val xWidth : Int = 640
    val yWidth : Int = 480
    val timingsWidth = 10
    val xBitsWidth : Int = log2Up(xWidth)
    val yBitsWidth : Int = log2Up(yWidth)

}
