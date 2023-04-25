package tetris

import spinal.core._
import spinal.lib.graphic._


case class PixelPos( bitWidth : Int = 3 ) extends Bundle {
  val x, y = UInt( bitWidth bits)
}

case class block_pattern() extends  Component {

  val io = new Bundle {
    val pos       = in(PixelPos())
    val rgb       = out( Rgb(RgbConfig(4,4,4) ))
  }

  val border = Rgb(4,4,4)
  border.r := 8
  border.g := 0
  border.b := 0

  val inner = Rgb(4, 4, 4)
  inner.r := 15
  inner.g := 0
  inner.b := 0

  when ( io.pos.y < 2) {

    io.rgb := border

  } otherwise(

    io.rgb := inner
  )


}
object blockPatternMain{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(
      gen = block_pattern()
    )
  }
}