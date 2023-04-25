package tetris

import spinal.core._
import spinal.lib._
import spinal.lib.graphic.{Rgb, RgbConfig}

case class patgen( numBars : Int) extends Component {

  import tetrisConfig._

  val io = new Bundle {
    val color_en = in Bool()
    val x = in UInt (xBitsWidth bits)
    val y = in UInt (yBitsWidth bits)
    val sel = in Bool()
    val color = out(Rgb(rgbConfig) )
  }


  val barBitsWidth: Int = log2Up(xWidth / numBars )

  val sel_toggle = Reg(Bool() ) init(false)

  sel_toggle :=  RegNextWhen( ! sel_toggle, io.sel)

  val color_bar = new Area {

    val color = Rgb(rgbConfig)

    val idx = io.x(io.x.high downto  barBitsWidth )
    val Color = List(
      0x000, 0x007, 0x00F, 0x070, 0x07F, 0x0FF, 0x700,
      0x707, 0x70F, 0x770, 0x77F, 0x7FF, 0xF00, 0xF07,
      0xF0F, 0xF70, 0xF7F, 0xFFF
    )


    /* Comment below code because it creates multiple "if()" rather than case/endcase of Verilog
    for ((color, index) <- Color.zipWithIndex) {
      when( idx === U(index, idx.getWidth bits)) {
        io.color.r := U(color & 0xF)
        io.color.g := U((color >> 4) & 0xF)
        io.color.b := U((color >> 8) & 0xF)
      }

    }
    */

    switch ( idx, coverUnreachable = false  )  {
      for ((color_i, index) <- Color.take(numBars).zipWithIndex) {
        is(U(index, idx.getWidth bits)) {
          color.b := U(color_i & 0xF)
          color.g := U((color_i >> 4) & 0xF)
          color.r := U((color_i >> 8) & 0xF)
        }
      }

    }

  }




  val color_palette = new Area {

    val color = Rgb(rgbConfig)

    val idx = io.y(io.y.high downto 4 )  @@ io.x( io.x.high downto 3)

    val color_vec = idx.resize(12).subdivideIn(4 bits)

    color.r  := color_vec( 2 )
    color.g  := color_vec( 1 )
    color.b  := color_vec( 0 )

  }

  io.color  :=  sel_toggle ? color_bar.color | color_palette.color

/*



  when ( io.color_en ) {

    when ( pixel_cnt === ( barWidth - 1 )  ) {
      pixel_cnt := 0
      idx := idx + 1
    } otherwise {
      pixel_cnt := pixel_cnt + 1
    }

  } .otherwise {
    pixel_cnt := 0
    idx := 0
  }
*/
}