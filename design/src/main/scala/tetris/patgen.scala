package tetris

import spinal.core._
import spinal.lib.Counter
import spinal.lib.graphic.{Rgb, RgbConfig}


case class patgen( numBars : Int) extends Component {

  import tetrisConfig._

  val io = new Bundle {
    val color_en = in Bool()
    val x = in UInt (xBitsWidth bits)
    val y = in UInt (yBitsWidth bits)
    val sel = in Bool()
    val color = out(Rgb(rgbConfig))
  }


  val barBitsWidth: Int = log2Up(xWidth / numBars)

  val sel_id = Counter(stateCount = 3, io.sel)


  val color_blank = Rgb(rgbConfig)
  color_blank.clear()


  val color_bar = new Area {

    val color = Rgb(rgbConfig)

    val idx = io.x(io.x.high downto barBitsWidth)
    val Color = List(
      0xFFF, 0x007, 0x00F, 0x070, 0x07F, 0x0FF, 0x700,
      0x707, 0x70F, 0x770, 0x77F, 0x7FF, 0xF00, 0xF07,
      0xF0F, 0xF70, 0xF7F, 0xFFF, 0xF50, 0xF05, 0xF55
    )

    assert(numBars <= Color.length)

    /* Comment below code because it creates multiple "if()" rather than case/endcase of Verilog
    for ((color, index) <- Color.zipWithIndex) {
      when( idx === U(index, idx.getWidth bits)) {
        io.color.r := U(color & 0xF)
        io.color.g := U((color >> 4) & 0xF)
        io.color.b := U((color >> 8) & 0xF)
      }

    }
    */

    switch(idx, coverUnreachable = false) {
      for ((color_i, index) <- Color.take(numBars).zipWithIndex) {
        is(U(index, idx.getWidth bits)) {
          color.b := U(color_i & 0xF)
          color.g := U((color_i >> 4) & 0xF)
          color.r := U((color_i >> 8) & 0xF)
        }
      }
      default {
        color.b := U(0)
        color.g := U(0)
        color.r := U(0)
      }

    }

  }


  val color_palette = new Area {

    val color = Rgb(rgbConfig)

    //val idx = io.y(io.y.high downto 3).resize( 6 bits ) @@ io.x(io.x.high downto 3).resize(6 bits)
    val idx =  io.x(io.x.high downto 3).resize(6 bits)  @@ io.y(io.y.high downto 3).resize( 6 bits )
    val color_vec = idx.subdivideIn(4 bits)

    color.r := color_vec(2)
    color.g := color_vec(1)
    color.b := color_vec(0)

  }

  when(sel_id === 2) {
    color_palette.color.r.msb := True
  }

  io.color  :=  sel_id.mux(
    0 -> color_bar.color,
    (1,2) -> color_palette.color,
    //2 -> color_palette.color,
    default -> color_blank
  )

}