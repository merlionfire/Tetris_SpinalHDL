package tetris

import spinal.core._
import spinal.lib.graphic.{Rgb, RgbConfig}
import spinal.core.Mem

import scala.collection.mutable.ArrayBuffer

/*
case class BlockPos( bitWidth : Int = 2 ) extends  Bundle {
  val x, y = UInt( bitWidth bits)
}

case class shape() extends Component {

  val io = new Bundle {
    val shape_id = in UInt( 3 bits )
    //val rotate   = in UInt( 2 bits )
    //val shape = out Vec( Bits( 4 bits), 4)
    val pos = in ( BlockPos() )
    val block_on  = out Bool()
  }

  val o_block   = Vec( B"1100", B"1100", B"0000", B"0000")
  val i_block_0 = Vec( B"1111", B"0000", B"0000", B"0000")
  val i_block_1 = Vec( B"1000", B"1000", B"1000", B"1000")



  val shape = io.shape_id.mux(
    0 -> o_block,
    1 -> i_block_0,
    default -> i_block_1
  )

  io.block_on := shape(io.pos.y)(io.pos.x)



}


*/






object Color {
  def apply( color : Int ) = {
    val rgb = Rgb(RgbConfig(4,4,4))
    rgb.r := U(  color & 0xF)
    rgb.g := U( (color >> 4 ) & 0xF )
    rgb.b := U( (color >> 8 ) & 0xF )
    rgb
  }
}

case class Position ( x : Int, y : Int )

class _object ( x : Int, y : Int, color: Int) extends Component {

  import tetrisConfig._

  val x0 = U(x, xBitsWidth bits)
  val y0 = U(y, yBitsWidth bits)

  val io = new Bundle {

    val x = in UInt (xBitsWidth bits)
    val y = in UInt (yBitsWidth bits)
    val inside = out(Reg(Bool()) init (false))
    val color = out(Rgb(RgbConfig(4, 4, 4)))
  }

  io.color.r := U(color & 0xF)
  io.color.g := U((color >> 4) & 0xF)
  io.color.b := U((color >> 8) & 0xF)
}


class rectangle ( origin : Position, color: Int, width : Int, height : Int) extends _object( origin.x, origin.y, color) {
  import tetrisConfig._

  val x_width = U(width, xBitsWidth bits)
  val y_height = U(height, yBitsWidth bits)
  when(io.x >= x0 && io.x < x0 + x_width && io.y >= y0 && io.y < y0 + y_height) {
    io.inside := True
  } otherwise (
    io.inside := False
    )

}

class square ( origin : Position, color: Int, size : Int) extends rectangle( origin, color,size, size ) {
}

class ball ( origin : Position, color: Int, size : Int) extends _object( origin.x, origin.y, color) {
  import tetrisConfig._

  val diameter = U(size,  xBitsWidth bits)

  val rom = Vec( Bits( size bits), size )

  for (i <- 0 until size; j <- 0 until size ) {
    if (Math.sqrt((i - size / 2) * (i - size / 2) + (j - size / 2) * (j - size / 2)) <= size / 2) {
      rom(i)(j) := True
    } else {
      rom(i)(j) := False
    }
  }


  val x_offset = (io.x - x0).resize( log2Up(size) bits )
  val y_offset = (io.y - y0).resize( log2Up(size) bits )

  val xBits = rom( x_offset )

  when(io.x >= x0 && io.x < x0 + diameter && io.y >= y0 && io.y < y0 + diameter ) {
    io.inside := xBits( y_offset )
  } otherwise (
    io.inside := False
    )

}



class layout extends  Component {
  import tetrisConfig._

  val io = new Bundle {
    val x = in UInt (xBitsWidth bits)
    val y = in UInt (yBitsWidth bits)
    val inside = out(Reg(Bool()) init (false))
    val color = out(Rgb(RgbConfig(4, 4, 4)))
  }

  var color_out = Color(0x000000)
  var inside_out = Bool(false)

  val object_array = ArrayBuffer[_object]()

  object_array += new rectangle( Position( 10,20),  0x00F,20, 120 )
  object_array += new square(    Position( 110,20), 0x0F0, size= 60 )
  object_array += new ball (    Position( 200,300), 0xF00, size= 100 )

  for ( item <- object_array) {
    item.io.x := io.x
    item.io.y := io.y
    when ( item.io.inside ) {
      color_out := item.io.color
    }
    inside_out |= item.io.inside
  }

  io.color  := color_out
  io.inside := inside_out

}

object shapeMain{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(
      //gen = shape()
      gen = new layout()
    )
  }
}


