package tetris

import spinal.core._
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.{Vga, VgaCtrl}
import spinal.lib.master


case class top ( ) extends Component {

  import tetrisConfig._

  val io = new Bundle{
    val vga       = master(Vga(rgbConfig))
    val patgen_sel = in Bool()
  }

  val testImage: Boolean  = true

  val objects = new layout()
  val patgen  = new patgen(16)
  val ctrl = VgaCtrl(rgbConfig,timingsWidth = timingsWidth )


  val x_addr = Reg(UInt(xBitsWidth bits)) init(0)
  val y_addr = Reg(UInt(yBitsWidth bits)) init(0)

  when ( ctrl.io.vga.colorEn ) {
    x_addr :=  x_addr + 1
  } .otherwise {
    x_addr := 0
  }

  when(ctrl.io.frameStart) {
    y_addr := 0
  }.elsewhen( ctrl.io.vga.colorEn.fall() ) {
    y_addr := y_addr + 1
  }


  objects.io.y  := y_addr
  objects.io.x  := x_addr

  ctrl.io.softReset := False
  ctrl.io.timings.setAs_h640_v480_r60
  ctrl.io.pixels.valid := True


  patgen.io.color_en := ctrl.io.vga.colorEn
  patgen.io.x := x_addr
  patgen.io.y := y_addr
  patgen.io.sel := io.patgen_sel.rise(False)
  ctrl.io.vga <> io.vga

  if (testImage) {
    ctrl.io.pixels.r := patgen.io.color.r
    ctrl.io.pixels.g := patgen.io.color.g
    ctrl.io.pixels.b := patgen.io.color.b

  } else {
    ctrl.io.pixels.r := objects.io.color.r
    ctrl.io.pixels.g := objects.io.color.g
    ctrl.io.pixels.b := objects.io.color.b
  }

}


object top{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(
      //gen = shape()
      gen = new top()
    )
  }
}