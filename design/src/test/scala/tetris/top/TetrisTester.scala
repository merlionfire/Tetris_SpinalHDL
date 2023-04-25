package tetris

import common.WorkshopSimConfig
import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.graphic.RgbConfig


class TetrisTester extends FunSuite{

  var compiled : SimCompiled[top] = null

  test("compile") {
    compiled = WorkshopSimConfig()
      .withVCS
      .withFSDBWave
      .workspacePath("tb")
      .compile(
        top(  )
      )
    }



  test( testName = "testbench") {
    compiled.doSimUntilVoid(seed = 1) { dut =>

      dut.clockDomain.forkStimulus(10)
      TetrisChecker( dut.io.vga, dut.clockDomain)

    }

  }
}



