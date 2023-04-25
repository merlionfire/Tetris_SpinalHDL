package tetris

import java.io.FileWriter
import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Color, Dimension, Graphics}
import javax.swing.{JFrame, JLabel, JPanel, JTextArea, WindowConstants}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.graphic.vga.Vga
import spinal.lib.sim.{StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
object TetrisChecker {

  def apply( vga : Vga, cd : ClockDomain) : Unit = {

    SimTimeout(5000000)

    val resX = 640
    val resY = 480
    val resultArray = Array.ofDim[Int](resY, resX)



    fork {
      val image = new BufferedImage(resX, resY, BufferedImage.TYPE_INT_BGR )
      var rgb_value : Int = 0
      var red_ext: Int = 0
      var green_ext: Int = 0
      var blue_ext: Int = 0

      for ( y <-0 until resY ;
            x <-0 until resX ) {

        cd.waitSamplingWhere(vga.colorEn.toBoolean)
/*
        red_ext = vga.color.r.toInt | vga.color.r.toInt << 4
        green_ext = vga.color.g.toInt | vga.color.g.toInt << 4
        blue_ext = vga.color.b.toInt | vga.color.b.toInt << 4
*/
        red_ext = vga.color.r.toInt << 4 | 0xF
        green_ext =  vga.color.g.toInt << 4 | 0xF
        blue_ext = vga.color.b.toInt << 4 | 0xF

        rgb_value = ( red_ext << 16 | blue_ext << 8 | blue_ext  )
        resultArray(y)(x) = rgb_value
        image.setRGB(x, y, rgb_value )
      }

      val dutLines = ArrayBuffer[String]()
      println("DUT=")

      for (y <- 0 until resY) {
        val l = resultArray(y).map(v => f"$v%3d").mkString(",")
        //println(l)
        dutLines += l
      }

      val writer = new FileWriter("vag_output.txt")

      // Write each line to the file
      dutLines.foreach(line => writer.write(line + "\n"))

      // Close the FileWriter
      writer.close()



      val frame = new JFrame {
        setPreferredSize(new Dimension(resX+ 16, resY+ 48));
        var closed = false
        /*
        add(new JPanel {
          this.setPreferredSize(new Dimension(resX, resY))

          override def paintComponent(g: Graphics): Unit = {
            g.drawImage(image, 0, 0, resX, resY, null)
          }
        })
        */

        val mainPanel = new JPanel(new BorderLayout)
        mainPanel.setPreferredSize(new Dimension(resX, resY))

        val imagePanel = new JPanel {
          override def paintComponent(g: Graphics): Unit = {
            g.drawImage(image, 0, 0, resX, resY, null)
          }
        }
        mainPanel.add(imagePanel, BorderLayout.CENTER)

        val titlePanel = new JPanel {
          setPreferredSize(new Dimension(resX, 24))
          add(new JLabel("Title"))
        }
        mainPanel.add(titlePanel, BorderLayout.NORTH)

        val textPanel = new JPanel {
          setPreferredSize(new Dimension(resX, 48))
          add(new JTextArea("This is some text."))
        }
        mainPanel.add(textPanel, BorderLayout.SOUTH)

        add(mainPanel)

        pack();
        setVisible(true);
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
          override def windowClosing(e: WindowEvent): Unit = {
            closed = true
          }
        });
      }
      var error = false
      while (true) {
        if (frame.closed) {
          println("simTime : " + simTime())
          if (error) simFailure() else simSuccess()
          Thread.sleep(10)
        }
      }
    }




  }
}
