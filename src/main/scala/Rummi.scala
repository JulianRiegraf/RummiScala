import de.htwg.se.rummi.aview.Tui
import de.htwg.se.rummi.aview.swing.SwingGui
import de.htwg.se.rummi.controller.Controller

import scala.io.StdIn

object Rummi {


  var tui: Tui = null
  var gui: SwingGui = null

  def main(args: Array[String]): Unit = {

    println(" _____                               _  _            _")
    println("|  __ \\                             (_)| |          | |")
    println("| |__) |_   _  _ __ ___   _ __ ___   _ | | __ _   _ | |__")
    println("|  _  /| | | || '_ ` _ \\ | '_ ` _ \\ | || |/ /| | | || '_ \\")
    println("| | \\ \\| |_| || | | | | || | | | | || ||   < | |_| || |_) |")
    println("|_|  \\_\\\\__,_||_| |_| |_||_| |_| |_||_||_|\\_\\ \\__,_||_.__/")

    println()
    print("Number of players? ")
    val numberOfPlayer = StdIn.readLine().toInt
    var playerNames: List[String] = Nil

    for (i <- 1 to numberOfPlayer) {
      print("Player " + i + ": ")
      playerNames = StdIn.readLine() :: playerNames
    }

    val controller = new Controller(playerNames)
    controller.initGame()

    tui = new Tui(controller)
    gui = new SwingGui(controller)
    gui.init
    gui.visible = true


    var input: String = ""

    do {
      input = StdIn.readLine()
      tui.processInputLine(input)
    } while (input != "q")

  }
}



