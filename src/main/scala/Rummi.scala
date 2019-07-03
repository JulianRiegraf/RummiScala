import de.htwg.se.rummi.aview.Tui
import de.htwg.se.rummi.aview.swing.SwingGui
import de.htwg.se.rummi.controller.Controller

import scala.io.StdIn

object Rummi {

  def main(args: Array[String]): Unit = {

    println(" _____                               _  _            _")
    println("|  __ \\                             (_)| |          | |")
    println("| |__) |_   _  _ __ ___   _ __ ___   _ | | __ _   _ | |__")
    println("|  _  /| | | || '_ ` _ \\ | '_ ` _ \\ | || |/ /| | | || '_ \\")
    println("| | \\ \\| |_| || | | | | || | | | | || ||   < | |_| || |_) |")
    println("|_|  \\_\\\\__,_||_| |_| |_||_| |_| |_||_||_|\\_\\ \\__,_||_.__/")

    println()

    var playerNames: List[String] = null

    if (args.size > 0) {
      // Read player names from program arguments
      val numberOfPlayers : Int= args(0).toInt
      if (args.size - 1 != numberOfPlayers) throw new IllegalArgumentException
      playerNames = args.slice(1, args.size).toList
    } else {
      // Ask user to input player names
      print("Number of players? ")
      val numberOfPlayer = StdIn.readLine().toInt

      for (i <- 1 to numberOfPlayer) {
        print("Player " + i + ": ")
        playerNames = StdIn.readLine() :: playerNames
      }
    }

    val controller = new Controller(playerNames)
    controller.initGame()

    val tui = new Tui(controller)
    tui.printTui

    val gui = new SwingGui(controller)
    gui.init
    gui.visible = true

//    val gui1 = new SwingGui(controller)
//    gui1.init
//    gui1.visible = true

    var input: String = ""

    do {
      input = StdIn.readLine()
      tui.processInputLine(input)
    } while (input != "q")
  }
}