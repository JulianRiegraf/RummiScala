package de.htwg.se.rummi

import com.google.inject.Guice
import de.htwg.se.rummi.aview.Tui
import de.htwg.se.rummi.aview.swing.SwingGui
import de.htwg.se.rummi.controller.ControllerInterface

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

    var playerNames: List[String] = Nil

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

    val injector = Guice.createInjector(new RummiModule)
    val controller = injector.getInstance(classOf[ControllerInterface])

    controller.initGame(playerNames)

    val tui = new Tui(controller)
    tui.printTui

    val gui = new SwingGui(controller)
    gui.init
    gui.visible = true

    var input: String = ""

    do {
      print("\nrummi>")
      input = StdIn.readLine()
      tui.processInputLine(input)
    } while (input != "q")
  }
}
