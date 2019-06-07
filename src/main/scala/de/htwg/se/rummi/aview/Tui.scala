package de.htwg.se.rummi.aview

import de.htwg.se.rummi.controller.{Controller, PlayerSwitchedEvent}

import scala.swing.Reactor

class Tui(controller: Controller) extends Reactor {


  listenTo(controller)

  def processInputLine(input: String): Unit = {
    input match {
      case _ => print(input + "\n")
    }
  }

  reactions += {
    case event: PlayerSwitchedEvent => printTui
  }

  def printTui: Unit = {
    println("Print Tui")
  }
}
