package de.htwg.se.rummi.aview

import de.htwg.se.rummi.Const
import de.htwg.se.rummi.controller.GameState
import de.htwg.se.rummi.controller.ControllerInterface
import de.htwg.se.rummi.controller.controllerBaseImpl.{FieldChangedEvent, GameStateChanged, PlayerSwitchedEvent, ValidStateChangedEvent}
import de.htwg.se.rummi.model.Grid

import scala.swing.Reactor

class Tui(co: ControllerInterface) extends Reactor {


  listenTo(co)


  def processInputLine(input: String): Unit = {
    input match {
      case "q" =>
      case "e" => //controller.createEmptyGrid
      case "n" => //controller.createNewGrid
      case "z" => co.undo
      case "y" => co.redo
      case "s" => print(co.save)
      case "sort" => co.sortRack
      case "finish" => co.switchPlayer
      case "draw" => co.draw
      case _ => input.split(" ").toList match {
        case from :: _ :: to :: Nil => co.moveTile(from, to)
        case _ => println("Can not parse input.")
      }
    }
  }

  def printTui: Unit = {
    print("\n   ")
    print(('A' to ('A' + Const.GRID_COLS - 1).toChar).mkString("  ", "  ", "\n"))

    var i = 1
    val gridStrings = printGrid(co.field, Const.GRID_ROWS).map(x => {
      val s = f"$i%2d" + "|" + x
      i += 1
      s
    })

    val rackStrings = printGrid(co.rackOfActivePlayer, Const.RACK_ROWS).map(x => {
      val s = f"$i%2d" + "|" + x
      i += 1
      s
    })

    ((gridStrings :+ "\n _________________________________________\n") ::: rackStrings).foreach(x => println(x))
  }

  reactions += {
    case event: FieldChangedEvent => {
      printTui
    }

    case event: ValidStateChangedEvent => {
      if (co.game.isValidField) {
        println("TUI: Field is valid again.")
      } else {
        println("TUI: Field is not valid anymore.")
      }
    }

    case event: PlayerSwitchedEvent => {
      println("It's " + co.activePlayer.name + "'s turn.")
      printTui
    }

    case event: GameStateChanged => {
      co.getGameState match {
        case GameState.WON => {
          println(("---- " + co.activePlayer + " wins! ----").toUpperCase)
        }
        case _ =>
      }
    }
  }

  def printGrid(grid: Grid, amountRows: Int): List[String] = {

    var rows: List[String] = Nil
    for (i <- 1 to amountRows) {
      var row = ""
      for (j <- 1 to Const.GRID_COLS) {
        row += " " + (grid.getTileAt(i, j) match {
          case Some(t) => if (t.number < 10) {
            " " + t.toString
          } else {
            t.toString
          }
          case None => " _"
        })
      }
      rows = rows :+ row
    }
    rows
  }


}
