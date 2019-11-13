package de.htwg.se.rummi.aview

import de.htwg.se.rummi.Const
import de.htwg.se.rummi.controller.{Controller, FieldChangedEvent, GameState, GameStateChanged, PlayerSwitchedEvent, ValidStateChangedEvent, WinEvent}
import de.htwg.se.rummi.model.Grid

import scala.swing.Reactor

class Tui(co: Controller) extends Reactor {


  listenTo(co)


  def processInputLine(input: String): Unit = {
    input match {
      case "q" =>
      case "e" => //controller.createEmptyGrid
      case "n" => //controller.createNewGrid
      case "z" => //controller.undo
      case "y" => //controller.redo
      case "save" => println(co.save())
      case "f" => //controller.save
      case "l" => //controller.load
      case "sort" => co.sortRack()
      case "finish" => co.switchPlayer()
      case "draw" => co.draw()
      case _ => input.split(" ").toList match {
        case from :: _ :: to :: Nil => moveTile(from, to)
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

  def moveTile(from: String, to: String): Unit = {
    val (f, t) = coordsToFields(from, to).getOrElse(throw new NoSuchElementException("No such field."))

    if (f._1 <= Const.GRID_ROWS) {
      val tile = co.field.getTileAt(f._1, f._2).
        getOrElse({
          println("There is no tile on field " + from)
          return
        })
      if (t._1 <= Const.GRID_ROWS) {
        co.moveTile(co.field, co.field, tile, t._1, t._2)
      } else {
        co.moveTile(co.field, co.rackOfActivePlayer, tile, t._1 - Const.GRID_ROWS, t._2)
      }
    } else {
      val tile = co.rackOfActivePlayer.getTileAt(f._1 - Const.GRID_ROWS, f._2).
        getOrElse({
          println("There is no tile on field " + from)
          return
        })
      if (t._1 <= Const.GRID_ROWS) {
        co.moveTile(co.rackOfActivePlayer, co.field, tile, t._1, t._2)
      } else {
        co.moveTile(co.rackOfActivePlayer, co.rackOfActivePlayer, tile, t._1 - Const.GRID_ROWS, t._2)
      }
    }
  }

  def toColNumber(col: Char): Option[Int] = {
    val ret = col - 65
    if (ret >= 0 && ret <= Const.GRID_COLS) {
      return Some(ret + 1)
    }
    None
  }

  def coordsToFields(from: String, to: String): Option[((Int, Int), (Int, Int))] = {
    val fromChars = from.toList
    val toChars = to.toList

    val toRow: Int = toChars.filter(x => x.isDigit).mkString("").toInt
    val fromRow: Int = fromChars.filter(x => x.isDigit).mkString("").toInt

    val fromCol: Int = toColNumber(fromChars(0).charValue()) match {
      case Some(c) => c
      case None => {
        return None
      }
    }
    val toCol: Int = toColNumber(toChars(0).charValue()) match {
      case Some(c) => c
      case None =>
        return None
    }
    Some((fromRow, fromCol), (toRow, toCol))
  }

}
