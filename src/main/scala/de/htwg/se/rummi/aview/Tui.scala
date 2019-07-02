package de.htwg.se.rummi.aview

import de.htwg.se.rummi.aview.swing.{Field, Grid, Rack, RackSortMode}
import de.htwg.se.rummi.controller.{Controller, FieldChangedEvent, PlayerSwitchedEvent, RackChangedEvent, StatusMessageChangedEvent, ValidStateChangedEvent, WinEvent}
import de.htwg.se.rummi.model.{Ending, RummiSet, Tile}

import scala.swing.Reactor

class Tui(controller: Controller) extends Reactor {

  val ROWS: Int = 8
  val COLS: Int = 13

  val grid = new Grid(8, 13)
  val rack = new Rack(4, 13)


  listenTo(controller)
  rack.loadRack(controller.getRack(controller.getActivePlayer).sortBy(x => (x.color, x.number)))

  def processInputLine(input: String): Unit = {
    input match {
      case "q" =>
      case "e" => //controller.createEmptyGrid
      case "n" => //controller.createNewGrid
      case "z" => //controller.undo
      case "y" => //controller.redo
      case "s" => //controller.solve
      case "f" => //controller.save
      case "l" => //controller.load
      case _ => input.split(" ").toList match {
        case from :: _ :: to :: Nil => {

          val x = coordsToFields(from, to) match {
            case Some(x) => moveTile(x._1, x._2, x._3)
            case _ => println("Invalid input")
          }
        }
        case _ =>
      }
    }
  }

  def printTui: Unit = {
    println("Current Player: " + controller.getActivePlayer.name)
    print("\n   ")
    print(('A' to ('A' + COLS - 1).toChar).mkString("  ", "  ", "\n"))

    var i = 1
    val gridStrings = printGrid.map(x => {
      val s = f"$i%2d" + "|" + x
      i += 1
      s
    })

    val rackStrings = printRack.map(x => {
      val s = f"$i%2d" + "|" + x
      i += 1
      s
    })

    ((gridStrings :+ "\n _________________________________________\n") ::: rackStrings).foreach(x => println(x))
  }

  reactions += {
    case event: RackChangedEvent => {
      println("RackChangedEvent")
      rack.loadRack(controller.getRack(controller.getActivePlayer).sortBy(x => (x.color, x.number)))
      printTui
    }

    case event: FieldChangedEvent => {
      println("FieldChangedEvent")
      grid.update(controller.getPlayingField)
      printTui
    }

    case event: ValidStateChangedEvent => {
      println("ValidStateChangedEvent")
    }

    case event: PlayerSwitchedEvent => {
      println("--- PlayerSwitchedEvent ---")
      println("Current Player: " + controller.getActivePlayer.name)
      rack.loadRack(controller.getRack(controller.getActivePlayer).sortBy(x => (x.color, x.number)))
      printTui
    }

    case event: WinEvent => {
      grid.enabled = false
    }

    case event: StatusMessageChangedEvent => {
      println("Status: " + controller.statusMessage)
    }
  }

  def printGrid: List[String] = {

    var rows: List[String] = Nil

    for (r <- 1 to grid.ROWS) {
      val fieldsInRow = grid.fields.filter(f => f.row == r)
      var row = ""
      for (c <- 1 to grid.COLS) {
        row += " " + (fieldsInRow.find(f => f.col == c).get.tileOpt match {
          case Some(t) => if (t.number < 10) " " + t.toString else t.toString
          case None => " _"
        })
      }
      rows = rows :+ row
    }
    rows
  }

  def printRack: List[String] = {
    var rows: List[String] = Nil
    for (r <- 1 to rack.ROWS) {
      val fieldsInRow = rack.fields.filter(f => f.row == r)
      var row = ""
      for (c <- 1 to rack.COLS) {
        row += " " + (fieldsInRow.find(f => f.col == c).get.tileOpt match {
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

  def coordsToFields(from: String, to: String): Option[(Field, Field, Tile)] = {
    val fromChars = from.toList
    val toChars = to.toList

    var toRow: Int = toChars.filter(x => x.isDigit).mkString("").toInt
    var fromRow: Int = fromChars.filter(x => x.isDigit).mkString("").toInt

    var toField: Field = null
    var fromField: Field = null

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

    if (fromRow > grid.ROWS) {
      fromRow = fromRow - grid.ROWS
      printRack
      fromField = rack.fields.find(x => x.row == fromRow && x.col == fromCol).getOrElse(return None)
    } else {
      fromField = grid.fields.find(x => x.row == fromRow && x.col == fromCol).getOrElse(return None)
    }

    if (toRow > grid.ROWS) {
      toRow = toRow - grid.ROWS
      toField = rack.fields.find(x => x.row == toRow && x.col == toCol).getOrElse(return None)
    } else {
      toField = grid.fields.find(x => x.row == toRow && x.col == toCol).getOrElse(return None)
    }

    fromField.tileOpt match {
      case Some(tile) => Some((fromField, toField, tile))
      case None => None
    }
  }

  def toColNumber(col: Char): Option[Int] = {
    val ret = col - 65
    if (ret >= 0 && ret <= COLS) {
      return Some(ret + 1)
    }
    None
  }


  private def moveTile(fieldFrom: Field, fieldTo: Field, selectedTile: Tile): Unit = {

    if (rack.fields.contains(fieldTo)) {
      controller.moveTileToRack(selectedTile)
      return
    }

    grid.getField(fieldTo.row, fieldTo.col).get.setTile(selectedTile)

    val row = fieldTo.row
    val col = fieldTo.col
    if (grid.getField(row, col - 1).isDefined &&
      grid.getField(row, col - 1).get.tileOpt.isDefined &&
      grid.getField(row, col + 1).isDefined &&
      grid.getField(row, col + 1).get.tileOpt.isDefined) {
      // Both neighbor fields are set -> combining the two sets to one

      val leftSet = grid.getSet(grid.getField(row, col - 1).get).get
      val rightSet = grid.getSet(grid.getField(row, col + 1).get).get
      grid.setsInGrid += leftSet -> (grid.getLeftField(leftSet), grid.getField(row, col + rightSet.tiles.size).get)
      controller.moveTile(selectedTile, leftSet, Ending.RIGHT)
      rightSet.tiles.foreach(t => controller.moveTile(t, leftSet, Ending.RIGHT))

    } else if (grid.getField(row, col + 1).isDefined && grid.getField(row, col + 1).get.tileOpt.isDefined) {
      // The field on the right is set
      val set = grid.getSet(grid.getField(row, col + 1).get).get
      grid.setsInGrid += set -> (fieldTo, grid.getRighttField(set))
      controller.moveTile(selectedTile, set, Ending.LEFT)

    } else if (grid.getField(row, col - 1).isDefined && grid.getField(row, col - 1).get.tileOpt.isDefined) {
      // The field on the left is set
      val set = grid.getSet(grid.getField(row, col - 1).get).get
      grid.setsInGrid += set -> (grid.getLeftField(set), fieldTo)
      controller.moveTile(selectedTile, set, Ending.RIGHT)
    }
    else {
      val newSet = new RummiSet(Nil)
      grid.setsInGrid += newSet -> (fieldTo, fieldTo)
      controller.moveTile(selectedTile, newSet, Ending.RIGHT)
    }
  }

}
