package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.controller.Controller
import de.htwg.se.rummi.model.{Ending, RummiSet, Tile}

import scala.collection.mutable
import scala.swing.{Dimension, GridPanel, Swing}

class Grid(val ROWS: Int, val COLS: Int, controller: Controller) extends GridPanel(rows0 = ROWS, cols0 = COLS) {

  var fields: List[Field] = Nil
  val setsInGrid = mutable.Map[RummiSet, (Field, Field)]()

  preferredSize = new Dimension(ROWS * 11, COLS * 11)

  for (i <- 1 to ROWS) {
    for (j <- 1 to COLS) {
      val field = new Field(i, j)
      field.border = Swing.LineBorder(Color.BLACK, 1)
      contents += field
      fields = field :: fields
    }
  }

  def getLeftField(set: RummiSet): Field = {
    setsInGrid(set)._1
  }

  def getRighttField(set: RummiSet): Field = {
    setsInGrid(set)._2
  }

  def getSet(field: Field): Option[RummiSet] = {
    setsInGrid.find(x => {
      val (left, right) = x._2
      left.row == field.row && left.col <= field.col && right.col >= field.col
    }) match {
      case Some(value) => Some(value._1)
      case None => None
    }
  }

  def getFreeSpaceOnGrid(set: RummiSet): Option[(Field, Field)] = {

    for (row <- 1 to ROWS) {
      val setsInRow = setsInGrid.retain((_, v) => v._1.row == row)
      val mappedList = setsInRow.map(k => (k._2._1.col, k._2._2.col)).toList.sortBy(x => x._1)
      if (COLS < set.tiles.size) return None
      if (mappedList.size == 0) return Some((getField(row, 1).get, getField(row, set.tiles.size).get))
      for (i <- 0 to mappedList.size - 1) {

        val setSize = set.tiles.size
        val SPACE_BETWEEN_SETS = 1

        // Check space between edge and first set
        if (i == 0) {
          if (setSize + SPACE_BETWEEN_SETS < mappedList(i)._1) {
            return Some((getField(row, 1).get, getField(row, setSize).get))
          }
        }

        // Check space between sets
        if (i <= mappedList.size - 2) {
          val x = mappedList(i)
          val y = mappedList(i + 1)
          val requiredSpace = setSize + 2 * SPACE_BETWEEN_SETS
          val spaceSize = (y._1 - x._2) - 1
          if (spaceSize >= requiredSpace) {
            return Some((getField(row, x._2 + 2).get, getField(row, y._1 - 2).get))
          }
        }

        // Check space between last set and edge of grid
        if (i == mappedList.size - 1) {
          val x = mappedList(i)
          if (setSize + SPACE_BETWEEN_SETS <= COLS - x._2) {
            return Some((getField(row, x._2 + 2).get, getField(row, x._2 + 1 + setSize).get))
          }
        }

      }
    }

    None
  }

  def update(setList: List[RummiSet]): Unit = {

    fields.foreach(x => x.unsetTile())

    setList.foreach(updateSets)

    // Remove all sets which have been removed from the controller
    setsInGrid.filter(s => !setList.contains(s._1)).foreach(s => setsInGrid -= s._1)

    //
    setsInGrid.toList.reverse.foreach(x => putTilesOnFields(x._1, x._2))

  }

  private def putTilesOnFields(set: RummiSet, fields: (Field, Field)) = {

    var col = fields._1.col

    for (i <- 0 to set.tiles.size - 1) {

      val field = getField(fields._1.row, col) match {
        case Some(f) => f
        case None => throw new NoSuchElementException
      }
      field.setTile(set.tiles(i))

      // Print red border
      val lastTile: Int = set.tiles.size - 1

      i match {
        case 0 => field.border = Swing.MatteBorder(2, 2, 2, 0, Color.RED)
        case `lastTile` => field.border = Swing.MatteBorder(2, 0, 2, 2, Color.RED)
        case _ => field.border = Swing.MatteBorder(2, 0, 2, 0, Color.RED)
      }
      col += 1
    }
  }

  private def updateSets(s: RummiSet) = {

    if (!setsInGrid.contains(s)) {
      // Set is added by another UI
      getFreeSpaceOnGrid(s) match {
        case Some(space) => setsInGrid += (s -> space)
        case None => throw new NoSuchElementException("No free space found.")
      }
    } else {
      // Set is already on the playing field, but may have changed

      val fieldsTuple = setsInGrid(s)

      val from = fieldsTuple._1
      val to = fieldsTuple._2

      val length = to.col - from.col + 1

      if (length != s.tiles.size) { // Check if size of set has changed
        getFreeSpaceOnGrid(s) match {
          case Some(space) => setsInGrid += (s -> space)
          case None => throw new NoSuchElementException("No free space found.")
        }
      }
    }
  }

  def isTileOnField(tile: Tile): Boolean = {
    return fields.find(t => t.tileOpt.isDefined && t.tileOpt.get == tile).isDefined
  }

  def isGridField(field: Field): Boolean = {
    return fields.contains(field)
  }

  def getField(x: Int, y: Int): Option[Field] = {
    fields.find(f => (f.row == x && f.col == y))
  }

  def moveTile(fieldTo: Field, fieldFrom: Field, selectedTile: Tile) = {

    // Tile was taken from the middle of a set
    if (getField(fieldFrom.row, fieldFrom.col + 1).isDefined &&
      getField(fieldFrom.row, fieldFrom.col + 1).get.tileOpt.isDefined &&
      getField(fieldFrom.row, fieldFrom.col - 1).isDefined &&
      getField(fieldFrom.row, fieldFrom.col - 1).get.tileOpt.isDefined) {

      val setFrom = getSet(fieldFrom).get
      val setStartField = setsInGrid(setFrom)._1
      val setEndField = setsInGrid(setFrom)._2

      val newSet = new RummiSet(Nil)

      val tilesLeftSet = setFrom.tiles.filter(x => setFrom.tiles.indexOf(x) < setFrom.tiles.indexOf(selectedTile))
      tilesLeftSet.foreach(x => controller.moveTile(x, newSet, Ending.RIGHT))

      setsInGrid += newSet -> (setStartField, getField(fieldFrom.row, fieldFrom.col - 1).get)

      setsInGrid += setFrom -> (getField(fieldFrom.row, fieldFrom.col + 1).get, setEndField)

    }
    else if (getField(fieldFrom.row, fieldFrom.col + 1).isDefined && getField(fieldFrom.row, fieldFrom.col + 1).get.tileOpt.isDefined) {
      val setFrom = getSet(fieldFrom).get
      setsInGrid += setFrom -> (getField(fieldFrom.row, fieldFrom.col + 1).get, setsInGrid(setFrom)._2)
    }
    else if (getField(fieldFrom.row, fieldFrom.col - 1).isDefined && getField(fieldFrom.row, fieldFrom.col - 1).get.tileOpt.isDefined) {
      val setFrom = getSet(fieldFrom).get
      setsInGrid += setFrom -> (setsInGrid(setFrom)._1, getField(fieldFrom.row, fieldFrom.col - 1).get)
    }

    getField(fieldTo.row, fieldTo.col).get.setTile(selectedTile)

    val row = fieldTo.row
    val col = fieldTo.col
    if (getField(row, col - 1).isDefined &&
      getField(row, col - 1).get.tileOpt.isDefined &&
      getField(row, col + 1).isDefined &&
      getField(row, col + 1).get.tileOpt.isDefined) {
      // Both neighbor fields are set -> combining the two sets to one

      val leftSet = getSet(getField(row, col - 1).get).get
      val rightSet = getSet(getField(row, col + 1).get).get
      setsInGrid += leftSet -> (getLeftField(leftSet), getField(row, col + rightSet.tiles.size).get)
      controller.moveTile(selectedTile, leftSet, Ending.RIGHT)
      rightSet.tiles.foreach(t => controller.moveTile(t, leftSet, Ending.RIGHT))

    } else if (getField(row, col + 1).isDefined && getField(row, col + 1).get.tileOpt.isDefined) {
      // The field on the right is set
      val a = getField(row, col + 1).get
      val set = getSet(a).get
      setsInGrid += set -> (fieldTo, getRighttField(set))
      controller.moveTile(selectedTile, set, Ending.LEFT)

    } else if (getField(row, col - 1).isDefined && getField(row, col - 1).get.tileOpt.isDefined) {
      // The field on the left is set
      val set = getSet(getField(row, col - 1).get).get
      setsInGrid += set -> (getLeftField(set), fieldTo)
      controller.moveTile(selectedTile, set, Ending.RIGHT)
    }
    else {
      val newSet = new RummiSet(Nil)
      setsInGrid += newSet -> (fieldTo, fieldTo)
      controller.moveTile(selectedTile, newSet, Ending.RIGHT)
    }
  }
}
