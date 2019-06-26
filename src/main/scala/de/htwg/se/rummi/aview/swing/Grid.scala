package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.model.{RummiSet, Tile}

import scala.collection.mutable
import scala.swing.{Dimension, GridPanel, Swing}

class Grid(val ROWS: Int, val COLS: Int) extends GridPanel(rows0 = ROWS, cols0 = COLS) {

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

  def update(field: List[RummiSet]): Unit = {

    fields.foreach(x => x.unsetTile())

    for (s <- field) {
      if (!setsInGrid.contains(s)) {
        getFreeSpaceOnGrid(s) match {
          case Some(space) => setsInGrid += (s -> space)
          case None => throw new NoSuchElementException("No free space found.")
        }
      }
    }

    // Remove all sets which have been removed from the controller
    setsInGrid.filter(s => !field.contains(s._1)).foreach(s => setsInGrid -= s._1)

    for ((set, field) <- setsInGrid.toList.reverse) {

      val row = field._1.row
      var col = field._1.col

      for (i <- 0 to set.tiles.size - 1) {

        val field = getField(row, col) match {
          case Some(f) => f
          case None => throw new NoSuchElementException
        }

        field.setTile(set.tiles(i))
        val x: Int = set.tiles.size - 1

        i match {
          case 0 => field.border = Swing.MatteBorder(2, 2, 2, 0, Color.RED)
          case `x` => field.border = Swing.MatteBorder(2, 0, 2, 2, Color.RED)
          case _ => field.border = Swing.MatteBorder(2, 0, 2, 0, Color.RED)
        }
        col += 1
      }
    }

    println("Sets in Grid")
    for ((s, (lf, rf)) <- setsInGrid) {
      print("\t- (" + lf.row + ", " + lf.col + ") -> (" + rf.row + ", " + rf.col + ")\t")
      s.tiles.foreach(x => print(x + "|"))
      println()
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
}
