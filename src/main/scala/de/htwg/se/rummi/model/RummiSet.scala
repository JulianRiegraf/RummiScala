package de.htwg.se.rummi.model

class RummiSet(var tiles: List[Tile]) {

  def +(tile: Tile): Unit = {
    tiles = tile :: tiles
  }

  def -(tile: Tile): Unit = {
    tiles = tile :: tiles
  }

  def isValidRun(): Boolean = {
    if (tiles.size < 3) return false
    if (tiles.groupBy(_.color).size > 1) return false

    for (i <- 0 to tiles.size - 2) {
      val n: List[Tile] = tiles.sortBy(_.number)
      if (n(i).number + 1 != n(i + 1).number)
        return false
    }
    true
  }

  def isValidGroup(): Boolean = {
    if (tiles.size < 3) return false
    if (tiles.groupBy(_.number).size > 1) return false
    if (tiles.groupBy(_.color).size != tiles.size) return false
    true
  }

}