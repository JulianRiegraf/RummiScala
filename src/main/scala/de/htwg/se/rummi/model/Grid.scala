package de.htwg.se.rummi.model

case class Grid(ROWS: Int, COLS: Int, tiles: Map[(Int, Int), Tile]) {

  tiles.keys.find(x => x._1 > ROWS || x._2 > COLS)
    .map(x => throw new IllegalArgumentException("Tile indices '" + x + "' out of bounds."))

  def getTileAt(row: Int, col: Int): Option[Tile] = {
    tiles.get((row, col))
  }

  def getFreeField(): Option[(Int, Int)] = {
    for (i <- 1 to ROWS; j <- 1 to COLS) {
      if (tiles.get((i, j)).isEmpty) {
        return Some(((i, j)))
      }
    }
    None
  }

  /**
    * Get the position of a tile in the grid.
    *
    * @param tile the tile
    * @return row, col tuple
    */
  def getTilePosition(tile: Tile): Option[(Int, Int)] = tiles.find(x => x._2 == tile).map(x => x._1)

  def size(): Int = tiles.size

}
