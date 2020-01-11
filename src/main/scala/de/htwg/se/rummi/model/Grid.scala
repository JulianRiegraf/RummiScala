package de.htwg.se.rummi.model

import play.api.libs.json.{JsNumber, JsObject, Json}

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

  /**
    * Creates a new Grid with same size but with the specified tiles.
    * @param tiles the tiles for the new Grid
    * @return a new Grid with same size
    */
  def copy(tiles: Map[(Int, Int), Tile]): Grid ={
    Grid(ROWS,COLS, tiles)
  }

  def toXml = {
    <grid>
      <cols>{COLS}</cols>
      <rows>{ROWS}</rows>
      <tiles>
        {tiles.toList.map(mapTupleToXml)}
      </tiles>
    </grid>
  }

  private def mapTupleToXml(tuple: ((Int, Int), Tile)) = {
    val x = tuple._1._1
    val y = tuple._1._2
    val t = tuple._2

    <tilePos>
      <x>{x}</x>
      <y>{y}</y>{t.toXml}
    </tilePos>
  }
}

object Grid {

  import play.api.libs.json._

  implicit val mapWrites: Writes[Grid] = new Writes[Grid] {
    override def writes(o: Grid): JsValue = Json.obj(
        "COLS" -> JsNumber(o.COLS),
        "ROWS" -> JsNumber(o.ROWS),
        "tiles" -> JsArray(o.tiles.toList.map(mapTupleToJson))
    )
  }

  def mapTupleToJson(tuple: ((Int, Int), Tile)): JsObject = {
    val x = tuple._1._1
    val y = tuple._1._2
    val t = tuple._2
    Json.obj(
      "x" -> JsNumber(x),
      "y" -> JsNumber(y),
      "tile" -> Json.toJson(t)
    )
  }

}