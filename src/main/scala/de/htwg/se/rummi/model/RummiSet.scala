package de.htwg.se.rummi.model

trait RummiSet {
  def isValid(): Boolean

  def +(tile: Tile): RummiSet

  def -(tile: Tile): RummiSet
}

case class Run(tiles: List[Tile]) extends RummiSet {

  override def isValid(): Boolean = {
    if(tiles.size < 3) return false
    if (tiles.groupBy(_.color).size > 1) return false
    // TODO: Check if tiles are ascending
    true
  }

  override def +(tile: Tile): RummiSet = new Run((tiles :+ tile).sortBy(x => x.number))

  override def -(tile: Tile): RummiSet = new Run(tiles.filter(_ != tile).sortBy(x => x.number))

}

case class Group(tiles: List[Tile]) extends RummiSet {

  override def isValid(): Boolean = {
    if(tiles.size < 3) return false
    if (tiles.groupBy(_.number).size > 1) return false
    if (tiles.groupBy(_.color).size != tiles.size) return false
    true
  }
  override def +(tile: Tile): RummiSet = new Group(tiles :+ tile)

  override def -(tile: Tile): RummiSet = new Group(tiles.filter(_ != tile))
}


