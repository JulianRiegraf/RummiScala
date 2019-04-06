package de.htwg.se.rummi.model

trait Set {
  def isValid(): Boolean

  def +(tile: Tile): Set

  def -(tile: Tile): Set
}

case class Run(tiles: List[Tile]) extends Set {

  override def isValid(): Boolean = {
    if(tiles.size < 3) return false
    if (tiles.groupBy(_.color).size > 1) return false
    if (tiles.groupBy(_.number).size != tiles.size) return false
    true
  }

  override def +(tile: Tile): Set = new Run((tiles :+ tile).sorted)

  override def -(tile: Tile): Set = new Run(tiles.filter(_ != tile).sorted)

}

case class Group(tiles: List[Tile]) extends Set {

  override def isValid(): Boolean = {
    if(tiles.size < 3) return false
    if (tiles.groupBy(_.number).size > 1) return false
    if (tiles.groupBy(_.color).size != tiles.size) return false
    true
  }
  override def +(tile: Tile): Set = new Group(tiles :+ tile)

  override def -(tile: Tile): Set = new Group(tiles.filter(_ != tile))
}


