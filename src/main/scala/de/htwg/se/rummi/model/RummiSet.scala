package de.htwg.se.rummi.model

class RummiSet(var tiles: List[Tile]) {

  def +(tile: Tile): Unit = {
    tiles = tile :: tiles
  }

  def -(tile: Tile): Unit = {
    tiles = tile :: tiles
  }


  def getPoints(): Int = {
    var points = 0
    if (isValidGroup()) {
      points = tiles.size * tiles.find(x => !x.joker).get.number
    } else if (isValidRun()) {
      tiles.find(x => x.joker == true) match {
        case
          Some(t) => {
          val index = tiles.indexOf(t)
          var jokerpoints = 0
          //joker am anfang
          if (index == 0) {
            jokerpoints = tiles(1).number - 1
            for (i <- (index + 1) to tiles.size - 1) {
              points += tiles(i).number
            }
            points += jokerpoints
          }
        }
        case
          None => {
          for (i <- 0 to tiles.size - 1) {
            points += tiles(i).number

          }
        }




        /*val n = t
        for (i <- (index + 1) to (tiles.size - 1)) {
          if (tiles(index).number != n.number + i) {
            if (tiles(index).joker) {
              points += n.number + i
            }
          } else {
            points += tiles(index).number
          }
        }
        for (i <- (index - 1) to 0) {
          if (tiles(index).number != n.number - i) {
            if (tiles(index).joker) {
              points += n.number - i
            }
          } else {
            points += tiles(index).number
          }
        }*/
      }
    }

    return points
  }

  def isValidRun(): Boolean = {
    if (tiles.size < 3) return false
    if (tiles.groupBy(_.color).size > 1) return false
    var n: List[Tile] = tiles.sortBy(_.number)
    if (tiles.count(x => x.joker) > 0) {
      // TODO: Check if valid with Joke

    } else {
      for (i <- 0 to tiles.size - 2) {
        if (n(i).number + 1 != n(i + 1).number)
          return false
      }
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