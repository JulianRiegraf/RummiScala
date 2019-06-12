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
          var jokerPoints = 0
          var secondJokerPoints = 0
          //joker am anfang
          if (index == 0) {
            // jokerPoints = tiles(1).number - 1
            for (i <- (index + 1) to tiles.size - 1) {
              if (tiles(i).joker) {
                if (i == 1) {
                  //2. joker an stelle 2:
                  jokerPoints = tiles(index + 2).number - 2
                  secondJokerPoints = tiles(i + 1).number -1
                } else {
                  secondJokerPoints = tiles(i - 1).number + 1
                }
              }
              jokerPoints = tiles(index + 1).number - 1
              points += tiles(i).number
            }
            points = jokerPoints + secondJokerPoints + points
          }
          //joker am ende
        }
        case
          // kein joker enthalten
          None => {
          for (i <- 0 to tiles.size - 1) {
            points += tiles(i).number
          }
        }
      }
    }

    return points
  }

  def isValidRun(): Boolean = {
    if (tiles.size < 3) return false
    if (tiles.groupBy(_.color).size > 1) return false
    var n: List[Tile] = tiles.sortBy(_.number)
    if (tiles.count(x => x.joker) > 0) {
      // TODO: Check if valid with Joker

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