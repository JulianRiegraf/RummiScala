package de.htwg.se.rummi.model

import de.htwg.se.rummi.model.RummiColor.{BLUE, GREEN, RED, YELLOW}

case class Playingfield() {
  
  val LOWEST_NUMBER = 1
  val HIGHEST_NUMBER = 13

  val NUMBER_OF_INITIAL_RACK_TILES: Int = 14

  // Jeder Spieler bewahrt seine Steine in seinem Rack auf
  var racks: List[(Player, List[Tile])] = Nil

  // Verdeckte Steine
  var coveredTiles: List[Tile] = Nil

  // Alle Groups oder Runs, die auf dem Spielfeld liegen
  var sets: List[RummiSet] = Nil

  def generateNewGame(players: List[Player]): Unit = {

    sets = Nil
    coveredTiles = Nil
    racks = Nil

    for(i <- LOWEST_NUMBER to HIGHEST_NUMBER){
      coveredTiles = new Tile(i, RED) :: coveredTiles
      coveredTiles = new Tile(i, RED) :: coveredTiles
      coveredTiles = new Tile(i, BLUE) :: coveredTiles
      coveredTiles = new Tile(i, BLUE) :: coveredTiles
      coveredTiles = new Tile(i, GREEN) :: coveredTiles
      coveredTiles = new Tile(i, GREEN) :: coveredTiles
      coveredTiles = new Tile(i, YELLOW) :: coveredTiles
      coveredTiles = new Tile(i, YELLOW) :: coveredTiles
    }

    coveredTiles = (1 to 2 map(i => new Tile(i, YELLOW, true))).toList ::: coveredTiles

    coveredTiles = scala.util.Random.shuffle(coveredTiles)

    for (p <- players) {
      // Take 14 tiles and add them to the rack of the player
      val tilesAddToRack = coveredTiles.take(NUMBER_OF_INITIAL_RACK_TILES)
      racks = (p, tilesAddToRack) :: racks

      // Remove the tiles added to the rack from the coveredTiles list
      coveredTiles = coveredTiles.filter(t => !tilesAddToRack.contains(t))
    }
  }
}
