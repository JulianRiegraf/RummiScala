package de.htwg.se.rummi.model

import de.htwg.se.rummi.Const
import play.api.libs.json.{JsArray, JsObject, JsValue, Json, Writes}

case class Game(playerNames: List[String]) {


  // Jeder Spieler bewahrt seine Steine in seinem Rack auf
  var racks: Map[Player, Grid] = Map.empty

  // Verdeckte Steine
  var coveredTiles: List[Tile] = Nil

  // Alle Groups oder Runs, die auf dem Spielfeld liegen
  var grid: Grid = Grid(Const.GRID_ROWS, Const.GRID_COLS, Map.empty)

  val players = playerNames.map(x => Player(x))
  var activePlayerIndex: Int = 0
  var isValidField = false

  generateNewGame(players)


  def generateNewGame(players: List[Player]): Unit = {

    grid = Grid(Const.GRID_ROWS, Const.GRID_COLS, Map.empty)
    coveredTiles = Nil
    racks = Map.empty

    for (i <- Const.LOWEST_NUMBER to Const.HIGHEST_NUMBER) {
      coveredTiles = new Tile(i, RED) :: coveredTiles
      coveredTiles = new Tile(i, RED) :: coveredTiles
      coveredTiles = new Tile(i, BLUE) :: coveredTiles
      coveredTiles = new Tile(i, BLUE) :: coveredTiles
      coveredTiles = new Tile(i, GREEN) :: coveredTiles
      coveredTiles = new Tile(i, GREEN) :: coveredTiles
      coveredTiles = new Tile(i, YELLOW) :: coveredTiles
      coveredTiles = new Tile(i, YELLOW) :: coveredTiles
    }

    coveredTiles = (1 to 2 map (i => new Tile(i, WHITE, true))).toList ::: coveredTiles

    coveredTiles = scala.util.Random.shuffle(coveredTiles)

    for (p <- players) {
      // Take 14 tiles and add them to the rack of the player
      var tilesAddToRack = coveredTiles.take(Const.NUMBER_OF_INITIAL_RACK_TILES)

      // Remove the tiles added to the rack from the coveredTiles list
      coveredTiles = coveredTiles.filter(t => !tilesAddToRack.contains(t))

      var map: Map[(Int, Int), Tile] = Map.empty
      var i, j = 1
      while (!tilesAddToRack.isEmpty) {
        map = map + ((i, j) -> tilesAddToRack.head)
        tilesAddToRack = tilesAddToRack.drop(1)
        j += 1
        if (j > Const.RACK_COLS) {
          j = 1
          i += 1
        }
      }
      racks = racks + (p -> Grid(Const.RACK_ROWS, Const.RACK_COLS, map))
    }
  }

  def racksToXml = {
    racks.toList.map(tuple => {
      <rack>
      <player>{tuple._1.name}</player>
        {tuple._2.toXml}
      </rack>
    })
  }

  def racksToJson(): JsArray = {
    JsArray(
      racks.toList.map(tuple => {
        Json.obj(
          "player" -> tuple._1.name,
          "grid" -> tuple._2
        )
      })
    )
  }
  
  def toXml()  = {
    <game>
    <players>{ players.map(p => p.toXml)}</players>
    <racks>{racksToXml}</racks>
    <field>{grid.toXml}</field>
    <activePlayerIndex>{activePlayerIndex}</activePlayerIndex>
    <isValidField>{isValidField}</isValidField>
    <coveredTiles>{coveredTiles.toStream.map(t => t.toXml)}</coveredTiles>
    </game>
  }
}

object Game {

  import play.api.libs.json.Json

  implicit val gameWrites = new Writes[Game] {
    override def writes(o: Game): JsValue = {
      Json.obj(
        "players" -> JsArray(o.players.map(p => p.toJson)),
        "racks" -> o.racksToJson(),
        "field" -> o.grid,
        "activePlayerIndex" -> o.activePlayerIndex,
        "isValidField" -> o.isValidField,
        "coveredTiles" -> o.coveredTiles
      )
    }
  }
  implicit val gameReads = Json.reads[Game]
}