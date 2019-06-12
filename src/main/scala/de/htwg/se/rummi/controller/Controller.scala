package de.htwg.se.rummi.controller

import java.util.NoSuchElementException

import de.htwg.se.rummi.model._

import scala.swing.Publisher
import scala.swing.event.Event

class Controller(playerNames: List[String]) extends Publisher {

  val number_of_number_tiles = 104
  val number_of_joker_tiles = 2
  val lowest_number = 1
  val highest_number = 13

  val MINIMUM_POINTS_FIRST_ROUND = 30
  var statusMessage: String = ""
  var hasDrawn = false

  var tilesMovedFromRacktoGrid: List[Tile] = Nil

  val playingfield = new Playingfield()
  val players = playerNames.map(x => new Player(x))
  var firstMoveList = players
  private var activePlayerIndex: Int = 0

  def initGame() = {
    playingfield.generate(players)
  }

  def getActivePlayer: Player = {
    players(activePlayerIndex)
  }

  // finish
  def switchPlayer(): Unit = {

    if (isValid() == false) {
      return
    }

    activePlayerIndex = activePlayerIndex + 1
    //println("activepolayer: " + activePlayerIndex)
    if (activePlayerIndex >= players.size) {
      activePlayerIndex = 0
    }

    //println("Active Player: " + getActivePlayer.name)
    hasDrawn = false

    // check if playingfield is valid
    statusMessage = ""
    publish(new StatusMessageChangedEvent)
    tilesMovedFromRacktoGrid = Nil
    publish(new PlayerSwitchedEvent)
  }

  def getPlayingField: List[RummiSet] = {
    playingfield.sets
  }

  def getRack(player: Player): List[Tile] = {
    playingfield.racks.find(x => x._1 == player) match {
      case Some(t) => t._2
      case None => {
        println("No Rack of " + player.name)
        throw new NoSuchElementException
      }
    }
  }

  def checkFirstMove(): Boolean = {
    var activePlayer = getActivePlayer
    if(firstMoveList.contains(activePlayer)){
      val sumOfFirstMove = tilesMovedFromRacktoGrid.map(x => x.number).sum
      println("sumoffirstmove: "+ sumOfFirstMove)
      if(sumOfFirstMove <= MINIMUM_POINTS_FIRST_ROUND){
        return false
      } else {
        firstMoveList = firstMoveList.filter(x => x != activePlayer)
        return true
      }
    }
    true
  }

  def isValid(): Boolean = {

    // CHeck if playingfield is valid
    for (s <- playingfield.sets) {
      if (s.isValidRun() == false && s.isValidGroup() == false) {
        statusMessage = "Dein Spielfeld hat fehler, du Pisser!"
        publish(new StatusMessageChangedEvent)
        return false
      }
    }

    // TODO: Joker Logic
    // draws and not played -> valid
    if(tilesMovedFromRacktoGrid.size == 0 && hasDrawn){
      return true
    } else {
      // first move -> 30points or more
      val checker = checkFirstMove()
      println("checker: " + checker)
      if (checker == false){
        statusMessage = "Du musst in deinem ersten Zug 30 oder mehr Punkte legen."
        publish(new StatusMessageChangedEvent)
        return false
      } else {
        if(tilesMovedFromRacktoGrid.size > 0){
          return true
        } else {
          statusMessage = "Du musst ziehen oder Steine legen..."
          publish(new StatusMessageChangedEvent)
          return false
        }
      }
    }
    true
  }


  def getStatusMessage: String = {
    statusMessage
  }

  def draw(): Unit = {

    if (hasDrawn) {
      statusMessage = "Du darfst nur einmal ziehen, du Papnase."
      publish(new StatusMessageChangedEvent)
      return
    }

    if (tilesMovedFromRacktoGrid.size != 0) {
      statusMessage = "Du host scho glegt, du Zipfelklatscher."
      publish(new StatusMessageChangedEvent)
      return
    }

    val newTile = playingfield.coveredTiles(0)
    playingfield.coveredTiles = playingfield.coveredTiles.filter(x => x != newTile)

    var r = playingfield.racks.find(x => x._1 == getActivePlayer) match {
      case Some(r) => r
      case None => throw new NoSuchElementException
    }

    playingfield.racks = playingfield.racks.filter(x => x._1 != getActivePlayer)

    playingfield.racks = (r._1, newTile :: r._2) :: playingfield.racks

    hasDrawn = true
    publish(new RackChangedEvent)
  }

  def moveTile(tile: Tile, setOption: Option[RummiSet]): Unit = {

    // The player moves a tile from the rack or from a set on the playing field to another set
    // set is None, if the tile builds a new set

    playingfield.racks.find(x => x._1 == getActivePlayer) match {
      case Some(rack) => {
        if (rack._2.contains(tile)) {
          println("Tile " + tile + " in rack")
          // Tile is in the rack of current player
          playingfield.racks = playingfield.racks.filter(x => x._1 == getActivePlayer)
          playingfield.racks = (rack._1, rack._2.filter(x => x != tile)) :: playingfield.racks
          tilesMovedFromRacktoGrid = tile :: tilesMovedFromRacktoGrid
          publish(new RackChangedEvent)
        } else {
          println("Tile " + tile + " in grid")

          // Hole Set, welches tile enthält
          val set: RummiSet = playingfield.sets.find(x => x.tiles.contains(tile)).get
          set.tiles = set.tiles.filter(t => t != tile)
          if (set.tiles.size == 0) {
            playingfield.sets = playingfield.sets.filter(x => x != set)
          }
        }


      }
      case None => {
        println("None")
      }
    }

    setOption match {
      case Some(set) => set + tile
      case None => {
        playingfield.sets = new RummiSet(tile :: Nil) :: playingfield.sets
      }
    }


    // A set, which was a run can be changed to a group!

    // check if the set is valid -> ValidStateChangedEvent if necessary

    // Check if the player has won

    statusMessage = ""
    publish(new StatusMessageChangedEvent)
    publish(new FieldChangedEvent)
  }

  def moveTileToRack(tile: Tile): Unit = {
    println("Move " + tile + " to rack.")

    if (!tilesMovedFromRacktoGrid.contains(tile)) {
      statusMessage = "Des darfsch du ned!"
      publish(new StatusMessageChangedEvent)
      return
    }

    val set: RummiSet = playingfield.sets.find(x => x.tiles.contains(tile)).get
    set.tiles = set.tiles.filter(t => t != tile)
    if (set.tiles.size == 0) {
      playingfield.sets = playingfield.sets.filter(x => x != set)
    }
    val rack = playingfield.racks.find(x => x._1 == getActivePlayer).get
    playingfield.racks = playingfield.racks.filter(x => x._1 == getActivePlayer)
    playingfield.racks = (rack._1, tile :: rack._2) :: playingfield.racks

    tilesMovedFromRacktoGrid = tilesMovedFromRacktoGrid.filter(x => x != tile)

    publish(new RackChangedEvent)
    publish(new FieldChangedEvent)
  }


}

case class PlayerSwitchedEvent() extends Event

case class RackChangedEvent() extends Event

case class ValidStateChangedEvent() extends Event

case class FieldChangedEvent() extends Event

case class StatusMessageChangedEvent() extends Event

case class WinEvent(winningPlayer: Player) extends Event