package de.htwg.se.rummi.controller

import java.util.NoSuchElementException

import de.htwg.se.rummi.model._

import scala.swing.Publisher
import scala.swing.event.Event

class Controller(playerNames: List[String]) extends Publisher {


  val number_of_number_tiles = 104
  val number_of_joker_tiles = 2
  val lowest_number = 1
  val highest_number = 13;

  val MINIMUM_POINTS_FIRST_ROUND = 30
  var statusMessage: String = ""

  val playingfield = new Playingfield()
  val players = playerNames.map(x => new Player(x))
  var activePlayerIndex = 0

  def initGame() = {
    playingfield.generate(players)
    publish(new PlayerSwitchedEvent())
  }

  def getActivePlayer: Player = {
    players(activePlayerIndex)
  }

  def switchPlayer(): Unit = {

    // TODO Check valid state

    activePlayerIndex += 1
    if (activePlayerIndex >= players.size) {
      activePlayerIndex = 0
    }

    // check if playingfield is valid
  }

  def getPlayingField: List[RummiSet] = {
    playingfield.sets
  }

  def getRack(player: Player): List[Tile] = {
    playingfield.racks.find(x => x._1 == player) match {
      case Some(t) => t._2
      case None => throw new NoSuchElementException
    }
  }

  def isValid(): Boolean = {

    // TODO: Check if minimum points in first turn are reached

    // TODO: If player draw a tile and does not make changes to the playingfield => also valid

    // CHeck if playingfield is valid
    for (s <- playingfield.sets) {
      if (s.isValid() == false) {
        return false
      }
    }
    true
  }


  def getStatusMessage: String = {
    statusMessage
  }

  def getTile(): Unit = {
    val newTile = playingfield.coveredTiles(0)
    playingfield.coveredTiles = playingfield.coveredTiles.filter(x => x != newTile)

    var r = playingfield.racks.find(x => x._1 == getActivePlayer) match {
      case Some(r) => r
      case None => throw new NoSuchElementException
    }

    playingfield.racks = playingfield.racks.filter(x => x._1 == getActivePlayer)

    playingfield.racks = (r._1, newTile :: r._2) :: playingfield.racks
    publish(new RackChangedEvent)
  }

  def moveTile(tile: Tile, setOption: Option[RummiSet]): Unit = {

    // The player moves a tile from the rack or from a set on the playing field to another set
    // set is None, if the tile builds a new set

    setOption match {
      case Some(set) => set + tile // TODO: Implement
      case None => new Run(tile :: Nil)
    }

    // A set, which was a run can be changed to a group!

    // check if the set is valid -> ValidStateChangedEvent if necessary

    // Check if the player has won

  }
}

case class PlayerSwitchedEvent() extends Event

case class RackChangedEvent() extends Event

case class ValidStateChangedEvent() extends Event

case class FieldChangedEvent() extends Event

case class StatusMessageChangedEvent() extends Event

case class WinEvent(winningPlayer: Player) extends Event