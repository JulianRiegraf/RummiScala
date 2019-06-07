package de.htwg.se.rummi.controller

import de.htwg.se.rummi.model._

import scala.swing.Publisher
import scala.swing.event.Event

class Controller(playerNames: List[String]) extends Publisher{

  val number_of_number_tiles = 104
  val number_of_joker_tiles = 2
  val lowest_number = 1
  val highest_number = 13;

  val playingfield = new Playingfield()
  val players = playerNames.map(x => new Player(x))

  def initGame() = {
    playingfield.generate(players)
    publish(new PlayerSwitchedEvent())
  }

  def getActivePlayer : Player = {
    // TODO: Implement
    throw new NotImplementedError()
  }

  def getPlayingField : RummiSet = {
    // TODO: Implement
    throw new NotImplementedError()
  }

  def getRack(player: Player) : List[Tile] = {
    // TODO: Implement
    throw new NotImplementedError()
  }

  def isValid(): Boolean = {
    // TODO: Implement - Reruns true, if the current state of the playing field is valid
    throw new NotImplementedError()
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
case class WinEvent(winningPlayer: Player) extends Event