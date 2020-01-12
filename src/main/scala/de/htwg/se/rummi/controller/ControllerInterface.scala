package de.htwg.se.rummi.controller

import de.htwg.se.rummi.controller.GameState.GameState
import de.htwg.se.rummi.model.{Game, Grid, Player, Tile}

import scala.swing.Publisher

trait ControllerInterface extends Publisher {
  def initGame: Unit

  def initGame(playerNames: List[String]): Unit

  def getRack(activePlayer: Player): Grid

  def getGameState: GameState

  def activePlayer: Player

  def game: Game

  def rackOfActivePlayer: Grid

  def field: Grid

  def moveTile(from: String, to: String): Unit

  def moveTile(gridFrom: Grid, gridTo: Grid, tile: Tile, newRow: Int, newCol: Int): Unit

  def draw: Unit

  def switchPlayer

  def sortRack: Unit

  def save: String

  def redo: Unit

  def undo: Unit

  def updateGrids(gridFrom: Grid, gridTo: Grid, tile: Tile, newRow: Int, newCol: Int): (Grid, Grid)

}
