package de.htwg.se.rummi.util

trait Command {

  def doStep:Unit
  def undoStep:Unit
  def redoStep:Unit

}