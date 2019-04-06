package de.htwg.se.rummi.model

object RummiColors {

  val ANSI_RESET = "\u001B[0m"

  sealed abstract class Color(name: String, ansiCode: String) {

    def printInColor(text: String): Unit = {
      print(ansiCode + text + ANSI_RESET)
    }

    override def toString = name

  }

  case object RED extends Color("RED", "\u001B[31m")
  case object BLUE extends Color("BLUE", "\u001B[34m")
  case object YELLOW extends Color("YELLOW", "\u001B[33m")
  case object GREEN extends Color("GREEN", "\u001B[32m")

}