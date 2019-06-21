package de.htwg.se.rummi.model


object RummiColor{

  val ANSI_RESET = "\u001B[0m"

  sealed abstract class Color(val name: String, val ansiCode: String) extends Ordered[Color]{


    def printInColor(text: String): Unit = {
      print(ansiCode + text + ANSI_RESET)
    }

    def stringInColor(text: String): String = {
      ansiCode + text + ANSI_RESET
    }

    override def toString = name

    override def compare(that: Color): Int = {
      this.name.compareTo(that.name)
    }

  }

  case object RED extends Color("RED", "\u001B[31m")
  case object BLUE extends Color("BLUE", "\u001B[34m")
  case object YELLOW extends Color("YELLOW", "\u001B[33m")
  case object GREEN extends Color("GREEN", "\u001B[32m")

}