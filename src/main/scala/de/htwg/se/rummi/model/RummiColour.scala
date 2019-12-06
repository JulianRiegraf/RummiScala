package de.htwg.se.rummi.model

case class RummiColour(val name: String, val ansiCode: String) extends Ordered[RummiColour]{

  val ANSI_RESET = "\u001B[0m"

    def stringInColor(text: String): String = {
      ansiCode + text + ANSI_RESET
    }

    override def toString = name

    override def compare(that: RummiColour): Int = {
      this.name.compareTo(that.name)
    }

}
  object RED extends RummiColour("RED", "\u001B[31m")
  object BLUE extends RummiColour("BLUE", "\u001B[34m")
  object YELLOW extends RummiColour("YELLOW", "\u001B[33m")
  object GREEN extends RummiColour("GREEN", "\u001B[32m")
  object WHITE extends RummiColour("WHITE", "\u001B[37m")

object RummiColour{
  import play.api.libs.json._
  implicit val colorWrites = Json.writes[RummiColour]
  implicit val colortileReads = Json.reads[RummiColour]
}

