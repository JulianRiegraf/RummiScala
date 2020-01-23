package de.htwg.se.rummi.model

import de.htwg.se.rummi.controller.controllerBaseImpl.Controller
import de.htwg.se.rummi.util.UndoManager
import org.scalatest.{Matchers, WordSpec}

class GameSpec extends WordSpec with Matchers {

  "A game" should {

    var playerNames: List[String] = List("patrick", "julian")

    var controller = new Controller()
    controller.initGame(playerNames)

    "some testing" in {

    }


  }
}
