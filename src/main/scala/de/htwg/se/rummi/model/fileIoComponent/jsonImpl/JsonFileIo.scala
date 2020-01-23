package de.htwg.se.rummi.model.fileIoComponent.jsonImpl

import de.htwg.se.rummi.model.Game
import de.htwg.se.rummi.model.fileIoComponent.FileIoInterface
import play.api.libs.json.{JsValue, Json}

class JsonFileIo extends FileIoInterface {
  override def load: Game = ???

  override def save(game: Game): String = {
    import java.io._
    val pw = new PrintWriter(new File("grid.json"))
    val json = Json.prettyPrint(gameToJson(game))
    pw.write(json)
    pw.close
    json
  }

  def gameToJson(game: Game): JsValue = {
    Json.toJson(game)
  }


}
