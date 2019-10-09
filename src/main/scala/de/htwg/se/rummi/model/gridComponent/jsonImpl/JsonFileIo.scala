package de.htwg.se.rummi.model.gridComponent.jsonImpl

import de.htwg.se.rummi.model.gridComponent.GridInterface
import de.htwg.se.sudoku.model.fileIoComponent.FileIoInterface
import play.api.libs.json.{JsValue, Json}

class JsonFileIo extends FileIoInterface {
  override def load: GridInterface = ???

  override def save(grid: GridInterface): Unit = {
    import java.io._
    val pw = new PrintWriter(new File("grid.json"))
    pw.write(Json.prettyPrint(gridToJson(grid)))
    pw.close
  }

  def gridToJson(grid: GridInterface): JsValue = {
    // TODO: implement serialization
    Json.obj()
  }


}
