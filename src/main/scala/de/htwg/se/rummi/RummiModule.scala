package de.htwg.se.rummi

import com.google.inject.AbstractModule
import de.htwg.se.rummi.model.fileIoComponent._
import net.codingwell.scalaguice.ScalaModule

class RummiModule extends AbstractModule with ScalaModule {
  override def configure() = {
    bind[FileIoInterface].to[jsonImpl.JsonFileIo]
  }
}
