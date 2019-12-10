package de.htwg.se.rummi

import com.google.inject.AbstractModule
import com.google.inject.name.Names
import net.codingwell.scalaguice.ScalaModule
import de.htwg.se.rummi.controller.Controller
import de.htwg.se.rummi.model.gridComponent.GridInterface

class RummiModule extends AbstractModule with ScalaModule {
  override def configure() = {
    // TODO: annotade stuff.
  }
}
