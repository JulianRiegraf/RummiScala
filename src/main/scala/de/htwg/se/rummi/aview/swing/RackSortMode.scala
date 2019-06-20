package de.htwg.se.rummi.aview.swing

object RackSortMode extends Enumeration {
  type RackSortMode = Value
  val COLOR, NUMBER, NONE = Value

  def next(currentRackSortMode: RackSortMode): RackSortMode.RackSortMode = {
    val valuesList = values.toList
    val maxIndex: Int = valuesList.size - 1
    val idx = valuesList.indexOf(currentRackSortMode)
    if (idx >= maxIndex) {
      valuesList(0)
    } else {
      valuesList(idx + 1)
    }
  }
}