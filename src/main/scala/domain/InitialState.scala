package domain

object InitialState:
  val data: StateData = StateData(
    materials = Map(Fabric -> 50, Wood -> 20, Plastic -> 100, Paint -> 30),
    finishedToys = Map.empty,
    defectiveToys = Map.empty,
    shiftHours = 0
  )