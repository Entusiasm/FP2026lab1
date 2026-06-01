package domain

object ConfigData:
  val initial: Config = Config(
    recipes = Map(
      TeddyBear -> Recipe(Map(Fabric -> 2, Paint -> 1)),
      Doll -> Recipe(Map(Fabric -> 3, Plastic -> 1, Paint -> 2)),
      Car -> Recipe(Map(Plastic -> 4, Wood -> 1, Paint -> 1))
    ),
    materialCosts = Map(Fabric -> 10, Wood -> 15, Plastic -> 5, Paint -> 3),
    shiftDuration = 8,
    allowedDefectRate = 10.0,
    actualDefectProbability = 0.05
  )