package domain

// Типы игрушек
sealed trait Product
case object TeddyBear extends Product
case object Doll extends Product
case object Car extends Product

// Типы материалов
sealed trait Material
case object Fabric extends Material
case object Wood extends Material
case object Plastic extends Material
case object Paint extends Material

// Рецепт: сколько единиц каждого материала нужно для одной игрушки
case class Recipe(materials: Map[Material, Int])

// Окружение (конфигурация) для Reader
case class Config(
  recipes: Map[Product, Recipe],
  materialCosts: Map[Material, Int],
  shiftDuration: Int,
  allowedDefectRate: Double,
  actualDefectProbability: Double
)

// Состояние фабрики (State)
case class StateData(
  materials: Map[Material, Int],
  finishedToys: Map[Product, Int],
  defectiveToys: Map[Product, Int],
  shiftHours: Int
)

// Вспомогательная функция: объединение двух словарей с суммированием значений
def combineMaps[K](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] =
  (a.keySet ++ b.keySet).map(k => k -> (a.getOrElse(k, 0) + b.getOrElse(k, 0))).toMap