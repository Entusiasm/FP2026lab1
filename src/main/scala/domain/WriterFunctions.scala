package domain

import monads.Writer

object WriterFunctions:

  def logReceiveMaterial(materials: Map[Material, Int]): Writer[Vector[String], Unit] =
    Writer.tell(Vector(s"Получены материалы: ${materials.mkString(", ")}"))

  def logProduction(product: Product, quantity: Int, defectCount: Int, cost: Int): Writer[Vector[String], Unit] =
    Writer.tell(Vector(
      s"Производство: $quantity шт. $product",
      s"  - дефектных: $defectCount",
      s"  - стоимость: $cost руб.",
      s"  - брак: ${defectCount.toDouble / quantity * 100}%"
    ))

  def logInspection(defectRate: Double, allowed: Boolean): Writer[Vector[String], Unit] =
    val msg = if allowed then s"Уровень брака $defectRate% в норме." else s"Уровень брака $defectRate% ПРЕВЫШЕН!"
    Writer.tell(Vector(s"Инспекция: $msg"))

  def logShiftAdvance(prevHours: Int, newHours: Int, shiftEnded: Boolean): Writer[Vector[String], Unit] =
    val msg = s"Смена: $prevHours -> $newHours час(ов)"
    if shiftEnded then Writer.tell(Vector(msg, "=== СМЕНА ЗАВЕРШЕНА ==="))
    else Writer.tell(Vector(msg))