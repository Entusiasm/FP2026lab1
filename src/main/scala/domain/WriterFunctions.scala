package domain

import monads.{Writer, VectorMonoid}
import monads.Writer.given

def logProduction(product: Product, quantity: Int, defectCount: Int, cost: Int): Writer[Vector[String], Unit] =
  Writer.tell(Vector(
    s"Производство: $quantity шт. товара $product",
    s"  - дефектных: $defectCount",
    s"  - стоимость: $cost руб.",
    s"  - брак составил ${defectCount.toDouble / quantity * 100}%"
  ))

def logReceiveMaterial(materials: Map[Material, Int]): Writer[Vector[String], Unit] =
  Writer.tell(Vector(s"Получены материалы: ${materials.mkString(", ")}"))

def logInspection(defectRate: Double, allowed: Boolean): Writer[Vector[String], Unit] =
  val msg = if (allowed) s"Уровень брака $defectRate% в пределах нормы." else s"Уровень брака $defectRate% ПРЕВЫШАЕТ норму!"
  Writer.tell(Vector(s"Инспекция: $msg"))

def logShiftAdvance(prevHours: Int, newHours: Int, shiftEnded: Boolean): Writer[Vector[String], Unit] =
  val msg = s"Смена: $prevHours -> $newHours час(ов)"
  if (shiftEnded) Writer.tell(Vector(msg, "=== СМЕНА ЗАВЕРШЕНА ==="))
  else Writer.tell(Vector(msg))