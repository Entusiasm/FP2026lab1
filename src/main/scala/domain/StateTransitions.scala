package domain

import monads.State
import domain.WriterFunctions._

def receiveMaterialState(mats: Map[Material, Int]): State[StateData, Unit] =
  State.modify(s => s.copy(materials = combineMaps(s.materials, mats)))

def runAssembly(config: Config, state: StateData, product: Product, quantity: Int): (Vector[String], StateData, Either[String, (Int, Int)]) =
  // без изменений
  val recipe = config.recipes(product)
  val enough = recipe.materials.forall { case (m, need) => state.materials.getOrElse(m, 0) >= need * quantity }
  if (!enough) {
    (Vector(s"Не хватает материалов для $product"), state, Left("Недостаточно материалов"))
  } else {
    val newMats = recipe.materials.foldLeft(state.materials) { case (acc, (m, need)) =>
      acc.updated(m, acc(m) - need * quantity)
    }
    val defectCount = (quantity * config.actualDefectProbability).toInt
    val goodCount = quantity - defectCount
    val finished = combineMaps(state.finishedToys, Map(product -> goodCount))
    val defective = combineMaps(state.defectiveToys, Map(product -> defectCount))
    val newState = state.copy(materials = newMats, finishedToys = finished, defectiveToys = defective)
    val cost = productionCost(product, quantity).run(config)
    val (log, _) = logProduction(product, quantity, defectCount, cost).run
    (log, newState, Right((goodCount, defectCount)))
  }

def inspectBatch(state: StateData, config: Config): (Vector[String], Boolean) =
  val totalProduced = state.finishedToys.values.sum + state.defectiveToys.values.sum
  val totalDefective = state.defectiveToys.values.sum
  val defectRate = if totalProduced == 0 then 0.0 else totalDefective.toDouble / totalProduced * 100
  val allowed = isDefectAllowed(defectRate).run(config)
  val (log, _) = logInspection(defectRate, allowed).run
  (log, allowed)

def advanceShift(state: StateData, config: Config): (Vector[String], StateData) =
  val newHours = state.shiftHours + 1
  val shiftEnded = newHours >= config.shiftDuration
  val finalHours = if shiftEnded then 0 else newHours
  val (log, _) = logShiftAdvance(state.shiftHours, finalHours, shiftEnded).run
  (log, state.copy(shiftHours = finalHours))