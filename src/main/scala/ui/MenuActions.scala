package ui

import monads.IO
import domain.{StateData, Config, Material, Product, TeddyBear, Doll, Car, Fabric, Wood, Plastic, Paint}
import domain.{receiveMaterialState, runAssembly, inspectBatch, advanceShift}
import domain.WriterFunctions.logReceiveMaterial
import plan.Plan

object MenuActions:

  def receiveMaterialAction(state: StateData, config: Config): IO[StateData] =
    for
      maybeMat <- Plan.readMaterial
      newState <- maybeMat match
        case Some(mat) =>
          for
            qty <- Plan.readQuantity
            ns <- if qty > 0 then
              val mats = Map(mat -> qty)
              val writer = logReceiveMaterial(mats)
              val nextState = receiveMaterialState(mats).run(state)._2   // новое имя
              for
                _ <- Plan.showLog(writer.log)
                _ <- Plan.showMessage("Материалы добавлены.")
              yield nextState
            else
              Plan.showMessage("Количество должно быть положительным").map(_ => state)
          yield ns
        case None =>
          Plan.showMessage("Неверный материал").map(_ => state)
    yield newState

  def assembleAction(state: StateData, config: Config): IO[StateData] =
    for
      maybeProduct <- Plan.readProduct
      newState <- maybeProduct match
        case Some(product) =>
          for
            qty <- Plan.readQuantity
            ns <- if qty > 0 then
              val (log, nextState, result) = runAssembly(config, state, product, qty)
              for
                _ <- Plan.showLog(log)
                _ <- result match
                  case Right((good, defect)) =>
                    Plan.showMessage(s"Произведено: $good годных, $defect бракованных.")
                  case Left(err) =>
                    Plan.showMessage(s"Ошибка: $err")
              yield nextState
            else
              Plan.showMessage("Количество должно быть положительным").map(_ => state)
          yield ns
        case None =>
          Plan.showMessage("Неверный товар").map(_ => state)
    yield newState

  def inspectAction(state: StateData, config: Config): IO[StateData] =
    for
      (log, allowed) <- IO.pure(inspectBatch(state, config))
      _ <- Plan.showLog(log)
      _ <- if !allowed then Plan.showMessage("ВНИМАНИЕ: брак превышен!") else IO.pure(())
    yield state

  def advanceAction(state: StateData, config: Config): IO[StateData] =
    for
      (log, nextState) <- IO.pure(advanceShift(state, config))
      _ <- Plan.showLog(log)
      _ <- if nextState.shiftHours == 0 && log.exists(_.contains("СМЕНА ЗАВЕРШЕНА"))
           then Plan.showMessage("Смена окончена.")
           else IO.pure(())
    yield nextState

  def showStateAction(state: StateData, config: Config): IO[StateData] =
    Plan.showState(state, config).map(_ => state)

  def mainMenu(config: Config): MenuTreeNode =
    MenuTreeNode("ГЛАВНОЕ МЕНЮ", Seq(
      MenuLeaf("Получить материалы", receiveMaterialAction),
      MenuLeaf("Собрать партию игрушек", assembleAction),
      MenuLeaf("Инспекция брака", inspectAction),
      MenuLeaf("Перейти к следующему часу смены", advanceAction),
      MenuLeaf("Показать состояние", showStateAction)
    ))