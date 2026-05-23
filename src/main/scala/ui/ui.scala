package ui

import monads.IO
import monads.IO.given
import domain.*
import domain.StateTransitions.*
import plan.Plan

class UI:
  private val config = Config(
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

  private var state = StateData(
    materials = Map(Fabric -> 50, Wood -> 20, Plastic -> 100, Paint -> 30),
    finishedToys = Map.empty,
    defectiveToys = Map.empty,
    shiftHours = 0
  )

  private def handleCommand(cmd: Int): IO[Boolean] = cmd match
    case 1 => // Получить материалы
      for
        maybeMat <- Plan.readMaterial
        result <- maybeMat match
          case Some(mat) =>
            for
              qty <- Plan.readQuantity
              _ <- if qty > 0 then
                val mats = Map(mat -> qty)
                val (log, _) = logReceiveMaterial(mats).run
                for
                  _ <- Plan.showLog(log)
                  _ <- IO { () => state = receiveMaterial(mats).run(state)._2 }
                  _ <- Plan.showMessage("Материалы добавлены.")
                yield ()
              else Plan.showMessage("Количество должно быть положительным")
            yield ()
          case None => Plan.showMessage("Неверный материал")
        running <- IO.pure(true)
      yield running

    case 2 => // Собрать партию
      for
        maybeProduct <- Plan.readProduct
        result <- maybeProduct match
          case Some(product) =>
            for
              qty <- Plan.readQuantity
              _ <- if qty > 0 then
                val (log, newState, assemblyResult) = runAssembly(config, state, product, qty)
                for
                  _ <- Plan.showLog(log)
                  _ <- assemblyResult match
                    case Right((good, defect)) =>
                      IO { () =>
                        state = newState
                        println(s"Успешно произведено: $good шт. годных, $defect шт. бракованных.")
                      }
                    case Left(err) => Plan.showMessage(s"Ошибка: $err")
                yield ()
              else Plan.showMessage("Количество должно быть положительным")
            yield ()
          case None => Plan.showMessage("Неверный товар")
        running <- IO.pure(true)
      yield running

    case 3 => // Инспекция брака
      for
        (log, allowed) <- IO.pure(inspectBatch(state, config))
        _ <- Plan.showLog(log)
        _ <- if !allowed then Plan.showMessage("ВНИМАНИЕ: уровень брака превышает допустимый!")
             else IO.pure(())
        running <- IO.pure(true)
      yield running

    case 4 => // Следующий час смены
      for
        (log, newState) <- IO.pure(advanceShift(state, config))
        _ <- Plan.showLog(log)
        _ <- IO { () => state = newState }
        _ <- if state.shiftHours == 0 && log.exists(_.contains("СМЕНА ЗАВЕРШЕНА"))
             then Plan.showMessage("Смена окончена. Начните новую смену.")
             else IO.pure(())
        running <- IO.pure(true)
      yield running

    case 5 => // Показать состояние
      for
        _ <- Plan.showState(state, config)
        running <- IO.pure(true)
      yield running

    case 0 => // Выход
      for
        _ <- Plan.showMessage("До свидания!")
        running <- IO.pure(false)
      yield running

    case _ => // Неверная команда
      for
        _ <- Plan.showMessage("Неверная команда")
        running <- IO.pure(true)
      yield running

  def run(): IO[Unit] = for
    _ <- Plan.showMenu
    cmd <- Plan.readCommand
    running <- handleCommand(cmd)
    _ <- if running then run() else IO.pure(())
  yield ()