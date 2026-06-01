package ui

import monads.IO
import domain.{ConfigData, InitialState}

object MainMenu:
  def run(): Unit =
    val config = ConfigData.initial
    val initialState = InitialState.data
    val menu = MenuActions.mainMenu(config)
    val finalState = menu.execute(initialState, config).unsafeRun()
    println(s"\nПрограмма завершена.")