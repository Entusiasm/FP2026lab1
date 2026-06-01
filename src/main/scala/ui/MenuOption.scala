package ui

import monads.IO
import domain.{StateData, Config}
import plan.Plan

trait MenuOption:
  def title: String
  def execute(state: StateData, config: Config): IO[StateData]

case class MenuLeaf(title: String, action: (StateData, Config) => IO[StateData]) extends MenuOption:
  def execute(state: StateData, config: Config): IO[StateData] = action(state, config)

case class MenuTreeNode(title: String, children: Seq[MenuOption]) extends MenuOption:
  def execute(state: StateData, config: Config): IO[StateData] =
    loop(state, config)

  private def loop(state: StateData, config: Config): IO[StateData] =
    for
      _ <- Plan.showMessage(s"\n=== $title ===")
      _ <- Plan.showMessage(children.zipWithIndex.map { case (opt, i) => s"${i+1} - ${opt.title}" }.mkString("\n"))
      _ <- Plan.showMessage("0 - назад")
      _ <- Plan.showMessage("> ")
      input <- Plan.readCommandString
      cmd <- IO.pure(input.toIntOption)
      newState <- cmd match
        case Some(n) if 1 <= n && n <= children.size =>
          children(n-1).execute(state, config)
        case Some(0) => IO.pure(state)
        case _ => Plan.showMessage("Неверная команда").map(_ => state)
      cont <- IO.pure(!cmd.contains(0))
      res <- if cont then loop(newState, config) else IO.pure(newState)
    yield res