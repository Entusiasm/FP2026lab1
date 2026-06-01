package plan

import monads.IO
import monads.IO.given
import domain.*

object Plan:
  def showMenu: IO[Unit] = IO { () =>
    println("\n=== МЕНЮ ===")
    println("1 - Получить материалы")
    println("2 - Собрать партию игрушек")
    println("3 - Инспекция брака")
    println("4 - Перейти к следующему часу смены")
    println("5 - Показать состояние")
    println("0 - Выход")
    print("> ")
  }

  def readCommand: IO[Int] = IO { () =>
    scala.io.StdIn.readInt()
  }

  def readProduct: IO[Option[Product]] = IO { () =>
    println("Выберите товар: 1 - Мишка, 2 - Кукла, 3 - Машинка")
    scala.io.StdIn.readInt() match
      case 1 => Some(TeddyBear)
      case 2 => Some(Doll)
      case 3 => Some(Car)
      case _ => None
  }

  def readMaterial: IO[Option[Material]] = IO { () =>
    println("Выберите материал: 1 - Ткань, 2 - Дерево, 3 - Пластик, 4 - Краска")
    scala.io.StdIn.readInt() match
      case 1 => Some(Fabric)
      case 2 => Some(Wood)
      case 3 => Some(Plastic)
      case 4 => Some(Paint)
      case _ => None
  }

  def readQuantity: IO[Int] = IO { () =>
    print("Введите количество: ")
    scala.io.StdIn.readInt()
  }

  def showLog(log: Vector[String]): IO[Unit] = IO { () =>
    log.foreach(println)
  }

  def showMessage(msg: String): IO[Unit] = IO { () =>
    println(msg)
  }
  
  def showState(state: StateData, config: Config): IO[Unit] = IO { () =>
    println("\n=== ТЕКУЩЕЕ СОСТОЯНИЕ ===")
    println(s"Материалы: ${state.materials}")
    println(s"Готовые игрушки: ${state.finishedToys}")
    println(s"Брак: ${state.defectiveToys}")
    println(s"Часы смены: ${state.shiftHours}/${config.shiftDuration}")
    println("===========================\n")
  }

  def readCommandString: IO[String] = IO(() => scala.io.StdIn.readLine())