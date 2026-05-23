package monads

import monads.Monad

// IO[A] — это описание эффекта ввода-вывода, который возвращает A.
// Никакой реальный ввод-вывод не выполняется до вызова unsafeRun.
case class IO[A](run: () => A):
   // unsafeRun: запускает эффект и возвращает результат.
  // Название unsafe подчёркивает, что выполнение эффектов нарушает
  // референциальную прозрачность, поэтому должно вызываться только в одном месте
  // (обычно в main).
  def unsafeRun(): A = run()
  def map[B](f: A => B): IO[B] = IO(() => f(run()))                 // преобразует результат эффекта чистой функцией

  // flatMap: последовательно выполняет два эффекта,
  // передавая результат первого во второй
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(run()).run())   

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)                    // pure: поднимает чистое значение в контекст IO (без эффектов)
  
  given ioMonad: Monad[IO] with
    def pure[A](a: A): IO[A] = IO.pure(a)
    def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)