package monads

// Reader[Env, A] — это вычисление, которое зависит от окружения Env,
// но не изменяет его. Оно просто читает конфигурацию.
// run: Env => A — чистая функция, которая получает окружение и возвращает результат
case class Reader[E, A](run: E => A)

object Reader:
  // pure: поднимает значение в Reader, игнорируя окружение
  def pure[E, A](a: A): Reader[E, A] = Reader(_ => a)           //  поднимает значение в Reader, игнорируя окружение
  def ask[E]: Reader[E, E] = Reader(identity)                   //  возвращает всё окружение как результат
  
  given readerMonad[E]: Monad[[X] =>> Reader[E, X]] with
    def pure[A](a: A): Reader[E, A] = Reader.pure(a)
    def flatMap[A, B](ma: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] =
      Reader(e => f(ma.run(e)).run(e))