package monads

case class Reader[E, A](run: E => A)

object Reader:
  def pure[E, A](a: A): Reader[E, A] = Reader(_ => a)
  def ask[E]: Reader[E, E] = Reader(identity)
  
  given readerMonad[E]: Monad[[X] =>> Reader[E, X]] with
    def pure[A](a: A): Reader[E, A] = Reader.pure(a)
    def flatMap[A, B](ma: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] =
      Reader(e => f(ma.run(e)).run(e))