package monads

import monads.Monad

case class IO[A](run: () => A):
  def unsafeRun(): A = run()
  def map[B](f: A => B): IO[B] = IO(() => f(run()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(run()).run())

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  
  given ioMonad: Monad[IO] with
    def pure[A](a: A): IO[A] = IO.pure(a)
    def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)