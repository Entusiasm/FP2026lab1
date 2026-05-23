package monads

trait Monoid[L]:
  def empty: L
  def combine(a: L, b: L): L

object VectorMonoid extends Monoid[Vector[String]]:
  def empty: Vector[String] = Vector.empty
  def combine(a: Vector[String], b: Vector[String]): Vector[String] = a ++ b

case class Writer[L, A](value: A, log: L):
  def map[B](f: A => B): Writer[L, B] = Writer(f(value), log)
  def flatMap[B](f: A => Writer[L, B])(using L: Monoid[L]): Writer[L, B] =
    val wb = f(value)
    Writer(wb.value, L.combine(log, wb.log))
  def run: (L, A) = (log, value)

object Writer:
  def pure[L, A](a: A)(using L: Monoid[L]): Writer[L, A] = Writer(a, L.empty)
  def tell[L](l: L): Writer[L, Unit] = Writer((), l)
  
  given writerMonad[L](using L: Monoid[L]): Monad[[X] =>> Writer[L, X]] with
    def pure[A](a: A): Writer[L, A] = Writer.pure(a)
    def flatMap[A, B](ma: Writer[L, A])(f: A => Writer[L, B]): Writer[L, B] = ma.flatMap(f)