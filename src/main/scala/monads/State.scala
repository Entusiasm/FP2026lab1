package monads

import monads.Monad

// State[S, A] — это чистая функция из старого состояния S
// в пару (новое состояние S, результат A).
case class State[S, A](run: S => (A, S)):
  // map: изменяет результат, состояние не меняет
  def map[B](f: A => B): State[S, B] = State(s => { val (a, s1) = run(s); (f(a), s1) })

  // flatMap: последовательно выполняет два состояния,
  // передавая промежуточное состояние из первого во второй.
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => { val (a, s1) = run(s); f(a).run(s1) })

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (a, s))                // возвращает результат, не меняя состояние
  def get[S]: State[S, S] = State(s => (s, s))                          // возвращает текущее состояние как результат (состояние не меняется)
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))                // Устанавливает новое состояние, возвращает Unit (старое состояние игнорируется)
  def modify[S](f: S => S): State[S, Unit] = State(s => ((), f(s)))     //применяет функцию к состоянию, возвращает Unit
  
  given stateMonad[S]: Monad[[X] =>> State[S, X]] with
    def pure[A](a: A): State[S, A] = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)