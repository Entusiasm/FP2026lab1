package monads

import monads.Monad

case class State[S, A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] = State(s => { val (a, s1) = run(s); (f(a), s1) })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => { val (a, s1) = run(s); f(a).run(s1) })

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (a, s))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = State(s => ((), f(s)))
  
  given stateMonad[S]: Monad[[X] =>> State[S, X]] with
    def pure[A](a: A): State[S, A] = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)