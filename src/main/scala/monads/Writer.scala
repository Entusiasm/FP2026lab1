package monads


// Моноид для типов, поддерживающих пустое значение и ассоциативную операцию.
// Для логов мы использую Vector[String] — он имеет пустой вектор и конкатенацию.
trait Monoid[L]:
  def empty: L
  def combine(a: L, b: L): L

object VectorMonoid extends Monoid[Vector[String]]:
  def empty: Vector[String] = Vector.empty
  def combine(a: Vector[String], b: Vector[String]): Vector[String] = a ++ b

// Writer[L, A] хранит и значение A, и накопленный лог типа L.
// Лог можно вести параллельно с вычислением.
case class Writer[L, A](value: A, log: L):
  // map: применяет функцию к значению, лог остаётся неизменным
  def map[B](f: A => B): Writer[L, B] = Writer(f(value), log) 

  // flatMap извлекает значение, передаёт его в f, объединяет логи   
  def flatMap[B](f: A => Writer[L, B])(using L: Monoid[L]): Writer[L, B] =
    val wb = f(value)
    Writer(wb.value, L.combine(log, wb.log))

   // вспомогательный метод для получения пары (лог, значение)
  def run: (L, A) = (log, value)

object Writer:
  def pure[L, A](a: A)(using L: Monoid[L]): Writer[L, A] = Writer(a, L.empty)  // создаёт Writer с пустым логом
  def tell[L](l: L): Writer[L, Unit] = Writer((), l)                         // добавляет сообщение в лог, результат не имеет смысла (Unit)
  
  // Реализация монады для Writer — использует flatMap из case class.
  given writerMonad[L](using L: Monoid[L]): Monad[[X] =>> Writer[L, X]] with
    def pure[A](a: A): Writer[L, A] = Writer.pure(a)
    def flatMap[A, B](ma: Writer[L, A])(f: A => Writer[L, B]): Writer[L, B] = ma.flatMap(f)