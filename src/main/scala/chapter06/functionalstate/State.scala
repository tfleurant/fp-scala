package chapter06.functionalstate

// either a type alias: typeState[S, +A] = S => (A, S)
// or case class State[S, +A](run: S => (A, S))
// Or with Scala 3 opaque type:
opaque type State[S, +A] = S => (A, S)

// opaque type has same encapsulation as a case class, and same performances as type alias (no runtime overhead for wrapping function)

object State {
  extension [S, A](underlying: State[S, A]) {
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- underlying
        b <- sb
      } yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] = { s =>
      val (a, s1) = underlying(s)
      f(a)(s1)
    }
  }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get[S]
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight(unit(List.empty[A]))((r, acc) => r.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))
}
