package chapter04.errorhandling

enum Validated[+E, +A]:
  case Valid(get: A)
  case Invalid(errors: E)

  def toEither: Either[E, A] = this match
    case Validated.Valid(get)      => Either.Right(get)
    case Validated.Invalid(errors) => Either.Left(errors)

  def map[B](f: A => B): Validated[E, B] = this match
    case Validated.Valid(get)      => Valid(f(get))
    case Validated.Invalid(errors) => Invalid(errors)

  def map2[EE >: E, B, C](b: Validated[EE, B])(f: (A, B) => C)(combineErrors: (EE, EE) => EE): Validated[EE, C] =
    (this, b) match
      case (Valid(aa), Valid(bb))       => Valid(f(aa, bb))
      case (Invalid(es), Valid(_))      => Invalid(es)
      case (Valid(_), Invalid(es))      => Invalid(es)
      case (Invalid(es1), Invalid(es2)) => Invalid(combineErrors(es1, es2))

object Validated:
  def fromEither[E, A](e: Either[E, A]): Validated[E, A] =
    e match
      case Either.Left(value)  => Invalid(value)
      case Either.Right(value) => Valid(value)

  def traverse[E, A, B](as: List[A], f: A => Validated[E, B], combineErrors: (E, E) => E): Validated[E, List[B]] =
    as.foldRight(Valid(Nil): Validated[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _)(combineErrors))

  def sequence[E, A](as: List[Validated[E, A]], combineErrors: (E, E) => E): Validated[E, List[A]] =
    traverse(as, identity, combineErrors)
