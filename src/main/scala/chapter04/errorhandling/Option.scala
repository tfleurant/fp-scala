package chapter04.errorhandling

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Option.Some(get) => Some(f(get))
    case Option.None      => Option.None

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
  // B >: A  B must be a supertype of A
  def getOrElse[B >: A](default: => B): B = this match
    case Option.Some(get) => get
    case Option.None      => default

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = flatMap(a => if f(a) then Some(a) else None)

object Option:
  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match
    case (Some(resolvedA), Some(resolvedB)) => Some(f(resolvedA, resolvedB))
    case _                                  => None

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case ::(head, next) => head.flatMap(h => sequence(next).map(l => h :: l))
    case Nil            => Some(List.empty)

  // Another (better) way to write it, without having to handle recursion ourselves
  def sequenceAlt[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  // Should only look at the full list once
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case ::(head, next) => f(head).flatMap(x => traverse(next)(f).map(l => x :: l))
    case Nil            => Some(Nil)

  // Same, with foldRight
  def traverseAlt[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))

  // sequence using traverse
  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverseAlt(as)(identity)
