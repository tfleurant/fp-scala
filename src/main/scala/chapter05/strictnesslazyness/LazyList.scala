package chapter05.strictnesslazyness

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case LazyList.Empty      => None
    case LazyList.Cons(h, t) => Some(h())

  // Intuitive but not stack safe
  def toListUnsafe: List[A] = this match
    case LazyList.Empty      => List.empty
    case LazyList.Cons(h, t) => h() :: t().toListUnsafe

  def toList: List[A] =
    @tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] =
      ll match
        case LazyList.Empty      => acc.reverse
        case LazyList.Cons(h, t) => go(t(), h() :: acc)
    go(this, Nil)

  def take(n: Int): LazyList[A] =
    this match
      case LazyList.Cons(h, t) if n > 0  => LazyList.Cons(h, () => t().take(n - 1))
      case LazyList.Cons(h, t) if n == 1 => LazyList.Cons(h, () => LazyList.empty)
      case _                             => LazyList.Empty

  def drop(n: Int): LazyList[A] =
    this match
      case LazyList.Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                            => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    this match
      case LazyList.Cons(h, t) if p(h())  => LazyList.Cons(h, () => t().takeWhile(p))
      case LazyList.Cons(h, _) if !p(h()) => LazyList.empty
      case _                              => this

  def exists(p: A => Boolean): Boolean = this match
    case LazyList.Cons(h, t) => p(h()) || t().exists(p)
    case _                   => false

  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case LazyList.Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _                   => acc

  // Not stack safe on large list when everything evaluates to false
  // However, it can terminate early on a true, thanks to foldRight
  def existsByFoldR(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((elem, acc) => p(elem) && acc)

  def takeWhileByFoldR(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.Empty)((elem, acc) => if p(elem) then LazyList.Cons(() => elem, () => acc) else LazyList.empty)

  def headOptionByFoldR: Option[A] =
    foldRight(None)((elem, _) => Some(elem))

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty)((elem, acc) => LazyList.Cons(() => f(elem), () => acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty)((elem, acc) => if p(elem) then LazyList.Cons(() => elem, () => acc) else acc)

  def append[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that)((elem, acc) => LazyList.cons(elem, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty)((elem, acc) => f(elem).append(acc))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapByUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this):
      case LazyList.Empty      => None
      case LazyList.Cons(h, t) => Some((f(h()), t()))

  def takeByUnfold(n: Int): LazyList[A] =
    LazyList.unfold((this, n)):
      case (LazyList.Cons(h, t), n1) if n1 > 0 => Some((h(), (t(), n1 - 1)))
      case _                                   => None

  def takeWhileByUnfold(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this):
      case LazyList.Cons(h, t) if p(h()) => Some((h(), t()))
      case _                             => None

  def zipWith[B](that: LazyList[B]): LazyList[(A, B)] =
    LazyList.unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
      case _                            => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2))        => Some(((None, Some(h2())), (Empty, t2())))
      case (Cons(h1, t1), Empty)        => Some(((Some(h1()), None), (t1(), Empty)))
      case _                            => None

  def startsWith[A1 >: A](prefix: LazyList[A1]): Boolean =
    zipAll(prefix)
      .takeWhile(_(1).isDefined)
      .forAll((a1, a2) => a1 == a2)

  def tails: LazyList[LazyList[A]] =
    LazyList
      .unfold(this):
        case s @ Cons(_, t) => Some(s, t())
        case Empty          => None
      .append(LazyList.empty)

  def hasSubsequence[A1 >: A](l: LazyList[A1]): Boolean =
    tails.exists(_.startsWith(l))

  // We can't use unfold as unfold builds from left to right
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight((init, LazyList(init))) { (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, LazyList.cons(b2, b1(1)))
    }._2

  // I can't seem to find a way to have it working, I always end up in Stack Overflow
//  override def equals(obj: Any): Boolean = obj match
//    case that: LazyList[_] =>
//      that.canEqual(this) && this.toList == that.toList
//    case _ => false
//
//  override def canEqual(that: Any): Boolean = that.isInstanceOf[LazyList[?]]

  // Only used for debug purpose
  override def toString: String = this.toList.toString

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continuallyNaive[A](a: A): LazyList[A] =
    cons(a, continuallyNaive(a))

  // Slightly more efficient, as we on define a Cons once
  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  val fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((value, newState)) => cons(value, unfold(newState)(f))
      case None                    => empty

  lazy val onesByUnfold: LazyList[Int] =
    unfold(1)(s => Some(1, 1))

  def continuallyByUnfold[A](a: A): LazyList[A] =
    unfold(a)(s => Some(a, a))

  def fromByUnfold(n: Int): LazyList[Int] =
    unfold(n)(s => Some(s, s + 1))

  lazy val fibsByUnfold: LazyList[Int] =
    unfold((0, 1))((current, next) => Some(current, (next, current + next)))
