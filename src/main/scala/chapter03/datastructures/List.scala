package chapter03.datastructures

import scala.annotation.tailrec

// +A -> Covrariant in A
// For List[A], List[B], if A is subtype of B, then List[A] is subtype of List[B]
// Without +, the List would be invariant, with Nothing subtype of everything
enum List[+A]:
  case Nil // Scala infers than Nil is List[Nothing]
  case Cons(head: A, tail: List[A]) // :: in standard library

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int = ints match
    case Nil              => 0
    case Cons(head, tail) => head + sum(tail)

  def product(doubles: List[Double]): Double = doubles match
    case Nil              => 1.0
    case Cons(head, tail) => head * product(tail)

  def tail[A](l: List[A]): List[A] = l match
    case Nil              => sys.error("List is empty")
    case Cons(head, tail) => tail

  def setHead[A](l: List[A], newHead: A): List[A] = l match
    case Nil              => sys.error("List is empty, can't replace head")
    case Cons(head, tail) => Cons(newHead, tail)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match
    case List.Cons(head, tail) if n > 0 => List.drop(tail, n - 1)
    case list                           => list

  @tailrec
  def dropWhile[A](l: List[A], predicate: A => Boolean): List[A] = l match
    case List.Cons(head, tail) if predicate(head) => List.dropWhile(tail, predicate)
    case list                                     => list

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    case List.Nil              => a2
    case List.Cons(head, tail) => List.Cons(head, append(tail, a2))

  // This can't be O(1), as we need to go to the end of the List to remove the last element (so O(n))
  def init[A](l: List[A]): List[A] = l match
    case List.Cons(head, tail) if tail != List.Nil => List.Cons(head, init(tail))
    case _                                         => List.Nil

  // As it is not tail recursive, this will result in StackOverflowError on large list
  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match
    case List.Nil              => acc
    case List.Cons(head, tail) => f(head, foldRight(tail, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int = foldRight(ns, 0, (a, b) => a + b)

  // Contrary to product, this implementation needs to evaluate the full list, there is no short circuit (O(n))
  def productViaFoldRight(ns: List[Double]): Double = foldRight(ns, 1.0, (a, b) => a * b)

  def length[A](list: List[A]): Int = list match
    case List.Nil              => 0
    case List.Cons(head, tail) => 1 + List.length(tail)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
    case List.Nil              => acc
    case List.Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](ns: List[A]): Int = foldLeft(ns, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil, (acc, e) => Cons(e, acc))

  // This is stack safe
  def foldRightAsFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B = foldLeft(reverse(as), acc, (b, a) => f(a, b))

  // Not stack safe
  def foldRightAsFoldLeft2[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

  // Not stack safe
  def foldLeftAsFoldRight[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    foldRight(as, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2, (x, acc) => Cons(x, acc))

  def flatten[A](ll: List[List[A]]): List[A] = foldLeft(ll, Nil, (acc, x) => List.append(acc, x))

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil, (x, acc) => List.Cons(x + 1, acc))

  def doublesToStrings(l: List[Double]): List[String] = foldRight(l, Nil, (x, acc) => List.Cons(x.toString, acc))

  def map[A, B](as: List[A], f: A => B): List[B] = foldRight(as, Nil: List[B], (x, acc) => List.Cons(f(x), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (x, acc) => if f(x) then Cons(x, acc) else acc)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = foldRight(as, Nil, (x, acc) => append(f(x), acc))

  // A simpler implementation
  def flatMapSimple[A, B](as: List[A], f: A => List[B]): List[B] = flatten(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, x => if f(x) then List(x) else Nil)

  def addPairWise(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match
      case (Nil, _)                                 => Nil
      case (_, Nil)                                 => Nil
      case (Cons(headA, tailA), Cons(headB, tailB)) => Cons(headA + headB, addPairWise(tailA, tailB))

  // This is not stack safe, as we are not tailrec
  def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] =
    (as, bs) match
      case (Nil, _)                                 => Nil
      case (_, Nil)                                 => Nil
      case (Cons(headA, tailA), Cons(headB, tailB)) => Cons(f(headA, headB), zipWith(tailA, tailB, f))

  // The accumulated value is passed in recursing call instead of first recursing
  def zipWithTailRec[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] =
    @tailrec
    def loop(as: List[A], bs: List[B], acc: List[C]): List[C] =
      (as, bs) match
        case (Nil, _)                                 => Nil
        case (_, Nil)                                 => Nil
        case (Cons(headA, tailA), Cons(headB, tailB)) => loop(tailA, tailB, Cons(f(headA, headB), acc))
    reverse(loop(as, bs, Nil))

  @tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
      case (Cons(headA, tailA), Cons(headB, tailB)) if headA == headB => startsWith(tailA, tailB)
      case (_, Nil)                                                   => true
      case (Nil, _)                                                   => false
      case _                                                          => false

  // Intuitive version
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
      case (a, b) if startsWith(a, b) => true
      case (Cons(headA, tailA), b)    => hasSubsequence(tailA, b)
      case _                          => false

  // Book version
  @tailrec
  def startsWithAlt[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
      case (_, Nil)                                                   => true
      case (Cons(headA, tailA), Cons(headB, tailB)) if headA == headB => startsWithAlt(tailA, tailB)
      case _                                                          => false

  @tailrec
  def hasSubsequenceAlt[A](sup: List[A], sub: List[A]): Boolean =
    sup match
      case List.Nil                     => sub == List.Nil
      case _ if startsWithAlt(sup, sub) => true
      case List.Cons(head, tail)        => hasSubsequenceAlt(tail, sub)
