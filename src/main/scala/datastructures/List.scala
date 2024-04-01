package datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil           => throw new NoSuchElementException("list is empty")
    case Cons(_, tail) => tail
  }

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil           => throw new NoSuchElementException("list is empty")
    case Cons(_, tail) => Cons(newHead, tail)
  }

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil if n > 0           => throw new NoSuchElementException("list is empty")
    case Cons(_, tail) if n > 0 => drop(tail, n - 1)
    case remainder              => remainder
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case x                           => x
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil              => Nil
    case Cons(_, Nil)     => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // This is not stack safe
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sum(ns: List[Int]): Int =
    foldLeft(ns, 0)((x, y) => x + y)

  def product(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((a, _) => a + 1)

  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]())((a, b) => Cons(b, a))

  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, acc) => Cons(a, acc))

  def flatten[A](input: List[List[A]]): List[A] =
    foldRight(input, List[A]())((a, acc) => appendWithFold(a, acc))

  def add1(ns: List[Int]): List[Int] =
    foldRight(ns, List[Int]())((b, acc) => Cons(b + 1, acc))

  def toStringList(ns: List[Double]): List[String] =
    foldRight(ns, List[String]())((b, acc) => Cons(b.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((b, acc) => Cons(f(b), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]()) { (b, acc) =>
      if (f(b)) { Cons(b, acc) }
      else { acc }
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) { a =>
      if (f(a)) { List(a) }
      else { List() }
    }

  def addIntLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil)                     => Nil
    case (Nil, _)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addIntLists(t1, t2))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }

}
