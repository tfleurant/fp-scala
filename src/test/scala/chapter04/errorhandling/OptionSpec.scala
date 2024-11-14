package chapter04.errorhandling

import munit.FunSuite

import Option.*

class OptionSpec extends FunSuite {
  test("map should apply when the Option is not None") {
    assertEquals(Some(3).map(_ * 2), Some(6))
  }

  test("map should return None when the Option is None") {
    assertEquals((None: Option[Int]).map(_ * 2), None)
  }

  test("flatMap should apply when the Option is not None") {
    assertEquals(Some(3).flatMap(x => Some(x * 2)), Some(6))
  }

  test("flatMap should return None when the apply function return None") {
    assertEquals(Some(3).flatMap(x => None), None)
  }

  test("flatMap should return None when the Option is None") {
    assertEquals((None: Option[Int]).map(x => Some(x * 2)), Option.None)
  }

  test("getOrElse should return Option value when Option is Some") {
    assertEquals(Some(1).getOrElse(2), 1)
  }

  test("getOrElse should return default value when Option is None") {
    assertEquals((None: Option[Int]).getOrElse(2), 2)
  }

  test("orElse should return original Option when it is Some") {
    assertEquals(Some(1).orElse(Some(2)), Some(1))
  }

  test("orElse should return default Option when original Option is None") {
    assertEquals((None: Option[Int]).orElse(Some(2)), Some(2))
  }

  test("filter should return Some with original value when value validates predicate") {
    assertEquals(Some(1).filter(_ > 0), Some(1))
  }

  test("filter should return None when value don't validate predicate") {
    assertEquals(Some(1).filter(_ <= 0), None)
  }

  test("filter should return None when Option is None") {
    assertEquals((None: Option[Int]).filter(_ <= 0), None)
  }

  test("lift should transform a function to apply it to a non empty option and return non empty value") {
    def f(a: Int): String =
      a.toString
    val liftedF = lift(f)
    assertEquals(liftedF(Some(5)), Some("5"))
  }

  test("lift should transform a function to apply it to an empty option and return an empty value") {
    def f(a: Int): String =
      a.toString

    val liftedF = lift(f)
    assertEquals(liftedF(None), None)
  }

  test("map2 should combine 2 non empty Option and apply function to return non empty Option value") {
    def f(a: Int, b: Double): String =
      a.toString + b.toString

    assertEquals(map2(Some(5), Some(6.2))(f), Some("56.2"))
  }

  test("map2 should combine 2 Option and apply function to return empty Option value when one of the Option is None") {
    def f(a: Int, b: Double): String =
      a.toString + b.toString

    assertEquals(map2(Some(5), None)(f), None)
  }

  test("sequence should return Some(List) when all elements of List are non empty Option values") {
    val data = List(Some(1), Some(2), Some(3))
    assertEquals(sequence(data), Some(List(1, 2, 3)))
  }

  test("sequence should return None when one element of the List is None") {
    val data = List(Some(1), None, Some(3))
    assertEquals(sequence(data), None)
  }

  test("traverse should apply function on list and return Some(List) when all transformed elements are Some") {
    val data = List(1, 2, 3)
    val f: Int => Option[String] = a => if a > 0 then Some(a.toString) else None

    assertEquals(traverse(data)(f), Some(List("1", "2", "3")))
  }

  test("traverse should apply function on list and return None when one of the transformed element is None") {
    val data = List(1, 2, 3)
    val f: Int => Option[String] = a => if a < 3 then Some(a.toString) else None

    assertEquals(traverse(data)(f), None)
  }
}
