package chapter04.errorhandling

import munit.FunSuite

import Either._

class EitherSpec extends FunSuite {
  test("map should apply function to Either when it is a Right") {
    assertEquals(Right(3).map(_ * 2), Right(6))
  }

  test("map should not apply function to Either when it is a Left") {
    assertEquals((Left(3): Either[Int, Int]).map(x => x * 2), Left(3))
  }

  test("flatMap should apply function to Either when it is a Right, and return a Right when function return a Right") {
    assertEquals(Right(3).flatMap(x => Right(x * 2)), Right(6))
  }

  test("flatMap should apply function to Either when it is a Right, and return a Left when function return a Left") {
    assertEquals(Right(3).flatMap(_ => Left(3): Either[Int, Int]), Left(3))
  }

  test("flatMap should not apply function to Either when it is a Left") {
    assertEquals((Left(3): Either[Int, Int]).flatMap(x => Right(x * 2)), Left(3))
  }

  test("orElse should return original Either when it is a Right") {
    assertEquals(Right(3).orElse(Right(6)), Right(3))
  }

  test("orElse should return provided value when original value is a Left") {
    assertEquals(Left(3).orElse(Right(6)), Right(6))
  }

  test("map2 should apply function when both values are Right") {
    assertEquals(Right(3).map2(Right(6))(_ * _), Right(18))
  }

  test("map2 should return first Left when one of the values is a Left") {
    assertEquals((Left(3): Either[Int, Int]).map2(Right(6))(_ * _), Left(3))
  }

  test("sequence should return the first Left") {
    // munit doesn't have many matchers, need to rely on string...
    assertNoDiff(
      Either.sequence(List(Right(1), Right(2), Left(new RuntimeException("")), Right(4))).toString,
      Left(new RuntimeException("")).toString,
    )
  }
  test("sequence should return a Right of List when no Left in List") {
    assertEquals(
      Either.sequence(List(Right(1), Right(2), Right(4))),
      Right(List(1, 2, 4)),
    )
  }

  test("traverse should return the first Left generated by function") {
    // munit doesn't have many matchers, need to rely on string...
    assertNoDiff(
      Either
        .traverse(List(1, 2, 4))({
          case x if x > 2 => Left(new RuntimeException(""))
          case x          => Right(x * 2)
        })
        .toString,
      Left(new RuntimeException("")).toString,
    )
  }

  test("traverse should return a Right of List when no Left returned by function") {
    assertEquals(
      Either.traverse(List(1, 2, 4))(x => Right(x * 2)),
      Right(List(2, 4, 8)),
    )
  }
}
