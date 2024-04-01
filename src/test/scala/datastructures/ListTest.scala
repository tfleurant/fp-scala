package datastructures

import munit.FunSuite

class ListTest extends FunSuite {

  test("drop should return tail after x element") {
    val input = List(1, 2, 3)
    assertEquals(List(3), List.drop(input, 2))
  }

  test("drop should throw an error when too many elements are dropped") {
    val input = List(1)
    intercept[NoSuchElementException](List.drop(input, 2))
  }

  test("dropWhile should return list") {
    val input = List(1, 2, 3)
    assertEquals(List(3), List.dropWhile(input)(i => i < 3))
    assertEquals(List(1, 2, 3), List.dropWhile(input)(i => i < 0))
  }

  test("init should return list") {
    val input = List(1, 2, 3)
    assertEquals(List(1, 2), List.init(input))
  }

  test("length should compute list length") {
    val input = List(1, 2, 3)
    assertEquals(3, List.length(input))
  }

  test("sum") {
    val input = List(1, 2, 3)
    assertEquals(6, List.sum(input))
  }

  test("product") {
    val input = List(1.0, 2.1, 3.2)
    assertEqualsDouble(6.72, List.product(input), 0.001)
  }

  test("lengthLeft should compute list length") {
    val input = List(1, 2, 3)
    assertEquals(3, List.lengthLeft(input))
  }

  test("reverse") {
    val input = List(1, 2, 3)
    assertEquals(List(3, 2, 1), List.reverse(input))
  }

  test("appendWithFold") {
    val input1 = List(1, 2, 3)
    val input2 = List(4, 5, 6)
    assertEquals(List.appendWithFold(input1, input2), List(1, 2, 3, 4, 5, 6))
  }

  test("flatten") {
    val input = List(List(1, 2, 3), List(4), List(5, 6))
    assertEquals(List.flatten(input), List(1, 2, 3, 4, 5, 6))
  }

  test("add1") {
    val input = List(1, 2, 3)
    assertEquals(List.add1(input), List(2, 3, 4))
  }

  test("toStringList") {
    val input = List(1.0, 2.0, 3.0)
    assertEquals(List.toStringList(input), List("1.0", "2.0", "3.0"))
  }

  test("map") {
    val input = List(1.0, 2.0, 3.0)
    assertEquals(List.map(input)(_.toString), List("1.0", "2.0", "3.0"))
  }

  test("filter") {
    val input = List(1, 2, 3)
    assertEquals(List.filter(input)(_ % 2 == 0), List(2))
  }

  test("flatmap") {
    val input = List(1, 2, 3)
    assertEquals(List.flatMap(input)(i => List(i, i)), List(1, 1, 2, 2, 3, 3))
  }

  test("filterWithFlatMap") {
    val input = List(1, 2, 3)
    assertEquals(List.filterWithFlatMap(input)(_ % 2 == 0), List(2))
  }

  test("addIntLists") {
    val input1 = List(1, 2, 3)
    val input2 = List(4, 5, 6)
    assertEquals(List.addIntLists(input1, input2), List(5, 7, 9))
  }

  test("zipWith") {
    val input1 = List(1, 2, 3)
    val input2 = List(4, 5, 6)
    assertEquals(List.zipWith(input1, input2)(_ + _), List(5, 7, 9))
  }
}
