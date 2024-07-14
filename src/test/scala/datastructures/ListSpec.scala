package datastructures

class ListSpec extends munit.FunSuite {
  test("tail should remove head when List is not empty") {
    val data = List(1, 2, 3, 4)
    assertEquals(List.tail(data), List(2, 3, 4))
  }

  test("tail should throw error when List is empty") {
    val data = List.Nil
    intercept[RuntimeException] {
      List.tail(data)
    }
  }

  test("setHead should replace head when list is not empty") {
    val data = List(1, 2, 3)
    assertEquals(List.setHead(data, 4), List(4, 2, 3))
  }

  test("setHead should throw error when List is empty, as it cannot replace head") {
    val data = List.Nil
    intercept[RuntimeException] {
      List.setHead(data, 5)
    }
  }

  test("drop should remove the first n elements of a list") {
    val data = List(1, 2, 3)
    assertEquals(List.drop(data, 2), List(3))
  }

  test("drop should remove all elements when list is empty") {
    val data = List.Nil
    assertEquals(List.drop(data, 2), List.Nil)
  }

  test("drop should remove all elements when list is smaller thant number of elements to remove") {
    val data = List(1, 2, 3)
    assertEquals(List.drop(data, 5), List.Nil)
  }

  test("dropWhile should remove all elements from the list as long as they match a predicate") {
    val data = List(1, 2, 3)
    assertEquals(List.dropWhile(data, _ < 3), List(3))
  }

  test("dropWhile should remove all elements from the list when the list is empty") {
    val data: List[Int] = List.Nil
    assertEquals(List.dropWhile(data, _ < 3), List.Nil)
  }

  test(
    "dropWhile should remove all elements when list is smaller thant number of elements to remove, as long as items match predicate",
  ) {
    val data: List[Int] = List(1, 2)
    assertEquals(List.dropWhile(data, _ < 3), List.Nil)
  }

  test("init should return all elements except the last one") {
    val data = List(1, 2, 3)
    assertEquals(List.init(data), List(1, 2))
  }

  test("init should return no element when list is empty") {
    val data: List[Int] = List.Nil
    assertEquals(List.init(data), List.Nil)
  }

  test("init should return no element when list has only one element") {
    val data: List[Int] = List(1)
    assertEquals(List.init(data), List.Nil)
  }

  test("length should return the length of a non empty list") {
    val data = List(1, 2, 3)
    assertEquals(List.length(data), 3)
  }

  test("length should return the length of an empty list") {
    val data = List.Nil
    assertEquals(List.length(data), 0)
  }

  test("sumViaFoldLeft should return the sum of all elements in a list of Int") {
    val data = List(1, 2, 3)
    assertEquals(List.sumViaFoldLeft(data), 6)
  }

  test("sumViaFoldLeft should return 0 for an empty list") {
    val data = List.Nil
    assertEquals(List.sumViaFoldLeft(data), 0)
  }

  test("sumViaFoldLeft should return the product of all elements in a list of Doubles") {
    val data = List(1.0, 2.0, 3.0)
    assertEquals(List.productViaFoldLeft(data), 6.0)
  }

  test("sumViaFoldLeft should return 1.0 for an empty list") {
    val data: List[Double] = List.Nil
    assertEquals(List.productViaFoldLeft(data), 1.0)
  }

  test("lengthViaFoldLeft should return the length of a non empty list") {
    val data = List(1, 2, 3)
    assertEquals(List.lengthViaFoldLeft(data), 3)
  }

  test("lengthViaFoldLeft should return the length of an empty list") {
    val data = List.Nil
    assertEquals(List.lengthViaFoldLeft(data), 0)
  }

  test("reverse should reverse a list") {
    val data = List(1, 2, 3)
    assertEquals(List.reverse(data), List(3, 2, 1))
  }

  test("appendViaFoldLeft should append 2 list") {
    val data1 = List(1, 2, 3)
    val data2 = List(4, 5, 6)
    assertEquals(List.appendViaFoldRight(data1, data2), List(1, 2, 3, 4, 5, 6))
  }

  test("flatten should flatten a list of list") {
    val data = List(List(1, 2), List(3, 4), List(5, 6))
    assertEquals(List.flatten(data), List(1, 2, 3, 4, 5, 6))
  }

  test("add1 should add 1 to all elements of a list of Int") {
    val data = List(0, 1, 2)
    assertEquals(List.add1(data), List(1, 2, 3))
  }

  test("doublesToStrings should transform a list of doubles to a list of strings") {
    val data = List(1.0, 2.0, 3.0)
    assertEquals(List.doublesToStrings(data), List("1.0", "2.0", "3.0"))
  }

  test("map should apply transformation to all elements of the list") {
    val data = List(1, 2, 3)
    assertEquals(List.map(data, _ * 2), List(2, 4, 6))
  }

  test("filter should remove all elements not satisfying condition") {
    val data = List(1, 2, 3)
    assertEquals(List.filter(data, _ % 2 == 0), List(2))
  }

  test("flatMap should map and flatten") {
    val data = List(1, 2, 3)
    assertEquals(List.flatMap(data, i => List(i, i)), List(1, 1, 2, 2, 3, 3))
  }

  test("filterViaFlatMap should remove all elements not satisfying condition") {
    val data = List(1, 2, 3)
    assertEquals(List.filterViaFlatMap(data, _ % 2 == 0), List(2))
  }

  test("addPairWise should add elements of 2 list pairwise") {
    val data1 = List(1, 2, 3)
    val data2 = List(4, 5, 6)
    assertEquals(List.addPairWise(data1, data2), List(5, 7, 9))
  }

  test("addPairWise should add elements of 2 list pairwise, and stop when one of the list is shorter than the other") {
    val data1 = List(1, 2)
    val data2 = List(4, 5, 6)
    assertEquals(List.addPairWise(data1, data2), List(5, 7))
  }

  test("zipWith should add elements of 2 list pairwise") {
    val data1 = List(1, 2, 3)
    val data2 = List(4, 5, 6)
    assertEquals(List.zipWith(data1, data2, _ * _), List(4, 10, 18))
  }

  test("zipWith should add elements of 2 list pairwise, and stop when one of the list is shorter than the other") {
    val data1 = List(1, 2)
    val data2 = List(4, 5, 6)
    assertEquals(List.zipWith(data1, data2, _ * _), List(4, 10))
  }

  test("hasSubsequence should check that a subsquence exists in a list") {
    val data = List(1, 2, 3, 4)
    assertEquals(List.hasSubsequence(data, List(2, 3, 4)), true)
  }

  test("hasSubsequence should not return true when subsequence does not exist") {
    val data = List(1, 2, 3, 4)
    assertNotEquals(List.hasSubsequence(data, List(2, 4)), true)
  }

  test("hasSubsequence should return true when both list are Nil") {
    val data = List.Nil
    assertEquals(List.hasSubsequence(data, List.Nil), true)
  }

  test("hasSubsequence should return true when containing list is Nil and sequence is not Nil") {
    val data = List.Nil
    assertEquals(List.hasSubsequence(data, List(1, 2, 3)), false)
  }
}
