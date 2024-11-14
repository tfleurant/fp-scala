package chapter05.strictnesslazyness

import munit.FunSuite

class LazyListSpec extends FunSuite {
  test("toList should evaluate all elements of the LazyList and return a List") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.toListUnsafe, List(1, 2, 3, 4))
  }

  test("take should return the n first elements") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.take(3).toList, List(1, 2, 3))
  }

  test("drop should remove the n first elements") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.drop(2).toList, List(3, 4))
  }

  test("takeWhile should return all starting elements that match predicate") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.takeWhile(x => x <= 2).toList, List(1, 2))
  }

  test("takeWhile should not return elements that match predicate after it didn't match an element") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.takeWhile(x => x > 1).toList, List.empty)
  }

  test("forAll should return true when all elements match predicate") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.forAll(x => x < 5), true)
  }

  test("forAll should return false when at least one element doesn't match predicate") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.forAll(x => x % 2 == 0), false)
  }

  test("takeWhileByFoldR should return all starting elements that match predicate") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.takeWhileByFoldR(x => x <= 2).toList, List(1, 2))
  }

  test("takeWhileByFoldR should not return elements that match predicate after it didn't match an element") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.takeWhileByFoldR(x => x > 1).toList, List.empty)
  }

  test("headOptionByFoldR should return first element of non empty list") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.headOptionByFoldR, Some(1))
  }

  test("headOptionByFoldR should None on empty List") {
    val data = LazyList.empty
    assertEquals(data.headOptionByFoldR, None)
  }

  test("map should apply transformation to all elements of the list") {
    val data = LazyList(1, 2, 3)
    assertEquals(data.map(_ * 2).toList, List(2, 4, 6))
  }

  test("filter should remove all elements not satisfying condition") {
    val data = LazyList(1, 2, 3)
    assertEquals(data.filter(_ % 2 == 0).toList, List(2))
  }

  test("append should add elements to end of LazyList") {
    val data = LazyList(1, 2, 3)
    assertEquals(data.append(LazyList(4, 5, 6)).toList, List(1, 2, 3, 4, 5, 6))
  }

  test("flatMap should map and flatten") {
    val data = LazyList(1, 2, 3)
    assertEquals(data.flatMap(i => LazyList(i, i)).toList, List(1, 1, 2, 2, 3, 3))
  }

  test("continually should generate infinitely the same value") {
    assertEquals(LazyList.continually(5).take(3).toList, List(5, 5, 5))
  }

  test("from should generate infinitely integers starting input") {
    assertEquals(LazyList.from(5).take(3).toList, List(5, 6, 7))
  }

  test("fibs should generate infinitely Fibonacci number") {
    assertEquals(LazyList.fibs.take(7).toList, List(0, 1, 1, 2, 3, 5, 8))
  }

  test("unfold should build a LazyList from an initial state and a generative function") {
    assertEquals(LazyList.unfold(0)(state => Some(state, state + 1)).take(5).toList, List(0, 1, 2, 3, 4))
  }

  test("unfold should stop when Option is None") {
    assertEquals(LazyList.unfold(0)(state => None).take(5).toList, List.empty)
  }

  test("onesByUnfold should generate infinitely 1") {
    assertEquals(LazyList.onesByUnfold.take(3).toList, List(1, 1, 1))
  }

  test("continuallyByUnfold should generate infinitely the same value") {
    assertEquals(LazyList.continuallyByUnfold(5).take(3).toList, List(5, 5, 5))
  }

  test("fromByUnfold should generate infinitely integers starting input") {
    assertEquals(LazyList.fromByUnfold(5).take(3).toList, List(5, 6, 7))
  }

  test("fibsByUnfold should generate infinitely Fibonacci number") {
    assertEquals(LazyList.fibsByUnfold.take(7).toList, List(0, 1, 1, 2, 3, 5, 8))
  }

  test("mapByUnfold should apply transformation to all elements of the list") {
    val data = LazyList(1, 2, 3)
    assertEquals(data.mapByUnfold(_ * 2).toList, List(2, 4, 6))
  }

  test("takeByUnfold should return the n first elements") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.takeByUnfold(3).toList, List(1, 2, 3))
  }

  test("takeWhileByUnfold should return all starting elements that match predicate") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.takeWhileByUnfold(x => x <= 2).toList, List(1, 2))
  }

  test("takeWhileByUnfold should not return elements that match predicate after it didn't match an element") {
    val data = LazyList(1, 2, 3, 4)
    assertEquals(data.takeWhileByUnfold(x => x > 1).toList, List.empty)
  }

  test("zipWith should apply zip two lists") {
    val data1 = LazyList(1, 2, 3)
    val data2 = LazyList("1", "2", "3")
    assertEquals(data1.zipWith(data2).toList, List((1, "1"), (2, "2"), (3, "3")))
  }

  test("zipWith should stop when either list is empty") {
    val data1 = LazyList(1, 2, 3)
    val data2 = LazyList("1", "2", "3")
    assertEquals(data1.take(2).zipWith(data2).toList, List((1, "1"), (2, "2")))
    assertEquals(data1.zipWith(data2.take(2)).toList, List((1, "1"), (2, "2")))
  }

  test("zipAll should apply zip two lists") {
    val data1 = LazyList(1, 2, 3)
    val data2 = LazyList("1", "2", "3")
    assertEquals(data1.zipAll(data2).toList, List((Some(1), Some("1")), (Some(2), Some("2")), (Some(3), Some("3"))))
  }

  test("zipAll should stop when either list is empty") {
    val data1 = LazyList(1, 2, 3)
    val data2 = LazyList("1", "2", "3")
    assertEquals(
      data1.take(2).zipAll(data2).toList,
      List((Some(1), Some("1")), (Some(2), Some("2")), (None, Some("3"))),
    )
    assertEquals(data1.zipAll(data2.take(2)).toList, List((Some(1), Some("1")), (Some(2), Some("2")), (Some(3), None)))
  }

  test("startsWith should return true when a lazy list starts with the subsequence of another lazy list") {
    val data1 = LazyList(1, 2, 3)
    val data2 = LazyList(1, 2)
    assertEquals(data1.startsWith(data2), true)
  }

  test("startsWith should return false when a lazy list doesn't with the subsequence of another lazy list") {
    val data1 = LazyList(1, 2, 3)
    val data2 = LazyList(3, 2)
    assertEquals(data1.startsWith(data2), false)
  }

  test("scanRight should build a list lazily while applying a function and return intermediate results") {
    val data = LazyList(1, 2, 3)
    assertEquals(data.scanRight(0)(_ + _).toList, List(1 + 2 + 3 + 0, 2 + 3 + 0, 3 + 0, 0))
  }
}
