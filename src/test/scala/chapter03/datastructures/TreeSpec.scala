package chapter03.datastructures

import Tree.Branch
import munit.FunSuite

class TreeSpec extends FunSuite {
  test("maximum should return the maximum inside a Tree[Int]") {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(26), Tree.Branch(Tree.Leaf(2), Tree.Leaf(4))),
      Tree.Leaf(5),
    )
    assertEquals(tree.maximum, 26)
  }

  test("depth should return the maximum path length from the root of the tree to any leaf") {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(26), Tree.Branch(Tree.Leaf(2), Tree.Leaf(4))),
      Tree.Leaf(5),
    )
    assertEquals(tree.depth, 4)
  }

  test("map should apply function to all elements of the Tree") {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(26), Tree.Branch(Tree.Leaf(2), Tree.Leaf(4))),
      Tree.Leaf(5),
    )
    val expected = Tree.Branch(
      Tree.Branch(Tree.Leaf(28), Tree.Branch(Tree.Leaf(4), Tree.Leaf(6))),
      Tree.Leaf(7),
    )
    assertEquals(tree.map(_ + 2), expected)
  }

  test("sizeViaFold should return the number of nodes in the Tree") {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(26), Tree.Branch(Tree.Leaf(2), Tree.Leaf(4))),
      Tree.Leaf(5),
    )
    assertEquals(tree.sizeViaFold, 7)
  }

  test("maximumViaFold should return the maximum inside a Tree[Int]") {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(26), Tree.Branch(Tree.Leaf(2), Tree.Leaf(4))),
      Tree.Leaf(5),
    )
    assertEquals(tree.maximumViaFold, 26)
  }

  test("depthViaFold should return the maximum path length from the root of the tree to any leaf") {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(26), Tree.Branch(Tree.Leaf(2), Tree.Leaf(4))),
      Tree.Leaf(5),
    )
    assertEquals(tree.depthViaFold, 4)
  }

  test("mapViaFold should apply function to all elements of the Tree") {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(26), Tree.Branch(Tree.Leaf(2), Tree.Leaf(4))),
      Tree.Leaf(5),
    )
    val expected = Tree.Branch(
      Tree.Branch(Tree.Leaf(28), Tree.Branch(Tree.Leaf(4), Tree.Leaf(6))),
      Tree.Leaf(7),
    )
    assertEquals(tree.mapViaFold(_ + 2), expected)
  }
}
