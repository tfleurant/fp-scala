package chapter03.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Tree.Leaf(_)             => 1
    case Tree.Branch(left, right) => 1 + left.size + right.size

  def depth: Int = this match
    case Tree.Leaf(_)             => 1
    case Tree.Branch(left, right) => 1 + left.depth.max(right.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Tree.Leaf(value)         => Tree.Leaf(f(value))
    case Tree.Branch(left, right) => Tree.Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Tree.Leaf(value)         => f(value)
    case Tree.Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

  def sizeViaFold: Int = fold(_ => 1, (x, y) => 1 + x + y)
  def depthViaFold: Int = fold(_ => 1, (x, y) => 1 + x.max(y))
  def mapViaFold[B](f: A => B): Tree[B] = fold(f.andThen(Leaf.apply), Tree.Branch.apply)

object Tree:
  // Example of extension method for a tree of a specific type (previously implicit class in scala 2)
  extension (t: Tree[Int])
    def firstPositive: Int = t match
      case Tree.Leaf(value) => value
      case Tree.Branch(left, right) =>
        val lpos = left.firstPositive
        if lpos > 0 then lpos else right.firstPositive

    def maximum: Int = t match
      case Tree.Leaf(value)         => value
      case Tree.Branch(left, right) => left.maximum.max(right.maximum)

    def maximumViaFold: Int = t.fold(identity, (x, y) => x.max(y))
