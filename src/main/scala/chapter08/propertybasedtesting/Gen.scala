package chapter08.propertybasedtesting

import chapter06.functionalstate.RNG.{int, nonNegativeInt}
import chapter06.functionalstate.State.{flatMap, map}
import chapter06.functionalstate.{RNG, State}
import chapter08.propertybasedtesting.Prop.FailedCase
import chapter08.propertybasedtesting.Prop.SuccessCount
import chapter06.functionalstate.SimpleRNG

enum Result {
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)
  case Proved

  def isFalsified: Boolean = this match {
    case Result.Passed          => false
    case Result.Proved          => false
    case Result.Falsified(_, _) => true
  }
}

opaque type TestCases = Int
object TestCases {
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x
}

opaque type MaxSize = Int
object MaxSize {
  extension (x: MaxSize) def toInt: Int = x
  def fromInt(x: Int): MaxSize = x
}

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop {
  opaque type FailedCase = String
  opaque type SuccessCount = Int
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = (maxSize, n, rng) =>
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if f(a) then Result.Passed
          else Result.Falsified(a.toString, i)
        } catch {
          case e: Exception => Result.Falsified(buildMsg(a, e), i)
        }
      }
      .find(_.isFalsified)
      .getOrElse(Result.Passed)

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace().mkString("\n")}"
  }

  extension (self: Prop) {
    def &&(that: Prop): Prop = { (max, tc, rng) =>
      self(max, tc, rng) match {
        case Result.Passed | Result.Proved => that(max, tc, rng)
        case x             => x
      }
    }
  }
  extension (self: Prop) {
    def ||(that: Prop): Prop = { (max, tc, rng) =>
      self(max, tc, rng) match {
        case Result.Falsified(_, _) => that(max, tc, rng)
        case x                      => x
      }
    }
  }

  @annotation.targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = (max, n, rng) => {
    val casesPerSize = (n.toInt - 1) / max.toInt + 1
    val props: LazyList[Prop] = LazyList
      .from(0)
      .take((n.toInt min max.toInt) + 1)
      .map(i => forAll(g(i))(f))
    val prop: Prop =
      props.map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng)).toList.reduce(_ && _)

    prop(max, n, rng)
  }

  extension (self: Prop) {
    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = SimpleRNG(System.currentTimeMillis()),
    ): Result = self(maxSize, testCases, rng)
  }

  extension (self: Prop) {
    def run(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = SimpleRNG(System.currentTimeMillis()),
    ): Unit = self(maxSize, testCases, rng) match {
      case Result.Falsified(msg, n) => println("! Falsified after $n passed tests:\n $msg")
      case Result.Passed            => println(s"+ OK, passed $testCases tests.")
      case Result.Proved            => println(s"+ OK, proved property.")
    }
  }

  def verify(p: => Boolean): Prop = {(_, _, _) =>
    if p then Result.Proved else Result.Falsified("()", 0)
  }
}

opaque type Gen[+A] = State[RNG, A]
opaque type SGen[+A] = Int => Gen[A]

object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
  }

  def unit[A](a: => A): Gen[A] = State(RNG.unit(a))
  def boolean: Gen[Boolean] = State(RNG.int).map(_ > 0)
  extension [A](self: Gen[A]) def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))
  extension [A](self: Gen[A]) def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)
  extension [A](self: Gen[A]) def listOfN[B](size: Gen[Int]): Gen[List[A]] = size.flatMap(self.listOfN)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1(1).abs / (g1(1).abs + g2(1).abs)
    State(RNG.double).flatMap(d => if d < g1Threshold then g1(0) else g2(0))
  }

  extension [A](self: Gen[A]) def unsized: SGen[A] = { _ => self }

  extension [A](self: Gen[A]) {
    def list: SGen[List[A]] = (n: Int) => self.listOfN(n)
  }

  extension [A](self: Gen[A]) {
    def nonEmptyList: SGen[List[A]] = (n: Int) => self.listOfN(n.max(1))
  }

}

object SGen {
  extension [A](self: SGen[A]) def map[B](f: A => B): SGen[B] = { x => self(x).map(f) }

  extension [A](self: SGen[A]) def flatMap[B](f: A => SGen[B]): SGen[B] = { x => self(x).flatMap(a => f(a)(x)) }
}
