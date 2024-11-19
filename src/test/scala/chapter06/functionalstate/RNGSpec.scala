package chapter06.functionalstate

import chapter06.functionalstate.RNG.*
import munit.FunSuite

import scala.annotation.tailrec

class RNGSpec extends FunSuite {

  @tailrec
  private def simpleIterator(rng: RNG, remainingIterations: Int)(block: RNG => RNG): Unit =
    if (remainingIterations > 0) {
      simpleIterator(block(rng), remainingIterations - 1)(block)
    } else {
      ()
    }

  test("nonNegativeInt should generate ab int in [0, Int.MaxValue]") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val (i, r) = nonNegativeInt(rng)
      assert(i >= 0)
      r
    }
  }

  test("double should generate a double in [0, 1[") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val (i, r) = double(rng)
      assert(i >= 0 && i < 1)
      r
    }
  }

  test("intDouble should generate a pair of (Int,Double)") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val ((i, d), r) = intDouble(rng)
      assert(i >= Int.MinValue && i <= Int.MaxValue)
      assert(d >= 0.0 && d <= Double.MaxValue, s"assertion failed for $d")
      r
    }
  }

  test("doubleInt should generate a pair of (Double, Int)") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val ((d, i), r) = doubleInt(rng)
      assert(i >= Int.MinValue && i <= Int.MaxValue)
      assert(d >= 0.0 && d <= Double.MaxValue, s"assertion failed for $d")
      r
    }
  }

  test("double3 should generate a tuple of (Double, Double, Double)") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val ((d1, d2, d3), r) = double3(rng)
      assert(d1 >= 0.0 && d1 <= Double.MaxValue)
      assert(d2 >= 0.0 && d2 <= Double.MaxValue)
      assert(d3 >= 0.0 && d3 <= Double.MaxValue)
      r
    }
  }

  test("ints should generate a list of random integers") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val (is, r) = ints(10)(rng)
      assert(is.distinct.size == 10)
      is.foreach(i => assert(i >= Int.MinValue && i <= Int.MaxValue))
      r
    }
  }

  test("doubleByMap should generate a double in [0, 1[") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val (i, r) = doubleByMap(rng)
      assert(i >= 0 && i < 1)
      r
    }
  }

  test("map2 should map 2 RNG actions") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val ((a, b), r) = map2[Int, Int, (Int, Int)](int, int)((a, b) => (a, b))(rng)
      assert(a != b)
      r
    }
  }

  test("randInts should generate a list of random integers") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val (is, r) = randInts(10)(rng)
      assert(is.distinct.size == 10)
      is.foreach(i => assert(i >= Int.MinValue && i <= Int.MaxValue))
      r
    }
  }

  test("nonNegativeLessThan should generate a double in [0, max[") {
    simpleIterator(SimpleRNG(4L), 100) { rng =>
      val (i, r) = nonNegativeLessThan(5)(rng)
      assert(i >= 0 && i < 5)
      r
    }
  }

}
