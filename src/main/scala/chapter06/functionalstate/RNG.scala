package chapter06.functionalstate

trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  // Linear congruential generator
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

object RNG:
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (randomValue, newRNG) = rng.nextInt
    randomValue match
      case r if r < 0 => (-(r + 1), newRNG)
      case r          => (r, newRNG)

  def double(rng: RNG): (Double, RNG) =
    val (randomValue, newRNG) = nonNegativeInt(rng)
    (randomValue / (Int.MaxValue.toDouble + 1), newRNG)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, r1) = double(rng)
    val (i, r2) = rng.nextInt
    ((d, i), r2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count > 0) {
      (1 to count).foldLeft((List.empty[Int], rng)) { case ((acc, currentRng), _) =>
        val (i, r1) = currentRng.nextInt
        (acc :+ i, r1)
      }
    } else {
      (List.empty, rng)
    }

  // State action data type
  type Rand[+A] = RNG => (A, RNG)

  // let's define a DSL using this type
  def int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  def doubleByMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      rs.foldLeft((List.empty[A], rng)) { case ((acc, r0), rand) =>
        val (i, r1) = rand(r0)
        (acc :+ i, r1)
      }

  // Can be written instead as
  def sequenceAlt[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))((r, acc) => map2(r, acc)(_ :: _))

  def randInts(count: Int): Rand[List[Int]] =
    sequenceAlt(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, r1) = r(rng)
      val rb = f(a)
      val (b, r2) = rb(r1)
      (b, r2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(0)
    }

  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f.andThen(unit))

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  // We can now roll a die, with injectable RNG implementation
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  // After definining State[S, A], Rand can then be written as
  type RandState[A] = State[RNG, A]
