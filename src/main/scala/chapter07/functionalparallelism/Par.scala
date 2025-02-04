package chapter07.functionalparallelism

import java.util.concurrent.*

// Par[A] is a representation of a task, that should be run synchronously or asynchronously
opaque type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    def isCancelled: Boolean = false

    def isDone: Boolean = true

    def get(timeout: Long, unit: TimeUnit): A = get
  }

  extension [A](pa: Par[A]) {
    // This implementation waits for both Future to complete before returing the Future result
    // Timeout is not respected
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = { es =>
      val futureA = pa(es)
      val futureB = pb(es)
      UnitFuture(f(futureA.get, futureB.get))
    }

    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = { es =>
      new Future[C] {
        private val futureA = pa(es)
        private val futureB = pb(es)
        @volatile private var cache: Option[C] = None

        def cancel(mayInterruptIfRunning: Boolean): Boolean =
          futureA.cancel(mayInterruptIfRunning) || futureB.cancel(mayInterruptIfRunning)

        def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

        def isDone: Boolean = cache.isDefined

        def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

        def get(timeout: Long, unit: TimeUnit): C = {
          val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, unit)
          val started = System.nanoTime()
          val a = futureA.get(timeoutNs, TimeUnit.NANOSECONDS)
          val elapsed = System.nanoTime() - started
          val b = futureB.get(timeoutNs - elapsed, TimeUnit.NANOSECONDS)
          val c = f(a, b)
          cache = Some(c)
          c
        }
      }
    }

    def run(s: ExecutorService): A = pa(s).get()

    def map[B](f: A => B): Par[B] = pa.map2Timeouts(unit(()))((a, _) => f(a))
  }

  // Only tags the Par to be run asynchronously later
  // Unfortunately this implementation will cause a deadlock on FixedThreadPools
  def fork[A](a: => Par[A]): Par[A] = { es =>
    es.submit(new Callable[A] {
      def call(): A = a(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2Timeouts(acc)((a, l) => a :: l))

  // A more efficient version
  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
    else {
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
    }
  }

  def fasterSequence[A](ps: List[Par[A]]): Par[List[A]] = sequenceBalanced(ps.toIndexedSeq).map(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      fasterSequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val fbs: List[Par[List[A]]] = as.map(asyncF(a => if f(a) then List(a) else Nil))
      fasterSequence(fbs).map(_.flatten)
    }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map(b => if b then 0 else 1))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val index = n.run(es) % choices.size
      choices(index)(es)
    }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      choices(key.run(es))(es)
    }

  // New generic operator
  extension [A](os: Par[A]) {
    def chooser[B](choices: A => Par[B]): Par[B] =
      es => choices(os.run(es))(es)
  }

  def choiceByChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    cond.chooser(b => if b then t else f)

  def choiceNByChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    n.chooser(i => choices(i % choices.size))

  // Chooser is actually flatMap !!
  extension [A](os: Par[A]) {
    def flatMap[B](choices: A => Par[B]): Par[B] =
      es => choices(os.run(es))(es)

  }

  // But we can have an even simpler operator: join
  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => ppa.run(es)(es)

  // implementing flatMap through join
  extension [A](os: Par[A]) {
    def flatMapByJoin[B](choices: A => Par[B]): Par[B] =
      join(os.map(choices))
  }

  // Implementing join through flatMap
  def joinByFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    ppa.flatMap(identity)

}
