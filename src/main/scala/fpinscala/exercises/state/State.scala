package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng0: RNG): (Int, RNG) =
    val (i, rng1) = rng0.nextInt
    (if i > 0 then i else -(i + 1), rng1)

  def double(rng: RNG): (Double, RNG) =
    val (i, rng1) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng1)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = double(rng1)
    ((i1, i2), rng2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = double(rng1)
    ((i2, i1), rng2)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (i1, rng1) = double(rng)
    val (i2, rng2) = double(rng1)
    val (i3, rng3) = double(rng2)
    ((i1, i2, i3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, acc: List[Int], r: RNG): (List[Int], RNG) = {
      if count <= 0 then
        (acc, r)
      else
        val (i, r2) = r.nextInt
        go(count - 1, i :: acc, r2)
    }
    go(count, Nil, rng)
  }

  def _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng0 => {
    val (a, rng1) = ra(rng0)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng1)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))((r, acc) => map2(r, acc)(_ :: _))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng0 =>
    val (i, rng1) = r(rng0)
    f(i)(rng1)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  // def map2ViaFlatMapForComprehension[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //   for
  //     a <- ra
  //     b <- rb
  //   yield f(a, b)

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- underlying
        b <- sb
      } yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] = s0 =>
      val (a, s1) = run(s0)
      f(a)(s1)
      // Note: this is the same as:
      // val (a, s1) = run(s0)
      // f(a).run(s1)

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight(unit(List.empty[A]))((r, acc) => r.map2(acc)(_ :: _))

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)

  val update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match
      case (_, Machine(_, 0, _)) => s
      case (Input.Coin, Machine(false, _, _)) => s
      case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Input.Turn, Machine(true, _, _)) => s
      case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)

