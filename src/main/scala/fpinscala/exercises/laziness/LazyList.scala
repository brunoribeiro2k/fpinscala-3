package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  def drop(n: Int): LazyList[A] = {
    this match
      case Cons(_, t) if n > 0 => t().take(n - 1)
      case _ => this
  }

  def takeWhile(p: A => Boolean): LazyList[A] = {
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def go(cur: Int, next: Int): LazyList[Int] =
      cons(cur, go(next, cur + next))
    go(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = {
    f(state) match
      case Some(a, s) => cons(a, unfold(s)(f))
      case None => empty
  }

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
