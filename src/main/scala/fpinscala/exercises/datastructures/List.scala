package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result: Int = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Cons(_, t) => t
      case _ => sys.error("Tail of Nil")

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else go(tail(l), n - 1)
    }
    go(l, n)
  }

  @tailrec
  def drop2[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Cons(_, t) => drop2(t, n - 1)
      case _ => l
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, Nil) => List(h)
      case Cons(h, t) => Cons(h, init(t))
      case Nil => sys.error("init of a Nil")
    }
  }

  def length[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(_, t) => 1 + length(t)
    }
  }

  def lengthViaFoldRight[A](l: List[A]): Int =
    foldRight(l, 0, (_, y) => y + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = {
    l match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)
    }
  }

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (x, _) => 1 + x)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A], (b, a) => Cons(a, b))
  }

  def foldRightViaFoldLeft[A,B](l: List[A], acc: B, f: (A, B) => B): B = {
    foldLeft(reverse(l), acc, (b, a) => f(a, b))
  }

  def foldRightViaFoldLeft2[A,B](l: List[A], acc: B, f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)
  }
  def foldLeftViaFoldRight[A,B](l: List[A], acc: B, f: (B, A) => B): B = {
    foldRight(l, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)
  }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r, Cons(_, _))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil: List[A], append)
  }

  def incrementEach(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int], (a, acc) => Cons(a + 1, acc))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String], (a, acc) => Cons(a.toString, acc))
  }

  def map[A,B](l: List[A], f: A => B): List[B] = {
    foldRight(l, Nil: List[B], (a, acc) => Cons(f(a), acc))
  }

  def filter[A](as: List[A], f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A], (a, acc) => {
      if (f(a)) Cons(a, acc)
      else acc
    })
  }

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = {
    concat(map(as, f))
  }

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = {
    flatMap(as, a => if (f(a)) List(a) else Nil)
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))
    }
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(hSup, tSup), Cons(hSub, tSub)) if hSup == hSub => startsWith(tSup, tSub)
      case _ => false
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
  }
