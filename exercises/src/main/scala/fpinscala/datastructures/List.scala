package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else drop(tail(l), n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumL(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def productL(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  def reverseL[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def append[A](l: List[A], a: List[A]): List[A] = foldRight(l, a)((x, y) => Cons(x, y))

  def flatten[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil: List[A])((x, y) => append(x, y))
  }

  def addOne(l: List[Int]): List[Int] = l match {
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
    case _ => l
  }

  def addOneFold(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def addOnetr(l: List[Int]): List[Int] = {
    @tailrec
    def loop(l: List[Int], o: List[Int]): List[Int] = l match {
      case Cons(x, xs) => loop(xs, Cons(x + 1, o))
      case _ => o
    }
    loop(l, Nil: List[Int])
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(l: List[A], o: List[B]): List[B] = l match {
      case Cons(x, xs) => loop(xs, Cons(f(x), o))
      case _ => o
    }
    loop(l, Nil: List[B])
  }

  def filter[A](as: List[A])(f: A => Boolean):List[A] = {
    @tailrec
    def loop(l: List[A], o: List[A]): List[A] = l match {
      case Cons(x, xs) if(f(x)) => loop(xs, Cons(x, o))
      case Cons(x, xs) if(!f(x)) => loop(xs, o)
      case _ => o
    }
    loop(as, Nil: List[A])
  }
}