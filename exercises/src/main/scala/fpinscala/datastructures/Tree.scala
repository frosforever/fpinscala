package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tr: Tree[A]): Int = tr match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // This can all be more efficient by not matching in the Leaf
  def max(tr: Tree[Int]): Int = {
    def helper(t: Tree[Int], max: Int): Int = t match {
      case Leaf(x) => x.max(max)
      case Branch(l, r) => helper(l, max).max(helper(r, max))
    }
    helper(tr, Int.MinValue)
  }

  def depth[A](tr: Tree[A]): Int = {
    def helper(t: Tree[A], accDepth: Int, maxDepth: Int): Int = t match {
      case Leaf(_) => (accDepth).max(maxDepth)
      case Branch(l, r) => helper(l, accDepth + 1, maxDepth).max(helper(r, accDepth + 1, maxDepth))
    }
    helper(tr, 0, Int.MinValue)
  }

  def map[A, B](tr: Tree[A])(f: A => B): Tree[B] = tr match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(x) => l(x)
    case Branch(le,r) => b(fold(le)(l)(b), fold(r)(l)(b))
  }

  def size2[A](tr: Tree[A]): Int =
    fold(tr)(_ => 1)(_ + _ + 1)

  def max2(tr: Tree[Int]): Int =
    fold(tr)(a => a)((l, r) => l.max(r))

  def depth2[A](tr: Tree[A]): Int =
    fold(tr)(_ => 0)((l, r) => 1 + l.max(r))

  def map2[A, B](tr: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tr)(x => Leaf(f(x)))((l,r) => Branch(l, r))

}