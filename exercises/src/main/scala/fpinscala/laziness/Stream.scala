package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = this.foldRight(Nil: List[A])((a, b) => a :: b)

  def take2(n: Int): Stream[A] =
    this.foldRight(Empty: Stream[A])((a, b) => if (n > 0) Cons(() => a, () => b.take2(n - 1)) else Empty)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n -1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else Cons(h, t)
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def forAll2(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll2(p) else false
    case _ => true
  }

  def headOption: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = this.foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

//  def append[B>:A](z: => B): Stream[B] = this.foldRight(Stream(z): Stream[B])((a, b) => cons(a, b))

  def append[B>:A](z: => Stream[B]): Stream[B] = this.foldRight(z)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])((a, b) => f(a) append b)

  // Unfold things
  def mapUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }


  def takeUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithUnfold[B, C](o: Stream[B])(f: (A, B) => C) = unfold((this, o)){
    case (Cons(h, t), Cons(ho, to)) => Some(f(h(), ho()), (t(), to()))
    case _ => None
  }

  // This should have taken f (Option[A], Option[B]) => C much like other zips above
  def zipAllUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)){
    case (Cons(h, t), Cons(hs2, ts2)) => Some((Some(h()), Some(hs2())), (t(), ts2()))
    case (Empty, Cons(hs2, ts2)) => Some((None, Some(hs2())), (Empty, ts2()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case _ => None
  }

  // Correct answer for above
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }


  // Run till end of comparing stream (s)
  def startsWith[B](s: Stream[B]): Boolean = this.zipAllUnfold(s).takeWhile(_._2.isDefined).forAll{
    case (a, b) => a == b
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(h, t) => Some(Cons(h, t), t())
    case _ => None
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // This seems right but is not what the answer has.
  // Is matching on the Cons(h, t) instead of the Stream really that bad?
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.foldRight(Stream(z)){
      case (a, Cons(h, t)) =>
        lazy val head = h()
        cons(f(a, head), Cons(() => head, t))
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(cur: Int, nxt: Int): Stream[Int] = Stream.cons(cur, go(nxt, cur + nxt))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  def fibs2: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)){
    case (cur: Int, nxt: Int) => Some(cur, (nxt, cur + nxt))
    case _ => None
  }

  def from2(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def ones2 = constant2(1)



}