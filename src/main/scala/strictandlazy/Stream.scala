package strictandlazy

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toListViaIter: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h()::acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def toListFast: List[A] = {
    val buffer = new scala.collection.mutable.ListBuffer[A]

    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buffer += h()
        go(t())
      case _ => buffer.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h, t) if n ==1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A,  => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, s) => if(p(a)) Stream.cons(a, s) else Stream.empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) =>Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))

}

case object Empty extends Stream[Nothing]

case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, cons(b, go(b, a+ b)))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) {case (f0, f1) => Some((f0, (f1, f0 + f1)))}

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(_ => Some((n, n + 1)))

  def constantsViaUnfold(n: Int): Stream[Int] =
    unfold(n)( _ => Some((n, n)))
}
