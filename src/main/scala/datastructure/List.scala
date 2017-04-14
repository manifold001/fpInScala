package datastructure

import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends  List[Nothing]

case class Cons[A](h: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, tail) => h + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(h, tail) =>  h * product(tail)
  }

  def apply[A](items: A*): List[A] =
    if (items.isEmpty) Nil else Cons(items.head, apply(items.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Exercise 3-2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(h, tail) => tail
  }

  // Exercise 3-3
  def setHead[A](h: A, as: List[A]): List[A] = as match {
    case Nil => Cons(h, Nil)
    case Cons(x, tail) => Cons(h, tail)
  }

  // Exercise 3-4
  def drop[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) as else {
      as match {
        case Nil => Nil
        case Cons(h, tail) => drop(tail, n - 1)
      }
    }
  }

 // Exercise 3-5
 def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
   case Nil => Nil
   case Cons(h, tail) if f(h) => dropWhile(tail, f)
   case _ => as
 }

 // Exercise  3-6
 def init[A](as: List[A]): List[A] = as match {
   case Nil => Nil
   case Cons(h, Nil) => Nil
   case Cons(h, tail) => Cons(h, init(tail))
 }

  // Exercise 3-9
  def length[A](as: List[A]): Int = {
    def go(as: List[A], acc: Int): Int =  as match {
      case Nil => acc
      case Cons(_, tail) => go(tail, acc + 1)
    }
    go(as, 0)
  }

  def length_2[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

  // Exercise 3-10
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def go(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(t, tail) => go(tail, f(t, acc))
    }
    go(as, z)
  }

  // Exercise 3-12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((a, bs) => Cons(a, bs))

  // Exercise 3-13
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B) : B =
    foldRight(reverse(as), z)(f)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)(f)

  // Exercise 3-14
  def append[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)(Cons(_, _))

  // Exercise 3-15
  def concatenate[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil: List[A])(append)

  // Exercise 3-16
  def increaseOne(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((h, t) => Cons(h + 1, t))

  // Exercise 3-17
  def double2string(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((h, t) => Cons(h.toString, t))

  // Exercise 3-18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  // Exercise 3-19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) =>
      if (f(h)) Cons(h, t) else t
    )

  // Exercise 3-20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B])((a, b) => append(f(a), b))

  def flatMap_2[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))

  // Exercise 3-21
  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3-22
  def addList(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(a + b, addList(ta, tb))
  }

  // Exercise 3-23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), zipWith(ta, tb)(f))
  }

  // Exercise 3-24
  def hasSubsquence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsquence(t, sub)
  }

  def startsWith[A](sup: List[A], prefix: List[A]): Boolean =  (sup, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
}

