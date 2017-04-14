package datastructure

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(root: Tree[Int]): Int = root match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = root match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29
  def fold[A, B](root: Tree[A])(f: A => B)(g: (B, B) => B): B = root match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }
}

