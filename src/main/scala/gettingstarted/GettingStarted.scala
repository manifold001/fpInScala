package gettingstarted

import scala.annotation.tailrec

object GettingStarted {

  // Exercise 2-1
  def fib(n: Int): Int = {

    @tailrec
    def go(a: Int, b: Int, n: Int): Int =
      if (n == 1) a else go(b, a + b, n - 1)

    go(0, 1, n)
  }

 // Exercise 2-2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) {
      true
    } else {
      val h = as.head
      val t = as.tail
      ordered(h, t.head) && isSorted(t, ordered)
    }
  }

  // Exercise 2-3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2-4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
  
}
