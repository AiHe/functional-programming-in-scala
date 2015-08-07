package me.heai

import scala.annotation.tailrec

/**
 * Created by aihe on 8/7/15.
 */
object P1C2 extends App{
  /**
   * exercise 2.1
   * @param n
   * @return
   */
  def fib(n: Int): Int = {
    @tailrec
    def helper(n: Int, prev: Int, cur: Int): Int = {
      n match {
        case 1 => prev
        case _ => helper(n-1, cur, prev + cur)
      }
    }
    helper(n, 0, 1)
  }
  // test
  println(fib(1))
  println(fib(2))
  println(fib(3))
  println(fib(4))
  println(fib(5))

  /**
   * exercise 2.2
   * @param as
   * @param ordered
   * @tparam A
   * @return
   */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean ={
    @tailrec
    def helper(idx: Int): Boolean = {
      if (idx == as.size - 1) true
      else if (!ordered(as(idx), as(idx+1))) false
      else helper(idx + 1)
    }
    helper(0)
  }

  // test
  println(isSorted(Array(1, 2, 3, 4), (a: Int, b: Int) => a <= b))
  println(isSorted(Array(1, 2, 4, 4), (a: Int, b: Int) => a <= b))
  println(isSorted(Array(1, 2, 4, 3), (a: Int, b: Int) => a <= b))


  /**
   * exercise 2.3
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)

  /**
   * exercise 2.4
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)


  /**
   * exercise 2.5
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
