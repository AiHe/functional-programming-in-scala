package me.heai.datastructure

/**
 * Created by aihe on 8/7/15.
 */

sealed trait List[+A] {

  override def toString() = {
    this match {
      case Cons(a, b) => a.toString + "->" + b.toString
      case _ => "^"
    }
  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x: Int, xs: List[Int]) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x: Double, xs: List[Double]) => x * product(xs)
  }
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, this.apply(as.tail: _*))
  }
}
