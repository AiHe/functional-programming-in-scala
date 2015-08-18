package me.heai.datastructure

/**
 * Created by aihe on 8/18/15.
 */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  val sampleTree1 = Leaf[Int](0)
  val sampleTree2 = Branch[Int](Leaf[Int](1), Leaf[Int](2))

  /**
   * exercise 3.25
   * @param t
   * @tparam A
   * @return
   */
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right)
    }
  }

  println(size(sampleTree1))
  println(size(sampleTree2))

  def dfs[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = {
    t match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) => dfs(right, dfs(left, z)(f))(f)
    }
  }

  /**
   * exercise 3.26
   * @param t
   * @return
   */
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(a) => a
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  println(maximum(sampleTree1))
  println(maximum(sampleTree2))

  /**
   * exercise 3.27
   * @param t
   * @tparam A
   * @return
   */
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }
  }

  println(depth(sampleTree1))
  println(depth(sampleTree2))

  /**
   * exercise 3.28
   * @param t
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }


  println(map(sampleTree1)(_ + 1))
  println(map(sampleTree2)(_ + 1))

  /**
   * exercise 3.29-1
   * @param t
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @return
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  /**
   * exercise 3.29-2
   * @param t
   * @tparam A
   * @return
   */
  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(_ + _)
  }

  println(sizeViaFold(sampleTree1))
  println(sizeViaFold(sampleTree2))

  /**
   * exercise 3.29-3
   * @param t
   * @return
   */
  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)((a: Int) => a)((a: Int, b: Int) => a max b)
  }

  println(maximumViaFold(sampleTree1))
  println(maximumViaFold(sampleTree2))

  /**
   * exercise 3.29-4
   * @param t
   * @tparam A
   * @return
   */
  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((a: Int, b: Int) => (a max b) + 1)
  }

  println(depthViaFold(sampleTree1))
  println(depthViaFold(sampleTree2))
}
