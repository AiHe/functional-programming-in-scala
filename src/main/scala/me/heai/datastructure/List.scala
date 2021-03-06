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


  val ex1: List[Double] = Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))
  println(ex1)
  println(ex2)
  println(ex3)

  /**
   * exercise 3.1
   */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  // Using the same idea, implement the function
  // setHead for replacing the first element of
  // a List with a different value.
  /**
   * exercise 3.2
   * @param l
   * @param a
   * @tparam A
   * @return
   */
  def setHead[A](l: List[A], a: A): List[A] = {
    l match {
      case Nil => throw new Exception("set head at empty list")
      case Cons(head, tail) => Cons(a, tail)
    }
  }

  //setHead(Nil, 1)
  println(setHead(List(3, 2, 1), 1))

  /**
   * exercise 3.3
   * @param l
   * @param n
   * @tparam A
   * @return
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(a, b) => drop(b, n - 1)
      }
    }
  }

  println(drop(List(1, 2, 3), 0))
  println(drop(List(1, 2, 3), 1))
  println(drop(List(1, 2, 3), 2))
  println(drop(List(1, 2, 3), 3))
  println(drop(List(1, 2, 3), 4))

  /**
   * exercise 3.4
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, b) => {
        if (f(a)) dropWhile(b, f)
        else l
      }
    }
  }

  println(dropWhile(List(1, 2, 3), (a: Int) => a > 1))
  println(dropWhile(List(1, 2, 3), (a: Int) => a >= 1))
  println(dropWhile(List(1, 2, 3), (a: Int) => a == 1))
  println(dropWhile(List(1, 2, 3), (a: Int) => a <= 1))
  println(dropWhile(List(1, 2, 3), (a: Int) => a < 1))
  println(dropWhile(List(1, 2, 3), (a: Int) => a <= 2))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  /**
   * exercise 3.9
   * @param as
   * @tparam A
   * @return
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0) {
      (e: A, acc: Int) => acc + 1
    }
  }

  println(length(Nil))
  println(length(List(1)))
  println(length(List(1, 2, 3)))

  /**
   * exercise 3.10
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  /**
   * exercise 3.11-1
   * @param ns
   * @return
   */
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  println(sum3(List()))
  println(sum3(List(1)))
  println(sum3(List(1, 2)))

  /**
   * exercise 3.11-2
   * @param ns
   */
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  println(product3(List()))
  println(product3(List(1)))
  println(product3(List(1, 2)))

  /**
   * exercise 3.12
   * @param as
   * @tparam A
   * @return
   */
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]()) {
    (acc: List[A], e: A) => Cons(e, acc)
  }

  println(println(reverse(List())))
  println(println(reverse(List(1))))
  println(println(reverse(List(1, 2))))

  /**
   * exercise 3.13
   * @param l
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z) {
      (acc: B, e: A) => f(e, acc)
    }
  }

  /**
   * exercise 3.14-1
   * @param l
   * @param r
   * @tparam A
   * @return
   */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRightViaFoldLeft(l, r) {
      (e: A, acc: List[A]) => Cons(e, acc)
    }
  }

  println(appendViaFoldRight(List(), List(1, 2)))
  println(appendViaFoldRight(List(1, 2), List()))
  println(appendViaFoldRight(List(1, 2), List(3, 4)))

  /**
   * exercise 3.14-2
   * @param l
   * @param r
   * @tparam A
   * @return
   */
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = {
    foldLeft(reverse(l), r) {
      (acc: List[A], e: A) => Cons(e, acc)
    }
  }

  println(appendViaFoldLeft(List(), List(1, 2)))
  println(appendViaFoldLeft(List(1, 2), List()))
  println(appendViaFoldLeft(List(1, 2), List(3, 4)))

  /**
   * exercise 3.15
   * @param l
   * @tparam A
   * @return
   */
  def concat[A](l: List[List[A]]): List[A] = {
    foldRightViaFoldLeft(l, List[A]())(appendViaFoldRight)
  }

  println(concat(List(List(), List(), List())))
  println(concat(List(List(), List(), List(1))))
  println(concat(List(List(), List(1), List())))
  println(concat(List(List(1), List(), List())))
  println(concat(List(List(1), List(1), List())))
  println(concat(List(List(), List(1), List(1))))
  println(concat(List(List(1), List(), List(1))))
  println(concat(List(List(1), List(1), List(1))))

  /**
   * exercise 3.16
   * @param l
   * @return
   */
  def add1(l: List[Int]): List[Int] = {
    foldRightViaFoldLeft(l, List[Int]()) {
      (e: Int, acc: List[Int]) => Cons(e + 1, acc)
    }
  }

  println(add1(List()))
  println(add1(List(1)))
  println(add1(List(1, 2)))

  /**
   * exercise 3.17
   * @param l
   * @return
   */
  def doubleToString(l: List[Double]): List[String] = {
    foldRightViaFoldLeft(l, List[String]()) {
      (e: Double, acc: List[String]) => Cons(e.toString, acc)
    }
  }

  println(doubleToString(List()))
  println(doubleToString(List(1.0)))
  println(doubleToString(List(1.0, 2.0)))

  /**
   * exercise 3.18
   * @param as
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(as, List[B]()) {
      (e: A, acc: List[B]) => Cons(f(e), acc)
    }
  }

  println(map(List[Int]())(_.toDouble))
  println(map(List(1))(_.toDouble))
  println(map(List(1, 2))(_.toString))

  /**
   * exercise 3.19
   * @param as
   * @param f
   * @tparam A
   * @return
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRightViaFoldLeft(as, List[A]()) {
      (e: A, acc: List[A]) => {
        if (f(e)) Cons(e, acc) else acc
      }
    }
  }

  println(filter(List[Int]())(_ % 2 == 0))
  println(filter(List[Int](1))(_ % 2 == 0))
  println(filter(List[Int](1, 2))(_ % 2 == 0))

  /**
   * exercise 3.20
   * @param as
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  println(flatMap(List[Int]())((e: Int) => List(e.toString, e.toString)))
  println(flatMap(List[Int](1))((e: Int) => List(e.toString, e.toString)))
  println(flatMap(List[Int](1, 2))((e: Int) => List(e.toString, e.toString)))

  /**
   * exercise 3.21
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l) {
      (e: A) => if (f(e)) List(e) else Nil
    }
  }

  println(filterViaFlatMap(List[Int]())(_ % 2 == 0))
  println(filterViaFlatMap(List[Int](1))(_ % 2 == 0))
  println(filterViaFlatMap(List[Int](1, 2))(_ % 2 == 0))

  /**
   * exercise 3.22
   * @param a
   * @param b
   * @return
   */
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    (a, b) match {
      case (Cons(ah: Int, at), Cons(bh: Int, bt)) => Cons(ah + bh, addPairwise(at, bt))
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
  }

  println(addPairwise(List[Int](), List[Int]()))
  println(addPairwise(List[Int](), List[Int](1)))
  println(addPairwise(List[Int](1), List[Int]()))
  println(addPairwise(List[Int](1), List[Int](1)))
  println(addPairwise(List[Int](1), List[Int](1, 2)))
  println(addPairwise(List[Int](1, 2), List[Int](1)))
  println(addPairwise(List[Int](1, 2), List[Int](1, 2)))

  /**
   * exercise 3.23
   * @param a
   * @param b
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    (a, b) match {
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
  }

  println(zipWith(List[Int](), List[Int]())(_ + _))
  println(zipWith(List[Int](), List[Int](1))(_ + _))
  println(zipWith(List[Int](1), List[Int]())(_ + _))
  println(zipWith(List[Int](1), List[Int](1))(_ + _))
  println(zipWith(List[Int](1), List[Int](1, 2))(_ + _))
  println(zipWith(List[Int](1, 2), List[Int](1))(_ + _))
  println(zipWith(List[Int](1, 2), List[Int](1, 2))(_ + _))

  /**
   * exercise 3.24
   * @param sup
   * @param sub
   * @tparam A
   * @return
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def containsFromStart(a: List[A], b: List[A]): Boolean = {
      (a, b) match {
        case (_, Nil) => true
        case (Cons(ah, at), Cons(bh, bt)) if ah == bh => containsFromStart(at, bt)
        case _ => false
      }
    }
    sup match {
      case Nil => sub == Nil
      case Cons(head, tail) => if (containsFromStart(sup, sub)) true else hasSubsequence(tail, sub)
    }
  }

  println(hasSubsequence(List[Int](), List[Int]()))
  println(hasSubsequence(List[Int](), List[Int](1)))
  println(hasSubsequence(List[Int](1), List[Int]()))
  println(hasSubsequence(List[Int](1), List[Int](1)))
  println(hasSubsequence(List[Int](1, 2), List[Int](1)))
  println(hasSubsequence(List[Int](1, 2), List[Int](2)))
  println(hasSubsequence(List[Int](1, 2, 3), List[Int](2)))
  println(hasSubsequence(List[Int](1), List[Int](1, 2)))

}
