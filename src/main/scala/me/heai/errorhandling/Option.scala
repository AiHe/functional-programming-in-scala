package me.heai.errorhandling

/**
 * Created by aihe on 8/7/15.
 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(v) => Some(f(v))
      case None => None
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(v) => f(v)
      case None => None
    }
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(v) => v
      case None => default
    }
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case _ => this
    }
  }
  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(v) if f(v) => this
      case _ => None
    }
  }

  def isEmpty = this match {
    case None => true
    case _ => false
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /**
   * exercise 4.2-1
   * @param xs
   * @return
   */
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)
  }

  println(mean(List[Double]()))
  println(mean(List(1.0)))
  println(mean(List(1.0, 2.0)))


  /**
   * exercise 4.2-2
   * @param xs
   * @return
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap((f: Double) => mean(xs.map(x => Math.pow(x - f, 2))))
  }


  println(variance(List[Double]()))
  println(variance(List(1.0)))
  println(variance(List(1.0, 2.0)))


  /**
   * exercise 4.3
   * @param a
   * @param b
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- a
      y <- b
    } yield f(x, y)
  }

  println(map2(None: Option[Int], None: Option[Int])(_ + _))
  println(map2(Some(1), None: Option[Int])(_ + _))
  println(map2(None: Option[Int], Some(1))(_ + _))
  println(map2(Some(1), Some(1))(_ + _))


  /**
   * exercise 4.4-1
   * @param a
   * @tparam A
   * @return
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap((x: A) => sequence(t) map((y: List[A]) => x :: y))
    }
  }


  /**
   * exercise 4.4-2
   * @param a
   * @tparam A
   * @return
   */
  def sequenceViaFold[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(List[A]()): Option[List[A]]){
      (e: Option[A], acc: Option[List[A]]) => map2(e, acc)(_ :: _)
    }
  }

  /**
   * exercise 4.5
   * @param a
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(List[B]()): Option[List[B]]){
      (e: A, acc: Option[List[B]]) => map2(f(e), acc)(_ :: _)
    }
  }
}
