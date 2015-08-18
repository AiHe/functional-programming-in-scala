package me.heai.errorhandling

/**
 * Created by aihe on 8/7/15.
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(v) => Right(f(v))
      case Left(v) => Left(v)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      x <- this
      y <- b
    } yield f(x, y)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {


  /**
   * exercise 4.6-1
   * @param es
   * @tparam E
   * @tparam A
   * @return
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight(Right(List[A]()): Either[E, List[A]]) {
      (e: Either[E, A], acc: Either[E, List[A]]) => e.map2(acc)(_ :: _)
    }
  }


  /**
   * exercise 4.6-2
   * @param as
   * @param f
   * @tparam E
   * @tparam A
   * @tparam B
   * @return
   */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight(Right(List[B]()): Either[E, List[B]]) {
      (e: A, acc: Either[E, List[B]]) => f(e).map2(acc)(_ :: _)
    }
  }
}
