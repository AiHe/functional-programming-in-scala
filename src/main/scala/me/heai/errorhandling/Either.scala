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
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
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
