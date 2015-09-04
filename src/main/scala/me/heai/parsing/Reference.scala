package me.heai.parsing

/**
 * Created by aihe on 9/4/15.
 */
import ReferenceTypes._

import scala.util.matching.Regex

object ReferenceTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {

  }
  sealed trait Result[+A] {

  }
}

object Reference extends Parsers[ParseError, Parser] {
  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  override def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = ???

  override implicit def string(s: String): Parser[String] = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override implicit def regex(r: Regex): Parser[String] = ???

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???
}
