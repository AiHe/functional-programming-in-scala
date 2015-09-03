package me.heai.parsing

import java.util.regex.Pattern

import scala.util.matching.Regex

/**
 * Created by aihe on 9/2/15.
 */

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  /**
   * exercise 9.4
   * @param n
   * @param p
   * @tparam A
   * @return
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    n match {
      case a if a <= 0 => succeed(List[A]())
      case a if a > 0 => p.map2(listOfN(n - 1, p))(_ :: _)
    }
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.head)

  /**
   * exercise 9.3
   * @param p
   * @tparam A
   * @return
   */
  def many[A](p: Parser[A]): Parser[List[A]] = {
    p.map2(many(p))(_ :: _) | succeed(List[A]())
  }

  /**
   * exercise 9.8
   * @param a
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = {
    flatMap(a)(f andThen succeed)
  }

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  /**
   * exercise 9.1-2
   * @param p
   * @tparam A
   * @return
   */
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, p.many)(_ :: _)
  }

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  /**
   * exercise 9.7-1
   * @param p
   * @param p2
   * @tparam A
   * @tparam B
   * @return
   */
  def productViaFlatMap[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    flatMap(p)(a => p2.map(b => (a, b)))
  }

  /**
   * exercise 9.1-1
   * @param p
   * @param p2
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p, p2) map f.tupled
  }

  /**
   * exercise 9.7-2
   * @param p
   * @param p2
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    flatMap(p)(a => p2.map(b => f(a, b)))
  }


  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] = {
    p.slice.map2(p2)((_, b) => b)
  }

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] = {
    p.map2(p2.slice)((a, _) => a)
  }

  def whitespace = "\\s*".r

  def digits = "\\d+".r

  def opt[A](p: Parser[A]): Parser[Option[A]] = {
    p.map(Some(_)) or succeed(None)
  }

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] =  string("\"") *> thru("\"").map(_.init)

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
  def escapedQuoted: Parser[String]


  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = s".*${Pattern.quote(s)}".r

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] = {
    token("[-+]?(\\d+\\.\\d*|\\.\\d+)([Ee][-+]?\\d+)?".r)
  }

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] = doubleString.map(_.toDouble)

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] = {
    attempt(p) <* whitespace
  }

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  // use `Parser[Any]` since don't care about result type of separator
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = {
    sep1(p, p2) | succeed(List[A]())
  }


  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = {
    p.map2((p2 *> p).many)(_ ::: _)
  }


  /** Parses a sequence of left-associative binary operators with the same precedence. */
  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] = {
    p.map2((op ** p).many)((a, b) => b.foldLeft(a){case (acc, (op, e)) => op(acc, e)})
  }


  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) = {
    start *> p <* stop
  }


  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] = "\\z".r


  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] = {
    p <* eof
  }


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]) = self.product(p, p2)

    def product[B](p2: => Parser[B]) = **(p2)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)

    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
  }

}

