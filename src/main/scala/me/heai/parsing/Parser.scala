package me.heai.parsing

/**
 * Created by aihe on 9/2/15.
 */

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

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

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

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

  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

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
  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p, p2) map { case (a, b) => f(a, b) }
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: Parser[B]) = self.product(p, p2)

    def product[B](p2: Parser[B]) = **(p2)

    def many: Parser[List[A]] = self.many(p)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
  }

}

