package me.heai.parsing

import java.util.regex.Pattern

import me.heai.monad.Monad.Monad

import scala.util.matching.Regex

/**
  * Created by aihe on 9/2/15.
  */

trait ParsersMonad[P[+ _]] extends Monad[P] {
  self =>

  def or[A](s1: P[A], s2: => P[A]): P[A]

  implicit def string(s: String): P[String]

  implicit def operators[A](p: P[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => P[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): P[String]

  /**
    * exercise 9.4
    *
    * @param n
    * @param p
    * @tparam A
    * @return
    */
  def listOfN[A](n: Int, p: P[A]): P[List[A]] = {
    n match {
      case a if a <= 0 => succeed(List[A]())
      case a if a > 0 => p.map2(listOfN(n - 1, p))(_ :: _)
    }
  }

  def run[A](p: P[A])(input: String): Either[ParseError, A]

  def char(c: Char): P[Char] = string(c.toString) map (_.head)

  //  def delay[A](p: => P[A]): P[A] = p

  /**
    * exercise 9.3
    *
    * @param p
    * @tparam A
    * @return
    */
  def many[A](p: P[A]): P[List[A]] = {
    map2(p, p.many)(_ :: _) | succeed(List[A]())
  }


  def unit[A](a: => A): P[A]

  /**
    * exercise 9.8
    *
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  override def map[A, B](a: P[A])(f: A => B): P[B] = {
    flatMap(a)(f andThen succeed)
  }

  def succeed[A](a: A): P[A]

  def slice[A](p: P[A]): P[String]

  /**
    * exercise 9.1-2
    *
    * @param p
    * @tparam A
    * @return
    */
  def many1[A](p: P[A]): P[List[A]] = {
    map2(p, p.many)(_ :: _)
  }

  //  def product[A, B](p: P[A], p2: => P[B]): P[(A, B)]

  //  def product[A, B](p: P[A], p2: => P[B]): P[(A, B)] = {
  //    map2(p, p2)((a, b) => (a, b))
  //  }

  /**
    * exercise 9.7-1
    *
    * @param p
    * @param p2
    * @tparam A
    * @tparam B
    * @return
    */
  def product[A, B](p: P[A], p2: => P[B]): P[(A, B)] = {
    for {
      a <- p
      b <- p2
    } yield (a, b)
  }

  //  /**
  //    * exercise 9.1-1
  //    *
  //    * @param p
  //    * @param p2
  //    * @param f
  //    * @tparam A
  //    * @tparam B
  //    * @tparam C
  //    * @return
  //    */
  //  override def map2[A, B, C](p: P[A], p2: => P[B])(f: (A, B) => C): P[C] = {
  //    product(p, p2) map f.tupled
  //  }

  //  def map2[A, B, C](p: P[A], p2: => P[B])(f: (A, B) => C): P[C]

  /**
    * exercise 9.7-2
    *
    * @param p
    * @param p2
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2ViaFlatMap[A, B, C](p: P[A], p2: => P[B])(f: (A, B) => C): P[C] = {
    for {
      a <- p
      b <- p2
    } yield f(a, b)
  }


  def flatMap[A, B](p: P[A])(f: A => P[B]): P[B]

  def label[A](msg: String)(p: P[A]): P[A]

  def scope[A](msg: String)(p: P[A]): P[A]

  def attempt[A](p: P[A]): P[A]

  def surround[A](open: P[Any], close: P[Any])(p: => P[A]) = {
    open ~> p <~ close
  }

  def skipLeft[A](open: P[Any], p: => P[A]): P[A] = {
    for {
      o <- open
      e <- p
    } yield {
      e
    }
  }

  def skipRight[A](p: P[A], close: => P[Any]): P[A] = {
    for {
      e <- p
      o <- close
    } yield e
  }

  def sep[A](p: P[A], separator: P[Any]): P[List[A]] = {
    sep1(p, separator) | succeed(List.empty[A])
  }

  def sep1[A](p: P[A], separator: P[Any]): P[List[A]] = {
    for {
      a <- p
      b <- (separator ~> p).many
    } yield a :: b
  }

  def whitespace = "\\s*".r

  //  /** Sequences two parsers, ignoring the result of the first.
  //    * We wrap the ignored half in slice, since we don't care about its result. */
  //  def skipL[B](p: P[Any], p2: => P[B]): P[B] = {
  //    p.slice.map2(p2)((_, b) => b)
  //  }
  //
  //  /** Sequences two parsers, ignoring the result of the second.
  //    * We wrap the ignored half in slice, since we don't care about its result. */
  //  def skipR[A](p: P[A], p2: => P[Any]): P[A] = {
  //    p.map2(p2.slice)((a, _) => a)
  //  }
  //
  //  def whitespace = "\\s*".r
  //
  //  def digits = "\\d+".r
  //
  //  def opt[A](p: P[A]): P[Option[A]] = {
  //    p.map(Some(_)) or succeed(None)
  //  }
  //
  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: P[String] = string("\"") ~> thru("\"").map {
    case s => {
      s.dropRight(1)
    }
  }

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
  def escapedQuoted: P[String] = {
    quoted label "string literal" token
  }


  /** P which consumes reluctantly until it encounters the given string. */
  def thru(s: String): P[String] = (".*?" + Pattern.quote(s)).r

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: P[String] = {
    "[-+]?(\\d+\\.\\d*|\\.\\d+)([Ee][-+]?\\d+)?".r token
  }

  /** Floating point literals, converted to a `Double`. */
  def double: P[Double] = doubleString.map(_.toDouble) label "double literal"

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: P[A]): P[A] = {
    p.! <~ whitespace
  }

  //
  //  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  //  // use `P[Any]` since don't care about result type of separator
  //  def sep[A](p: P[A], p2: P[Any]): P[List[A]] = {
  //    sep1(p, p2) | succeed(List[A]())
  //  }
  //
  //
  //  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  //  def sep1[A](p: P[A], p2: P[Any]): P[List[A]] = {
  //    p.map2((p2 *> p).many)(_ ::: _)
  //  }
  //
  //
  //  /** Parses a sequence of left-associative binary operators with the same precedence. */
  //  def opL[A](p: P[A])(op: P[(A, A) => A]): P[A] = {
  //    p.map2((op ** p).many)((a, b) => b.foldLeft(a) { case (acc, (op, e)) => op(acc, e) })
  //  }
  //
  //
  //  /** Wraps `p` in start/stop delimiters. */
  //  def surround[A](start: P[Any], stop: P[Any])(p: => P[A]) = {
  //    start *> p <* stop
  //  }
  //
  //
  /** A parser that succeeds when given empty input. */
  def eof: P[String] = "\\z".r label "unexpected trailing characters"


  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: P[A]): P[A] = {
    p <~ eof
  }


  case class ParserOps[A](p: P[A]) {

    def token = self.token(p)

    //    def delay = self.delay(p)

    def ! = self.attempt(p)

    def label(msg: String) = self.label(msg)(p)

    def scope(msg: String) = self.scope(msg)(p)

    def |[B >: A](p2: => P[B]): P[B] = self.or(p, p2)

    def or[B >: A](p2: => P[B]): P[B] = self.or(p, p2)

    def map[B](f: A => B): P[B] = self.map(p)(f)

    def slice: P[String] = self.slice(p)

    def ~~[B](p2: => P[B]) = self.product(p, p2)

    def product[B](p2: => P[B]) = ~~(p2)

    def many: P[List[A]] = self.many(p)

    def many1: P[List[A]] = self.many1(p)

    def map2[B, C](p2: => P[B])(f: (A, B) => C): P[C] = self.map2(p, p2)(f)

    def flatMap[B](f: A => P[B]): P[B] = self.flatMap(p)(f)

    def ~>[B](p2: => P[B]) = self.skipLeft(p, p2)

    def <~(p2: => P[Any]) = self.skipRight(p, p2)

    def sep(p2: P[Any]): P[List[A]] = self.sep(p, p2)

    def sep1(p2: P[Any]): P[List[A]] = self.sep1(p, p2)
  }

}

case class Location(input: String, var offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""

  //  def near(n: Int = 10): String = {
  //    (input.slice(offset - n, offset + n) -> offset).toString()
  //  }

  def columnCaret = (" " * (col - 1)) + "^"
}

case class ParseError(
    stack: List[(Location, String)] = List(),
    otherFailures: List[ParseError] = List()
) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def cover[A](msg: String): ParseError =
    ParseError(latestLoc.map((_, msg)).toList)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  //  override def toString: String = {
  //    "ParseError( " + stack.map { case (l, msg) =>
  //      "(near: " + l.near() + ", " + "message: \"" + msg + "\")"
  //    } + ", " + otherFailures.toString() + " )"
  //  }

  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
            collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
          context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
        mapValues(_.map(_._2).mkString("; ")).
        toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col
}

