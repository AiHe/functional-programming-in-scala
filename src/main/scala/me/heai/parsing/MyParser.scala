package me.heai.parsing

/**
  * Created by aihe on 9/4/15.
  */

import MyParserTypes._

import scala.util.matching.Regex

object MyParserTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = ParseState => Result[A]

  case class ParseState(var loc: Location) {
    def input: String = loc.input.substring(loc.offset)

    def advanceBy(numChars: Int): ParseState = {
//      copy(loc = loc.copy(offset = loc.offset + numChars))
      this.loc = loc.copy(offset = loc.offset + numChars)
      this
    }

    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(pe, c) => Failure(f(pe), c)
      case s@Success(_, _) => s
    }

    def uncommit: Result[A] = this match {
      case f@Failure(pe, true) => {
//        Failure(pe, false)
        f.isCommitted = false
        f
      }
//      case s@Success(a, c) => {
//        s
//      }
      case _ => this
    }

    def addFailureCommit(isCommitted: Boolean): Result[A] = this match {
      case f@Failure(pe, c) => {
        Failure(pe, c || isCommitted)
      }
      case _ => this
    }

    def addSuccessConsumed(more: Int): Result[A] = this match {
      case s@Success(a, n) => {
        Success(a, n + more)
      }
      case _ => this
    }

    def postProcess(isCommitted: Boolean, more: Int): Result[A] = this match {
      case f@Failure(pe, c) => {
        //        Failure(pe, c || isCommitted)
        f.isCommitted ||= isCommitted
        f
      }
      case s@Success(a, n) => {
        //        Success(a, n + more)
        s.charsConsumed += more
        s
      }
      case _ => this
    }
  }

  case class Success[+A](get: A, var charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, var isCommitted: Boolean) extends
      Result[Nothing]

}

object MyParser extends ParsersMonad[Parser] {

  var i = 0
  var j = 0

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = (ps: ParseState) => {
    p1(ps) match {
      case Failure(pe, false) => p2(ps)
      case r => r
    }
  }

  def flatMap[A, B](p: Parser[A])
      (f: (A) => Parser[B]): Parser[B] = (ps: ParseState) => {
    i += 1
    if (i % 1000000 == 0) println(s"i: $i")
    p(ps) match {
      case Success(a, n) => f(a)(ps.advanceBy(n)).
          //          addFailureCommit(n != 0).addSuccessConsumed(n)
          postProcess(n != 0, n)
      case fail@Failure(_, _) => fail
    }
  }

  def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }

  implicit def string(s: String): Parser[String] = {
    (ps: ParseState) => {
      val i = firstNonMatchingIndex(ps.loc.input, s, ps.loc.offset)
      if (i == -1) // they matched
        Success(s, s.length)
      else
        Failure(ps.advanceBy(i).loc.toError(s), i != 0)
    }
  } token


  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(ParseState(Location(input))) match {
      case Success(a, _) => Right(a)
      case Failure(pe, _) => Left(pe)
    }
  }


  implicit def regex(r: Regex): Parser[String] = (ps: ParseState) => {
    r.findPrefixOf(ps.input) match {
      case None => Failure(ps.loc.toError("regex " + r), false)
      case Some(m) => {
        Success(m, m.length)
      }
    }
  }

  def slice[A](p: Parser[A]): Parser[String] = (ps: ParseState) => {
    p(ps) match {
      case Success(a, n) => {
        Success(ps.slice(n), n)
      }
      case f@Failure(_, _) => f
    }
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = (ps: ParseState) => {
    p(ps).mapError(_.push(ps.loc, msg))
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = (ps: ParseState) => {
    p(ps).mapError(_.cover(msg))
  }


  def attempt[A](p: Parser[A]): Parser[A] = (ps: ParseState) => {
    p(ps).uncommit
  }

  def unit[A](a: => A): Parser[A] = succeed(a)

  def succeed[A](a: A): Parser[A] = (ps: ParseState) => {
    Success(a, 0)
  }

  //  /* We provide an overridden version of `many` that accumulates
  //   * the list of results using a monolithic loop. This avoids
  //   * stack overflow errors for most grammars.
  //   */
  //  override def many[A](p: Parser[A]): Parser[List[A]] =
  //    s => {
  //      var nConsumed: Int = 0
  //      val buf = new collection.mutable.ListBuffer[A]
  //      def go(p: Parser[A], offset: Int): Result[List[A]] = {
  //        p(s.advanceBy(offset)) match {
  //          case Success(a,n) => buf += a; go(p, offset+n)
  //          case f@Failure(e,true) => f
  //          case Failure(e,_) => Success(buf.toList,offset)
  //        }
  //      }
  //      go(p, 0)
  //    }
}