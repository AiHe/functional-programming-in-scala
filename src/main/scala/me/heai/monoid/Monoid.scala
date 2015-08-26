package me.heai.monoid

import scala.collection.immutable.HashMap

/**
 * Created by aihe on 8/20/15.
 */

object Monoid {

  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  /**
   * exercise 10.1-1
   */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  /**
   * exercise 10.1-2
   */
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  /**
   * exercise 10.103
   */
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  /**
   * exercise 10.1-4
   */
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  /**
   * exercise 10.2
   * @tparam A
   * @return
   */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  /**
   * exercise 10.3
   * @tparam A
   * @return
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2

    override def zero: (A) => A = a => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /**
   * exercise 10.5
   * @param as
   * @param m
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero) {
      case (acc, e) => m.op(acc, f(e))
    }
  }

  /**
   * exercise 10.6
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  /**
   * exercise 10.7
   * @param v
   * @param m
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v match {
      case IndexedSeq() => m.zero
      case IndexedSeq(a) => f(a)
      case a => {
        val (x, y) = v.splitAt(v.size - 1)
        m.op(foldMapV(x, m)(f), foldMapV(y, m)(f))
      }
    }
  }

  import me.heai.parallelism.NonBlockingScalaFuture._

  /**
   * exercise 10.8-1
   * @param m
   * @tparam A
   * @return
   */
  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1, a2)(m.op)

      override def zero: Par[A] = unit(m.zero)
    }
  }

  /**
   * exercise 10.8-2
   * @param v
   * @param m
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v, par(m))(asyncF(f))
  }

  /**
   * exercise 10.9
   * @param ints
   * @return
   */
  def ordered(ints: IndexedSeq[Int]): Boolean = ???


  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /**
   * exercise 10.10
   */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
      case (Part(l, w, r), Stub(b)) => Part(l, w, r + b)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((l1 + r2).isEmpty) 0 else 1), r2)
    }

    override def zero: WC = Stub("")
  }

  /**
   * exercise 10.11
   * @param s
   * @return
   */
  def count(s: String): Int = {
    def helper(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    def unStub(s: String) = if (s.isEmpty) 0 else 1
    foldMapV(s.toCharArray.toIndexedSeq, wcMonoid)(helper) match {
      case Stub(a) => unStub(a)
      case Part(l, w, r) => unStub(l) + w + unStub(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
      foldMap(as)(f.curried)(endoMonoid[B])(z)
    }

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
      foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)
    }

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
    }

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = {
      foldMap(fa)(a => List(a))(listMonoid)
    }
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = {
      foldLeft(as)(mb.zero)((acc, e) => mb.op(acc, f(e)))
    }
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = {
      foldRight(as)(mb.zero)((e, acc) => mb.op(f(e), acc))
    }
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = {
      foldRight(as)(mb.zero)((e, acc) => mb.op(f(e), acc))
    }
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
      foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)
    }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Leaf(v) => f(v)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case Some(v) => f(v, z)
      case None => z
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case Some(v) => f(z, v)
      case None => z
    }

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Some(v) => f(v)
      case None => mb.zero
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      override def op(x: (A, B), y: (A, B)): (A, B) = (A.op(x._1, y._1), B.op(x._2, y._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[(A) => B] {
      override def op(f: (A) => B, g: (A) => B): (A) => B = a => B.op(f(a), g(a))

      override def zero: (A) => B = a => B.zero
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val mapMergeMonoid: Monoid[HashMap[A, Int]] = new Monoid[HashMap[A, Int]] {

      override def op(ma: HashMap[A, Int], mb: HashMap[A, Int]): HashMap[A, Int] = {
        ma.merged(mb) { case ((a, b), (_, d)) => (a, b + d) }
      }

      override def zero: HashMap[A, Int] = HashMap()
    }
    IndexedSeqFoldable.foldMap(as)(a => HashMap(a -> 1))(mapMergeMonoid)
  }


  def main(args: Array[String]) {
    println(bag(IndexedSeq(1, 2, 1, 3, 2, 1)))
  }
}
