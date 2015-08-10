package me.heai.laziness

/**
 * Created by aihe on 8/7/15.
 */
sealed trait Stream[+A] {

  import Stream._
  import me.heai.errorhandling._

  /**
   * exercise 5.1
   * @return
   */
  def toList: List[A] = {
    def helper(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Cons(h, t) => helper(t(), h() :: acc)
        case _ => acc
      }
    }
    helper(this, List[A]()).reverse
  }

  /**
   * exercise 5.2-1
   * @param n
   * @return
   */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), Empty)
      case _ => Empty
    }
  }

  /**
   * exercise 5.2-2
   * @param n
   * @return
   */
  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => t().drop(n - 1)
      case _ => this
    }
  }

  /**
   * exercise 5.3
   * @param p
   * @return
   */
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }
  }


  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((e, acc) => acc || p(e))
  }

  /**
   * exercise 5.4-1
   * @param p
   * @return
   */
  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }
  }

  /**
   * exercise 5.4-2
   * @param p
   * @return
   */
  def forAllViaFoldRight(p: A => Boolean): Boolean = {
    foldRight(true){case (e, acc) => acc && p(e)}
  }

  /**
   * exercise 5.5
   * @param f
   * @return
   */
  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] = {
    foldRight(empty[A]) {
      case (e, acc) => {
        if (f(e)) cons(e, acc)
        else empty
      }
    }
  }


  /**
   * exercise 5.6
   * @return
   */
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
  }

  /**
   * exercise 5.7-1
   * @param f
   * @tparam B
   * @return
   */
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B]) {
      (e, acc) => cons(f(e), acc)
    }
  }

  /**
   * exercise 5.7-2
   * @param s
   * @tparam B
   * @return
   */
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s) {
      (e, acc) => cons(e, acc)
    }
  }

  /**
   * exercise 5.7-3
   * @param f
   * @tparam B
   * @return
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B]) {
      (e, acc) => f(e).append(acc)
    }
  }

  /**
   * exercise 5.7-4
   * @param f
   * @return
   */
  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A]) {
      (e, acc) => if (f(e)) cons(e, acc) else acc
    }
  }

//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]

  /**
   * exercise 5.13-1
   * @param f
   * @tparam B
   * @return
   */
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }


  /**
   * exercise 5.13-2
   * @param n
   * @return
   */
  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)){
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }
  }

  /**
   * exercise 5.13-3
   * @param f
   * @return
   */
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = {
    unfold(this){
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }
  }

  /**
   * exercise 5.13-4
   * @param s2
   * @param f
   * @tparam B
   * @tparam C
   * @return
   */
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  /**
   * exercise 5.13-5
   * @param s2
   * @tparam B
   * @return
   */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case (Empty, Empty) => None
    }
  }

  /**
   * exercise 5.14
   * @param s
   * @tparam A
   * @return
   */
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhileViaUnfold(!_._2.isEmpty).forAllViaFoldRight{
      case (Some(h1), Some(h2)) if h1 == h2 => true
      case _ => false
    }
  }


  /**
   * exercise 5.15
   * @return
   */
  def tails: Stream[Stream[A]] = {
    unfold(this){
      case s@Cons(h, t) => Some((s, t()))
      case Empty => None
    }.append(Stream(empty))
  }

  /**
   * exercise 5.16
   * @param z
   * @param f
   * @tparam B
   * @return
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight(z, Stream(z)){
      case (e, (acc1, acc2)) => {
        val tmp: B = f(e, acc1)
        (tmp, cons(tmp, acc2))
      }
    }._2
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  import me.heai.errorhandling._

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  /**
   * exercise 5.8
   * @param a
   * @tparam A
   * @return
   */
  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  /**
   * exercise 5.9
   * @param n
   * @return
   */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /**
   * exercise 5.10
   * @return
   */
  val fibs: Stream[Int] = {
    def helper(a: Int, b: Int): Stream[Int] = {
      cons(a, helper(b, a + b))
    }
    helper(0, 1)
  }

  /**
   * exercise 5.11
   * @param z
   * @param f
   * @tparam A
   * @tparam S
   * @return
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }


  /**
   * exercise 5.12-1
   */
  val fibsViaUnfold: Stream[Int] = unfold((0, 1)){
    case (a: Int, b: Int) => Some((a, (b, a+b)))
  }

  /**
   * exercise 5.12-2
   * @param n
   * @return
   */
  def fromViaUnfold(n: Int) = {
    unfold(n){
      a => Some((a, a+1))
    }
  }

  /**
   * exercise 5.12-3
   * @param a
   * @tparam A
   * @return
   */
  def constantViaUnfold[A](a: A) = {
    unfold(a){
      _ => Some((a, a))
    }
  }

  /**
   * exercise 5.12-4
   */
  val onesViaUnfold = {
    unfold(1){
      _ => Some((1, 1))
    }
  }

}























