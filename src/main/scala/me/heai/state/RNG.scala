package me.heai.state

/**
 * Created by aihe on 8/10/15.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  /**
   * exercise 6.1
   * @param rng
   * @return
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }

  /**
   * exercise 6.2
   * @param rng
   * @return
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ((i.toDouble / (Int.MaxValue + 1)), r)
  }

  /**
   * exercise 6.3-1
   * @param rng
   * @return
   */
  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i % 2 == 0, r)
  }

  /**
   * exercise 6.3-2
   * @param rng
   * @return
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  /**
   * exercise 6.3-3
   * @param rng
   * @return
   */
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  /**
   * exercis3 6.3-4
   * @param rng
   * @return
   */
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
   * exercise 6.4
   * @param count
   * @param rng
   * @return
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (l, r) = (1 to count).foldLeft((List[Int](), rng)){
      case ((acc, r), _) => {
        val (i, rr) =r.nextInt
        (i :: acc, rr)
      }
    }
    (l.reverse, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  /**
   * exercise 6.5
   */
  val doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  /**
   * exercise 6.6
   * @param ra
   * @param rb
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }
  }

  /**
   * exercise 6.7-1
   * @param fs
   * @tparam A
   * @return
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//    rng => {
//      val (l, r) = fs.foldLeft((List[A](), rng)){
//        case ((acc, r), e) => {
//          val (a, ra) = e(r)
//          (a :: acc, ra)
//        }
//      }
//      (l.reverse, r)
//    }
    fs.foldRight(unit(List[A]()))((e, acc) => map2(e, acc)(_ :: _))
  }

  /**
   * exercise 6.7-2
   * @param count
   * @return
   */
  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }


  /**
   * exercise 6.8-1
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }

  /**
   * exercise 6.8-2
   * @param n
   * @return
   */
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){
      i => if (i + (n-1) - (i % n) >= 0) unit(i % n) else nonNegativeLessThan(n)
    }
  }


  /**
   * exercise 6.9-1
   * @param s
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s){
      a => unit(f(a))
    }
  }

  /**
   * exercise 6.9-2
   * @param ra
   * @param rb
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a ,b))))
  }
}

import State._

case class State[S, +A](run: S => (A, S)) {

  /**
   * exercise 6.10-1
   * @param f
   * @tparam B
   * @return
   */
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, ss) = run(s)
      f(a).run(ss)
    })
  }

  /**
   * exercise 6.10-2
   * @param f
   * @tparam B
   * @return
   */
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  /**
   * exercise 6.10-3
   * @param sb
   * @param f
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      x <- this
      y <- sb
    } yield f(x, y)
  }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}


























