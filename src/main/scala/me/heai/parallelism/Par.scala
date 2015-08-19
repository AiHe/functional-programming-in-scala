package me.heai.parallelism

//import java.util.concurrent.ExecutorService

import scala.concurrent.duration.TimeUnit

/**
 * Created by aihe on 8/18/15.
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  trait ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
  }

  trait Callable[A] {
    def call: A
  }

  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    es => UnitFuture(f(a(es).get, b(es).get))
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit())((a, _) => f(a))
  }


  //  def map2[A,B,C](a: Par[A], b: Par[B])(timeout: Long, unit: TimeUnit)(f: (A,B) => C): Par[C] = {
//    es => UnitFuture(f(a(es).get(timeout, unit), b(es).get(timeout, unit)))
//  }

  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }


  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]())){
      (e, acc) => map2(e, acc)(_ :: _)
    }
  }

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = {
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    as match {
      case Vector() => unit(Vector())
      case a => {
        val (f, l) = a.splitAt(a.size / 2)
        map2(sequenceBalanced(f), sequenceBalanced(l))(_ ++ _)
      }
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    map(sequenceBalanced(ps.toVector))(_.toList)
  }



}
