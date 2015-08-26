package me.heai.parallelism

import java.util.concurrent.{Executors, ExecutorService}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._


/**
 * Created by aihe on 8/19/15.
 */
object NonBlockingScalaFuture {

  type Par[A] = ExecutionContext => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutionContext) => Future.successful(a)


  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es => a(es).zip(b(es)).map { case (a, b) => f(a, b) }(es)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit())((a, _) => f(a))
  }


  def fork[A](a: => Par[A]): Par[A] = a

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }


  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]())) {
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

  def equalSide[A](e: ExecutionContext)(p: Par[A], p2: Par[A]): Unit = {
    p(e).zip(p2(e)).map { case (a, b) => a == b }(e).onSuccess({
      case true => println(true)
      case false => println(false)
    })(e)
  }

  def equal[A](e: ExecutionContext)(p: Par[A], p2: Par[A]): Boolean = {
    Await.result(p(e).zip(p2(e)).map { case (a, b) => a == b }(e), Duration.Inf)

  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)


  def main(args: Array[String]) {
    val a = unit(42 + 1)
    val S = new ExecutionContext {
      val threadPool = Executors.newFixedThreadPool(1)

      def execute(runnable: Runnable) {
        threadPool.submit(runnable)
      }

      def reportFailure(t: Throwable) {}
    }

    println(equal(S)(a, fork(a)))
    equalSide(S)(a, fork(a))

    val b = lazyUnit(42 + 1)
    val T = new ExecutionContext {
      val threadPool = Executors.newFixedThreadPool(1)

      def execute(runnable: Runnable) {
        threadPool.submit(runnable)
      }

      def reportFailure(t: Throwable) {}
    }
    println(equal(T)(b, fork(b)))
    equalSide(T)(b, fork(b))
  }

}
