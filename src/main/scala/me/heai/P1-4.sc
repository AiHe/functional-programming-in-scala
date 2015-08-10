import me.heai.errorhandling.{Either, Right, Left}

/**
 * exercise 4.2-1
 * @param xs
 * @return
 */
def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.size)
}

mean(List[Double]())
mean(List(1.))
mean(List(1., 2.))


/**
 * exercise 4.2-2
 * @param xs
 * @return
 */
def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs).flatMap((f: Double) => mean(xs.map(x => Math.pow(x - f, 2))))
}


variance(List[Double]())
variance(List(1.))
variance(List(1., 2.))


/**
 * exercise 4.3
 * @param a
 * @param b
 * @param f
 * @tparam A
 * @tparam B
 * @tparam C
 * @return
 */
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  for {
    x <- a
    y <- b
  } yield f(x, y)
}

map2(None: Option[Int], None: Option[Int])(_ + _)
map2(Some(1), None: Option[Int])(_ + _)
map2(None: Option[Int], Some(1))(_ + _)
map2(Some(1), Some(1))(_ + _)


/**
 * exercise 4.4-1
 * @param a
 * @tparam A
 * @return
 */
def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap((x: A) => sequence(t) map((y: List[A]) => x :: y))
  }
}


/**
 * exercise 4.4-2
 * @param a
 * @tparam A
 * @return
 */
def sequenceViaFold[A](a: List[Option[A]]): Option[List[A]] = {
  a.foldRight(Some(List[A]()): Option[List[A]]){
    (e: Option[A], acc: Option[List[A]]) => map2(e, acc)(_ :: _)
  }
}

/**
 * exercise 4.5
 * @param a
 * @param f
 * @tparam A
 * @tparam B
 * @return
 */
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
  a.foldRight(Some(List[B]()): Option[List[B]]){
    (e: A, acc: Option[List[B]]) => map2(f(e), acc)(_ :: _)
  }
}


/**
 * exercise 4.6-1
 * @param es
 * @tparam E
 * @tparam A
 * @return
 */
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
  es.foldRight(Right(List[A]()): Either[E, List[A]]){
    (e: Either[E, A], acc: Either[E, List[A]]) => e.map2(acc)(_ :: _)
  }
}


/**
 * exercise 4.6-2
 * @param as
 * @param f
 * @tparam E
 * @tparam A
 * @tparam B
 * @return
 */
def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
  as.foldRight(Right(List[B]()): Either[E, List[B]]){
    (e: A, acc: Either[E, List[B]]) => f(e).map2(acc)(_ :: _)
  }
}



