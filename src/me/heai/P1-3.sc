
sealed trait List[+A] {
  override def toString() = {
    this match {
      case Cons(a, b) => a.toString + "->" + b.toString
      case _ => "^"
    }
  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))
println(ex1)
println(ex2)
println(ex3)
/**
 * exercise 3.1
 */
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

// Using the same idea, implement the function
// setHead for replacing the first element of
// a List with a different value.
/**
 * exercise 3.2
 * @param l
 * @param a
 * @tparam A
 * @return
 */
def setHead[A](l: List[A], a: A): List[A] = {
  l match {
    case Nil => throw new Exception("set head at empty list")
    case Cons(head, tail) => Cons(a, tail)
  }
}

//setHead(Nil, 1)
setHead(List(3, 2, 1), 1)

/**
 * exercise 3.3
 * @param l
 * @param n
 * @tparam A
 * @return
 */
def drop[A](l: List[A], n: Int): List[A] = {
  if (n <= 0) l
  else {
    l match {
      case Nil => Nil
      case Cons(a, b) => drop(b, n-1)
    }
  }
}

drop(List(1, 2, 3), 0)
drop(List(1, 2, 3), 1)
drop(List(1, 2, 3), 2)
drop(List(1, 2, 3), 3)
drop(List(1, 2, 3), 4)
/**
 * exercise 3.4
 * @param l
 * @param f
 * @tparam A
 * @return
 */
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  l match {
    case Nil => Nil
    case Cons(a, b) => {
      if(f(a)) dropWhile(b, f)
      else l
    }
  }
}

dropWhile(List(1, 2, 3), (a: Int) => a > 1)
dropWhile(List(1, 2, 3), (a: Int) => a >= 1)
dropWhile(List(1, 2, 3), (a: Int) => a == 1)
dropWhile(List(1, 2, 3), (a: Int) => a <= 1)
dropWhile(List(1, 2, 3), (a: Int) => a < 1)
dropWhile(List(1, 2, 3), (a: Int) => a <= 2)
def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
}

def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
/**
 * exercise 3.9
 * @param as
 * @tparam A
 * @return
 */
def length[A](as: List[A]): Int = {
  foldRight(as, 0){
    (e: A, acc: Int) => acc + 1
  }
}

length(Nil)
length(List(1))
length(List(1, 2, 3))
/**
 * exercise 3.10
 * @param as
 * @param z
 * @param f
 * @tparam A
 * @tparam B
 * @return
 */
def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }
}

/**
 * exercise 3.11-1
 * @param ns
 * @return
 */
def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

sum3(List())
sum3(List(1))
sum3(List(1, 2))
/**
 * exercise 3.11-2
 * @param ns
 */
def product3(ns: List[Double]) = foldLeft(ns, 1.)(_ * _)

product3(List())
product3(List(1))
product3(List(1, 2))
/**
 * exercise 3.12
 * @param as
 * @tparam A
 * @return
 */
def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]()){
  (acc: List[A], e: A) => Cons(e, acc)
}
reverse(List())
reverse(List(1))
reverse(List(1, 2))
/**
 * exercise 3.13
 * @param l
 * @param z
 * @param f
 * @tparam A
 * @tparam B
 * @return
 */
def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
  foldLeft(reverse(l), z){
    (acc:B, e:A) => f(e, acc)
  }
}

/**
 * exercise 3.14-1
 * @param l
 * @param r
 * @tparam A
 * @return
 */
def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
  foldRightViaFoldLeft(l, r){
    (e: A, acc: List[A]) => Cons(e, acc)
  }
}

appendViaFoldRight(List(), List(1, 2))
appendViaFoldRight(List(1, 2), List())
appendViaFoldRight(List(1, 2), List(3, 4))
/**
 * exercise 3.14-2
 * @param l
 * @param r
 * @tparam A
 * @return
 */
def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = {
  foldLeft(reverse(l), r){
    (acc: List[A], e: A) => Cons(e, acc)
  }
}

appendViaFoldLeft(List(), List(1, 2))
appendViaFoldLeft(List(1, 2), List())
appendViaFoldLeft(List(1, 2), List(3, 4))
/**
 * exercise 3.15
 * @param l
 * @tparam A
 * @return
 */
def concat[A](l: List[List[A]]): List[A] = {
  foldRightViaFoldLeft(l, List[A]())(appendViaFoldRight)
}
concat(List(List(), List(), List()))
concat(List(List(), List(), List(1)))
concat(List(List(), List(1), List()))
concat(List(List(1), List(), List()))
concat(List(List(1), List(1), List()))
concat(List(List(), List(1), List(1)))
concat(List(List(1), List(), List(1)))
concat(List(List(1), List(1), List(1)))
