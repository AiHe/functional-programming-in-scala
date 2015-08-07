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
/**
 * exercise 3.16
 * @param l
 * @return
 */
def add1(l: List[Int]): List[Int] = {
  foldRightViaFoldLeft(l, List[Int]()){
    (e: Int, acc: List[Int]) => Cons(e + 1, acc)
  }
}

add1(List())
add1(List(1))
add1(List(1, 2))
/**
 * exercise 3.17
 * @param l
 * @return
 */
def doubleToString(l: List[Double]): List[String] = {
  foldRightViaFoldLeft(l, List[String]()){
    (e: Double, acc: List[String]) => Cons(e.toString, acc)
  }
}

doubleToString(List())
doubleToString(List(1.))
doubleToString(List(1., 2.))
/**
 * exercise 3.18
 * @param as
 * @param f
 * @tparam A
 * @tparam B
 * @return
 */
def map[A,B](as: List[A])(f: A => B): List[B] = {
  foldRightViaFoldLeft(as, List[B]()){
    (e: A, acc: List[B]) => Cons(f(e), acc)
  }
}

map(List[Int]())(_.toDouble)
map(List(1))(_.toDouble)
map(List(1, 2))(_.toString)
/**
 * exercise 3.19
 * @param as
 * @param f
 * @tparam A
 * @return
 */
def filter[A](as: List[A])(f: A => Boolean): List[A] = {
  foldRightViaFoldLeft(as, List[A]()){
    (e: A, acc: List[A]) => {
      if (f(e)) Cons(e, acc) else acc
    }
  }
}

filter(List[Int]())( _ % 2 == 0)
filter(List[Int](1))( _ % 2 == 0)
filter(List[Int](1, 2))( _ % 2 == 0)
/**
 * exercise 3.20
 * @param as
 * @param f
 * @tparam A
 * @tparam B
 * @return
 */
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
  concat(map(as)(f))
}
flatMap(List[Int]())((e: Int) => List(e.toString, e.toString))
flatMap(List[Int](1))((e: Int) => List(e.toString, e.toString))
flatMap(List[Int](1, 2))((e: Int) => List(e.toString, e.toString))
/**
 * exercise 3.21
 * @param l
 * @param f
 * @tparam A
 * @return
 */
def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
  flatMap(l) {
    (e: A) => if (f(e)) List(e) else Nil
  }
}

filterViaFlatMap(List[Int]())( _ % 2 == 0)
filterViaFlatMap(List[Int](1))( _ % 2 == 0)
filterViaFlatMap(List[Int](1, 2))( _ % 2 == 0)
/**
 * exercise 3.22
 * @param a
 * @param b
 * @return
 */
def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
  (a, b) match {
    case (Cons(ah: Int, at), Cons(bh: Int, bt)) => Cons(ah + bh, addPairwise(at, bt))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }
}

addPairwise(List[Int](), List[Int]())
addPairwise(List[Int](), List[Int](1))
addPairwise(List[Int](1), List[Int]())
addPairwise(List[Int](1), List[Int](1))
addPairwise(List[Int](1), List[Int](1, 2))
addPairwise(List[Int](1, 2), List[Int](1))
addPairwise(List[Int](1,2 ), List[Int](1, 2))
/**
 * exercise 3.23
 * @param a
 * @param b
 * @param f
 * @tparam A
 * @tparam B
 * @tparam C
 * @return
 */
def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = {
  (a, b) match {
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }
}

zipWith(List[Int](), List[Int]())(_ + _)
zipWith(List[Int](), List[Int](1))(_ + _)
zipWith(List[Int](1), List[Int]())(_ + _)
zipWith(List[Int](1), List[Int](1))(_ + _)
zipWith(List[Int](1), List[Int](1, 2))(_ + _)
zipWith(List[Int](1, 2), List[Int](1))(_ + _)
zipWith(List[Int](1,2 ), List[Int](1, 2))(_ + _)
/**
 * exercise 3.24
 * @param sup
 * @param sub
 * @tparam A
 * @return
 */
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  def containsFromStart(a: List[A], b: List[A]): Boolean = {
    (a, b) match {
      case (_, Nil) => true
      case (Cons(ah, at), Cons(bh, bt)) if ah == bh => containsFromStart(at, bt)
      case _ => false
    }
  }
  sup match {
    case Nil => sub == Nil
    case Cons(head, tail) => if (containsFromStart(sup, sub)) true else hasSubsequence(tail, sub)
  }
}

hasSubsequence(List[Int](), List[Int]())
hasSubsequence(List[Int](), List[Int](1))
hasSubsequence(List[Int](1), List[Int]())
hasSubsequence(List[Int](1), List[Int](1))
hasSubsequence(List[Int](1, 2), List[Int](1))
hasSubsequence(List[Int](1, 2), List[Int](2))
hasSubsequence(List[Int](1, 2, 3), List[Int](2))
hasSubsequence(List[Int](1), List[Int](1, 2))
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
val sampleTree1 = Leaf[Int](0)
val sampleTree2 = Branch[Int](Leaf[Int](1), Leaf[Int](2))
/**
 * exercise 3.25
 * @param t
 * @tparam A
 * @return
 */
def size[A](t: Tree[A]): Int = {
  t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }
}

size(sampleTree1)
size(sampleTree2)
def dfs[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = {
  t match {
    case Leaf(a) => f(a, z)
    case Branch(left, right) => dfs(right, dfs(left, z)(f))(f)
  }
}

/**
 * exercise 3.26
 * @param t
 * @return
 */
def maximum(t: Tree[Int]): Int = {
  t match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left) max maximum(right)
  }
}

maximum(sampleTree1)
maximum(sampleTree2)
/**
 * exercise 3.27
 * @param t
 * @tparam A
 * @return
 */
def depth[A](t: Tree[A]): Int = {
  t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }
}

depth(sampleTree1)
depth(sampleTree2)

/**
 * exercise 3.28
 * @param t
 * @param f
 * @tparam A
 * @tparam B
 * @return
 */
def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
  t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}


map(sampleTree1)(_ + 1)
map(sampleTree2)(_ + 1)

/**
 * exercise 3.29-1
 * @param t
 * @param f
 * @param g
 * @tparam A
 * @tparam B
 * @return
 */
def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
  t match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }
}

/**
 * exercise 3.29-2
 * @param t
 * @tparam A
 * @return
 */
def sizeViaFold[A](t: Tree[A]): Int = {
  fold(t)(_ => 1)(_ + _)
}
sizeViaFold(sampleTree1)
sizeViaFold(sampleTree2)

/**
 * exercise 3.29-3
 * @param t
 * @return
 */
def maximumViaFold(t: Tree[Int]): Int = {
  fold(t)((a: Int) => a)((a: Int, b: Int) => a max b)
}
maximumViaFold(sampleTree1)
maximumViaFold(sampleTree2)

/**
 * exercise 3.29-4
 * @param t
 * @tparam A
 * @return
 */
def depthViaFold[A](t: Tree[A]): Int = {
  fold(t)(_ => 1)((a: Int, b: Int) => (a max b) + 1)
}

depthViaFold(sampleTree1)
depthViaFold(sampleTree2)





