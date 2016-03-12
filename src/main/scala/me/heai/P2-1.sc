//def f(a: => Int): Int = a
//
//def inf() = {
//  while(1 > 0){
//
//  }
//  1
//}
//
//def ff = f(inf())
//
//def g(a: Int)(f: Int => Int): Int = {
//  f(a)
//}
//
//g(f(1))(_*1)
//
//case class A()
//type X = A => Int
//
//
//def fork(a: X) = a
//
//def loop: X = {
//  a: A => inf
//}
//
//def map(x: => X): X = {
//  a: A => x(a)
//}
//
////val af: X = a => a.getA()
////
////val bf = b(af)((a: A) => new A(a.getA() * 2))
////
////bf(new A(1))
//
//fork(loop)
//
//map(loop)(new A())

//val e = new Function1[Int => Int, Int] {
//  def apply(x: Int => Int): Int = {
//    println(x.hashCode())
//    x(1)
//  }
//}
//val f = (i: Int) => i * 2
//println(f.hashCode())
//def m(i: Int) = i * 2
//println((m _).hashCode())
//e(f)
//e(m)
//val g = m _
//println(g.hashCode())
//e(m _)
//e(g)
//List(1, 2).sorted
//val a = Array(1, 2)
//a.++=(Array(3))
//var b = Array(3, 4)
//b.++(Array(5))
//println(b)
//b.++=(Array(5))
//println(b)
//
//val l = List(1, 2)
//l.++=(List(3))
//var m = List(1, 2)
//m.++=(List(3))
//
//import scala.collection.mutable.ListBuffer
//val o = ListBuffer(1, 2)
//o.++=(ListBuffer(3))
//var p = ListBuffer(1, 2)
//p.++=(ListBuffer(3))


class A(val a: Int) {
  private[this] val b: Int = 0
  def m(that: A): Unit = {
    println(this.a + " " + that.a)
    println(this.b + " " + that.b)
  }
}

val ao = new A(1)
//println(ao.a)
println(ao.m(new A(2)))


