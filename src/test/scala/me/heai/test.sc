trait HttpMethod {
  def body: String
  def bodyLength: Int = body.length
}
case class Connect(var body: String) extends HttpMethod
//case class Delete (body: String) extends HttpMethod
//case class Get (body: String) extends HttpMethod
//case class Head (body: String) extends HttpMethod
//case class Options(body: String) extends HttpMethod
//case class Post (body: String) extends HttpMethod
//case class Put (body: String) extends HttpMethod
//case class Trace (body: String) extends HttpMethod

new Connect("1123").bodyLength

abstract class A {
  type In
  def read: String
}

class B(val source: String) extends A {
  type In = String
  def read: String = source
}

new B("b").read

abstract class AA {
  type In
  val source: In
  def read: String
}

class BB(val source: String) extends AA {
  type In = String
  def read: String = source
}

new BB("bb").read

//class C extends AA {
//  type In = String
//  def read: String = source
//}

abstract class AAA[In] {
  val source: In
  def read: String
}

//class BBB(val source: Int) extends AAA[String] {
//  def read = source.toString
//}
//
//new BBB(222).source


class CCC(val source: String) extends AAA[String] {
  def read = source.toString
}
new CCC("ccc").read

1 max 2

"1".take(1)

"1".substring(1)

val a: Any = Array("1", "2", "3")

a match {
  case p: Array[Int] => println("int")
  case l: Array[String] => println("string")
}

val b: Any = List(1, 2, 3)
b match {
  case l: List[String] => println("string")
  case p: List[Int] => println("int")
}