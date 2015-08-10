def f(a: => Int): Int = a

def inf() = {
  while(1 > 0){

  }
  1
}

def ff = f(inf())

def g(a: Int)(f: Int => Int): Int = {
  f(a)
}

g(f(1))(_*1)

case class A()
type X = A => Int


def fork(a: X) = a

def loop: X = {
  a: A => inf
}

def map(x: => X): X = {
  a: A => x(a)
}

//val af: X = a => a.getA()
//
//val bf = b(af)((a: A) => new A(a.getA() * 2))
//
//bf(new A(1))

fork(loop)

map(loop)(new A())


