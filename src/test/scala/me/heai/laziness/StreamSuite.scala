package me.heai.laziness

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
 * Created by aihe on 8/10/15.
 */
@RunWith(classOf[JUnitRunner])
class StreamSuite extends FunSuite {

  test("empty"){
    assert(Stream.empty[Nothing].toList === Nil)
  }

  trait TestSets {
    val t = Stream(1, 2, 3)
  }

  test("stream toList, [1, 2, 3]") {
    new TestSets {
      assert(t.toList === List(1, 2, 3))
    }
  }

  test("stream map, +1") {
    new TestSets {
      assert(t.map(_ + 1).toList === List(2, 3, 4))
    }
  }

  test("stream append: [1, 2, 3] + []") {
    new TestSets {
      assert(t.append(Stream()).toList === (1 to 3).toList)
    }
  }

  test("stream append: [1, 2, 3] + [4, 5, 6]") {
    new TestSets {
      assert(t.append(Stream(4, 5, 6)).toList === (1 to 6).toList)
    }
  }

  test("stream tails, [1, 2, 3]") {
    new TestSets {
      assert(t.tails.map(_.toList).toList === List(List(1, 2, 3), List(2, 3), List(3), List()))
    }
  }

  test("stream scanRight, [1, 2, 3] 0"){
    new TestSets {
      assert(t.scanRight(0)(_ + _).toList === List(6, 5, 3, 0))
    }
  }
}
