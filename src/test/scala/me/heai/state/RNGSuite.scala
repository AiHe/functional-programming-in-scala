package me.heai.state

import me.heai.state.RNG.Simple
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


/**
 * Created by aihe on 8/10/15.
 */
@RunWith(classOf[JUnitRunner])
class RNGSuite extends FunSuite {

  val rng = Simple(727)

  println(RNG.ints(5)(rng))

  println(RNG.ints(5)(rng))
}
