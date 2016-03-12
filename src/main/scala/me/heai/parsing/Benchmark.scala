package me.heai.parsing

import org.json4s._
import rapture.json.jsonBackends.jackson._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io._

/**
  * Created by nlw on 18/04/15.
  *
  */
object Benchmark extends App {

  val data: Array[String] = Source.fromFile("src/main/resources/birds2.txt")
      .getLines().toArray

  // Do some stupid work to "warm up" the JVM, no idea if this makes any difference at all
  var yy = 0L
  for (x <- 1L to 100000L) {
    yy += x * x
  }

  def Test[A](name: String)(f: String => A): Long = {
    val start_time = System.currentTimeMillis()
    val parsed_data = data map f
    val end_time = System.currentTimeMillis()
    end_time - start_time
  }

  def theFuncs: Map[String, BenchmarkParser] = Map(
    "json4s" -> new Json4sParser,
    "spray" -> new SprayParser,
    "rapture" -> new RaptureParser,
    "lift" -> new LiftParser,
    "play" -> new PlayParser
  )

  val results = for {
    iteration <- (1 to 3).iterator
    (name, func) <- theFuncs.iterator
  } yield {
    val time = Test(name)(func.apply)
    f"$name%7s $time%05d $iteration%02d"
  }

  results foreach println
}


trait BenchmarkParser {
  def apply(s: String): Bird
}

class Json4sParser extends BenchmarkParser {
  implicit val formats = org.json4s.DefaultFormats

  def apply(s: String) = {
    org.json4s.jackson.JsonMethods.parse(s).extract[Bird]
  }
}

class RaptureParser extends BenchmarkParser {
  def apply(s: String) = {
    rapture.json.Json.parse(s).as[Bird]
  }
}

class LiftParser extends BenchmarkParser {
  implicit val formats = net.liftweb.json.DefaultFormats

  def apply(s: String) = {
    net.liftweb.json.parse(s).extract[Bird]
  }
}

class PlayParser extends BenchmarkParser {
  implicit val subReads = play.api.libs.json.Json.reads[Place]
  implicit val rowReads = play.api.libs.json.Json.reads[Bird]

  def apply(s: String) = {
    play.api.libs.json.Json.parse(s).as[Bird]
  }
}

class SprayParser extends BenchmarkParser {
  implicit val examFormat = jsonFormat5(Place)
  implicit val rowFormat = jsonFormat5(Bird)

  def apply(s: String) = {
    s.parseJson.convertTo[Bird]
  }
}
