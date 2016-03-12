package me.heai.parsing

import me.heai.parsing.JSON.{JObject, JArray}
import me.heai.parsing.MyParserTypes._

/**
  * Created by aihe on 9/3/15.
  */
trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: Seq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+ _]](P: ParsersMonad[Parser]): Parser[JSON] = {

    import P._
    //    implicit def tok(s: String): Parser[String] = P.token(P.string(s))

    def array = surround("[", "]")(value.sep(",").map(JArray(_))) scope
        "array"

    def keyValue = (escapedQuoted ~~ (":" ~> value)) scope "kv"

    def obj = surround("{", "}")(keyValue.sep(",").map(
      kv => JObject(kv.toMap))
    ) scope "object"

    def literal = {
      double.map(JNumber) |
          escapedQuoted.map(JString) |
          string("null").map(_ => JNull) |
          string("true").map(_ => JBool(true)) |
          string("false").map(_ => JBool(false))
    } scope "literal"

    def value: Parser[JSON] = array | obj | literal

    root(whitespace ~> (obj | array))

  }
}

/**
  * JSON parsing example.
  */
object JSONExample extends App {

  val R = me.heai.parsing.MyParser

  import me.heai.parsing.MyParserTypes.Parser

  val jsonTxt =
    """
  {
    "Company name" : "Microsoft Corporation",
    "Ticker"  : "MSFT",
    "Active"  : true,
    "Price"   : 30.66,
    "Shares outstanding" : 8.38e9,
    "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
  }
    """

  def printResult(e: Either[ParseError, JSON]) =
    e.fold(println, println)

  val json: Parser[JSON] = JSON.jsonParser(R)

  printResult {
    R.run(json)(jsonTxt)
  }

  //
  //  val malformedJson1 =
  //    """
  //{
  //  "Company name" ; "Microsoft Corporation"
  //}
  //    """
  //
  //  val malformedJson2 =
  //    """
  //[
  //  [ "HPQ", "IBM",
  //  "YHOO", "DELL" ++
  //  "GOOG"
  //  ]
  //]
  //    """


  //
  //    val data: Array[String] = io.Source.fromFile("src/main/resources/birds2.txt")
  //        .getLines().toArray
  //
  //    def Test[A](name: String): Long = {
  //      val start_time = System.currentTimeMillis()
  //      data.foreach(R.run(json))
  //      val end_time = System.currentTimeMillis()
  //      end_time - start_time
  //    }
  //
  //    val name = "test"
  //
  //    val results = for {
  //      iteration <- (1 to 1).iterator
  //    } yield {
  //      val time = Test("test")
  //      f"$name%7s $time%05d $iteration%02d"
  //    }
  //
  //    results foreach println


  //  println("--")
  //  printResult {
  //    R.run(json)(malformedJson1)
  //  }
  //  println("--")
  //  printResult {
  //    R.run(json)(malformedJson2)
  //  }
}
