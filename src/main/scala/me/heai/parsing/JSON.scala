package me.heai.parsing

/**
 * Created by aihe on 9/3/15.
 */
trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[ParseError, Parser[+ _]](P: Parsers[ParseError, Parser]): Parser[JSON] = {
    import P._

    //    val spaces = char(' ').many.slice

    def array = surround("[", "]")(value.sep(",").map(JArray(_)))

    def obj = surround("{", "}")(keyValue.sep(",").map(kv => JObject(kv.toMap)))

    def keyValue = escapedQuoted ** (":" *> value)

    def literal = double.map(JBool(_)) | escapedQuoted.map(JString(_)) | string("null").map(_ => JNull) |
      string("true").map(_ => JBool(true)) | string("false").map(_ => JBool(false))

    def value: Parser[JSON] = array | obj | literal

    root(whitespace *> (array | obj))

  }
}

/**
 * JSON parsing example.
 */
object JSONExample extends App {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
                """

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
                       """

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
                       """

  val P = me.heai.parsing.Reference

  import me.heai.parsing.ReferenceTypes.Parser

  def printResult[E](e: Either[E, JSON]) =
    e.fold(println, println)

  val json: Parser[JSON] = JSON.jsonParser(P)
  printResult {
    P.run(json)(jsonTxt)
  }
  println("--")
  printResult {
    P.run(json)(malformedJson1)
  }
  println("--")
  printResult {
    P.run(json)(malformedJson2)
  }
}
