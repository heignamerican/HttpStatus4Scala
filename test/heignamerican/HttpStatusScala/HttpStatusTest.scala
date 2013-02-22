package heignamerican.HttpStatusScala
import org.hamcrest.MatcherAssert._
import org.hamcrest.Matchers._
import org.junit.experimental.theories._
import org.junit.runner.RunWith

object HttpStatusTest {
  @(DataPoints @scala.annotation.target.getter)
  val targets: Array[String => Seq[String]] = Array(
    CaseClassPattern.getAnswer,
    NotCaseClassPattern.getAnswer)
}

@RunWith(classOf[Theories])
class HttpStatusTest {
  @Theory def test4xx(function: String => Seq[String]) = {
    val expected = Seq(
      "400 Bad Request",
      "401 Unauthorized",
      "402 Payment Required",
      "403 Forbidden",
      "404 Not Found",
      "405 Method Not Allowed",
      "406 Not Acceptable",
      "407 Proxy Authentication Required",
      "408 Request Timeout",
      "409 Conflict",
      "410 Gone",
      "411 Length Required",
      "412 Precondition Failed",
      "413 Request Entity Too Large",
      "414 Request-URI Too Large",
      "415 Unsupported Media Type",
      "416 Request Range Not Satisfiable",
      "417 Expectation Failed",
      "418 I'm a teapot",
      "422 Unprocessable Entity",
      "423 Locked",
      "424 Failed Dependency",
      "425 No code",
      "426 Upgrade Required",
      "428 Precondition Required",
      "429 Too Many Requests",
      "431 Request Header Fields Too Large",
      "449 Retry with")
    assertThat(function("4"), is(expected))
  }

  @Theory def test40x(function: String => Seq[String]) = {
    val expected = Seq(
      "400 Bad Request",
      "401 Unauthorized",
      "402 Payment Required",
      "403 Forbidden",
      "404 Not Found",
      "405 Method Not Allowed",
      "406 Not Acceptable",
      "407 Proxy Authentication Required",
      "408 Request Timeout",
      "409 Conflict")
    assertThat(function("40"), is(expected))
  }

  @Theory def test500(function: String => Seq[String]) = {
    val expected = Seq("Internal Server Error")
    assertThat(function("500"), is(expected))
  }

  @Theory def test403(function: String => Seq[String]) = {
    val expected = Seq("Forbidden")
    assertThat(function("403"), is(expected))
  }

  @Theory def testBad(function: String => Seq[String]) = {
    val expected = Seq(
      "400 Bad Request",
      "502 Bad Gateway")
    assertThat(function("Bad"), is(expected))
  }
  @Theory def testAll(function: String => Seq[String]) = {
    val expected = Seq(
      "100 Continue",
      "101 Switching Protocols",
      "102 Processing",
      "200 OK",
      "201 Created",
      "202 Accepted",
      "203 Non-Authoritative Information",
      "204 No Content",
      "205 Reset Content",
      "206 Partial Content",
      "207 Multi-Status",
      "208 Already Reported",
      "300 Multiple Choices",
      "301 Moved Permanently",
      "302 Found",
      "303 See Other",
      "304 Not Modified",
      "305 Use Proxy",
      "307 Temporary Redirect",
      "400 Bad Request",
      "401 Unauthorized",
      "402 Payment Required",
      "403 Forbidden",
      "404 Not Found",
      "405 Method Not Allowed",
      "406 Not Acceptable",
      "407 Proxy Authentication Required",
      "408 Request Timeout",
      "409 Conflict",
      "410 Gone",
      "411 Length Required",
      "412 Precondition Failed",
      "413 Request Entity Too Large",
      "414 Request-URI Too Large",
      "415 Unsupported Media Type",
      "416 Request Range Not Satisfiable",
      "417 Expectation Failed",
      "418 I'm a teapot",
      "422 Unprocessable Entity",
      "423 Locked",
      "424 Failed Dependency",
      "425 No code",
      "426 Upgrade Required",
      "428 Precondition Required",
      "429 Too Many Requests",
      "431 Request Header Fields Too Large",
      "449 Retry with",
      "500 Internal Server Error",
      "501 Not Implemented",
      "502 Bad Gateway",
      "503 Service Unavailable",
      "504 Gateway Timeout",
      "505 HTTP Version Not Supported",
      "506 Variant Also Negotiates",
      "507 Insufficient Storage",
      "509 Bandwidth Limit Exceeded",
      "510 Not Extended",
      "511 Network Authentication Required")
    assertThat(function(""), is(expected))
  }

  @Theory def test10(function: String => Seq[String]) = {
    val expected = Seq(
      "100 Continue",
      "101 Switching Protocols",
      "102 Processing")
    assertThat(function("10"), is(expected))
  }

  @Theory def testArienai(function: String => Seq[String]) = {
    val expected: Seq[String] = Seq()
    assertThat(function("3hoge"), is(expected))
  }
}
