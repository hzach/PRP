package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

   /******************
    *** CALCULATOR ***
    ******************/

  trait calcTestThings{

     val One = Literal(1)
     val Two = Literal(2)
     val Three = Literal(3)

     val namedExpressions1: Map[String, Signal[Expr]] =
       Map(
         "a" -> Signal(One),
         "b" -> Signal(Times(Ref("a"), Three)),
         "c" -> Signal(Divide(Ref("b"), Two)),
         "d" -> Signal(Plus(Ref("a"), Ref("c")))
       )

     val selfReference: Map[String, Signal[Expr]] =
      Map(
        "a" -> Signal(Ref("a")),
        "b" -> Signal(Plus(Ref("a"), Ref("b"))),
        "c" -> Signal(Plus(Ref("c"), Literal(1)))
      )

     val expected =
       Map(
         "a" -> Signal(1),
         "b" -> Signal(3),
         "c" -> Signal(1.5),
         "d" -> Signal(2.5))
   }

  test("should evaluate an expression") {

    new calcTestThings {
      assert(Calculator.eval(One, namedExpressions1) == 1)
      assert(Calculator.eval(Ref("a"), namedExpressions1) == 1)
      assert(Calculator.eval(Ref("b"), namedExpressions1) == 3)

      assert(Calculator.eval(Plus(One, Two), namedExpressions1) == 3)
      assert(Calculator.eval(Minus(Three, One), namedExpressions1) == 2)
      assert(Calculator.eval(Times(Three, Two), namedExpressions1) == 6)
      assert(Calculator.eval(Divide(Literal(8), Two), namedExpressions1) == 4)

      //NaN cases
      assert(Calculator.eval(Divide(Literal(1),Literal(0)), namedExpressions1) == Double.Infinity)
      //assert(Calculator.eval(Ref("a"), selfReference) == Double.NaN)
    }

  }

  test("computeValues returns a map of names to their evaluated expressions") {
//    new calcTestThings {
//      assert(Calculator.computeValues(namedExpressions1) == expected)
//    }
  }
}
