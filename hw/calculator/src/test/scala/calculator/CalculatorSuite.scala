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
         "c" -> Signal(Divide(Ref("a"), Two)),
         "d" -> Signal(Plus(Ref("a"), Ref("c"))),
         "e" -> Signal(Plus(Ref("e"), One))
       )

     val pathologicalReference: Map[String, Signal[Expr]] =
      Map(
        "a" -> Signal(Ref("a")),
        "b" -> Signal(Plus(Ref("a"), Ref("b"))),
        "c" -> Signal(Plus(Ref("c"), One)),
        "d" -> Signal(Plus(Ref("e"), Two)),
        "e" -> Signal(Plus(Ref("d"), One))
      )

     val badRef: Map[String, Signal[Expr]] =
      Map(
        "a" -> Signal(Plus(Ref("b"), One)),
        "b" -> Signal(Times(Two, Ref("a")))
      )

     val expected =
       Map(
         "a" -> Signal(1),
         "b" -> Signal(3),
         "c" -> Signal(2),
         "d" -> Signal(2.5)
       )
   }

  test("should evaluate an expression") {

    new calcTestThings {
      assert(Calculator.eval(One, namedExpressions1) == 1)
      assert(Calculator.eval(Ref("a"), namedExpressions1) == 1)
      assert(Calculator.eval(Ref("b"), namedExpressions1) == 3)
      assert(Calculator.eval(Ref("c"), namedExpressions1) == 0.5)

      assert(Calculator.eval(Plus(One, Two), namedExpressions1) == 3)
      assert(Calculator.eval(Minus(Three, One), namedExpressions1) == 2)
      assert(Calculator.eval(Times(Three, Two), namedExpressions1) == 6)
      assert(Calculator.eval(Divide(Literal(8), Two), namedExpressions1) == 4)

      //Infinite or NaN cases
      assert( Calculator.eval(Divide(Literal(1), Literal(0)), namedExpressions1).isInfinity )
      assert( Calculator.eval(Divide(Literal(0), Literal(0)), namedExpressions1).isNaN )
      assert( Calculator.eval(Ref("a"), pathologicalReference).isNaN )
      assert( Calculator.eval(Ref("f"), pathologicalReference).isNaN )
      assert( Calculator.eval(Ref("b"), pathologicalReference).isNaN )
      assert( Calculator.eval(Ref("c"), pathologicalReference).isNaN )
      assert( Calculator.eval(Ref("d"), pathologicalReference).isNaN )
      assert( Calculator.eval(Ref("e"), pathologicalReference).isNaN )
    }

  }

  test("computeValues returns a map of names to their evaluated expressions") {
    new calcTestThings {
      println(Calculator.computeValues(badRef).values.toSet.size)
      assert(Calculator.computeValues(namedExpressions1).toSet.size == expected.toSet.size)
    }
  }
}
