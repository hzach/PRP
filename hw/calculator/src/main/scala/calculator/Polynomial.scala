package calculator
import math._
object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var{ pow(b(),2) - 4*a()*c() }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      if (delta() < 0 ) Set()
      else {
        val left = (-b() - sqrt(delta())) / (2 * a())
        val right = (-b() + sqrt(delta())) / (2 * a())
        Set(left, right)
      }
    }
  }
}
