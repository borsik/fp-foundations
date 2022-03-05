package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits all digits") {
    forAll { (text: String) =>
      assert(selectDigits(text).forall(_.isDigit))
    }
  }

  test("secret idempotent") {
    forAll { (text: String) =>
      assert(secret(secret(text)) == secret(text))
    }
  }

  test("is valid username") {
    forAll { (text: String) =>
      assert(isValidUsername(text.reverse) == isValidUsername(text))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("is positive") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("for all") {
    forAll { (x: Int, y: Int, z: Int, pred: Int => Boolean) =>
      assert(Point(x, y, z).forAll(pred) == List(x, y, z).forall(pred))
    }
  }



}
