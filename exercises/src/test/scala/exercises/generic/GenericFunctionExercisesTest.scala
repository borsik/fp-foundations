package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try
import GenericFunctionExercises._
import exercises.action.DateGenerator.localDateArb

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(1, 2).swap == Pair(2, 1))
  }

  test("Pair map") {
    assert(Pair(1, 2).map(identity) == Pair(1, 2))
  }

  test("Pair decoded") {
    val tmp = GenericFunctionExercises.secret.swap.map(bytes => new String(bytes.reverse.toArray))
    assert(tmp == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") {
    assert(Pair(1, 2).zipWith(Pair(3, 4))(_ + _) == Pair(4, 6))
  }

  test("Pair productNames") {
    val productNames: Pair[String]  = Pair("Coffee", "Plane ticket")
    val productPrices: Pair[Double] = Pair(2.5, 329.99)
    val products = productNames.zipWith(productPrices)((name, price) => Product(name, price))

    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    assert((isEven && isPositive)(12) == true)
    assert((isEven && isPositive)(11) == false)
    assert((isEven && isPositive)(-4) == false)
    assert((isEven && isPositive)(-7) == false)
  }

  test("Predicate && PBT") {
    forAll { (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)
      def False[A]: Predicate[A] = Predicate(_ => false)
      def True[A]: Predicate[A] = Predicate(_ => true)
      assert((p1 && False)(value) == false )
      assert((p1 && True)(value) == p1(value))
    }
  }

  ignore("Predicate || PBT") {
    forAll { (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)
      def False[A]: Predicate[A] = Predicate(_ => false)
      def True[A]: Predicate[A] = Predicate(_ => true)
      assert((p1 && False)(value) == p1(value))
      assert((p1 && True)(value) == true)
    }
  }

  test("Predicate flip") {
    assert(isPositive.flip(5) == false)
    assert(isEven.flip(5) == true) 
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(Try(userIdDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder UserId round-trip") {
    forAll { number: Int =>
      val json = number.toString
      assert(userIdDecoder.decode(json) == UserId(number))
    }
  }

  test("JsonDecoder LocalDate") {
    forAll { date: LocalDate =>
      val json = date.format(DateTimeFormatter.ISO_LOCAL_DATE)
      assert(localDateDecoder.decode(s"\"$json\"") == date)
    }
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    assert(weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    assert(weirdLocalDateDecoder.decode("18347")          == LocalDate.of(2020,3,26))
    assert(Try(weirdLocalDateDecoder.decode("hello")).isFailure)
    forAll { date: LocalDate =>
      val json = date.format(DateTimeFormatter.ISO_LOCAL_DATE)
      val jsonLong = date.toEpochDay.toString
      assert(weirdLocalDateDecoder.decode(s"\"$json\"") == weirdLocalDateDecoder.decode(jsonLong))
    }
  }

}
