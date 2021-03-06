package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.ExecutionContext.Implicits.global

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0),
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature consistent") {
    forAll { (samples: ParList[Sample]) =>
      assert(minSampleByTemperature(samples) == samples.partitions.flatten.minByOption(_.temperatureFahrenheit))
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0),
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature pbt") {
    forAll { (samples: ParList[Sample]) =>
      assert(averageTemperature(samples) == averageTemperature(ParList(samples.partitions ++ samples.partitions)))
    }
  }

  test("monoFoldLeft") {
    forAll { (list: ParList[Int]) =>
      assert(list.monoFoldLeft(Monoid.sumInt) == list.toList.sum)
    }
  }

  test("monoFoldLeft is foldLeft") {
    forAll { list: ParList[Int] =>
      assert(list.monoFoldLeft(Monoid.sumInt) == list.toList.foldLeft(Monoid.sumInt.default)(Monoid.sumInt.combine))
    }
  }

  test("foldMap is consistent") {
    forAll { (list: ParList[Int]) =>
      val monoid = Monoid.sumInt
      assert(list.monoFoldLeft(monoid) == list.foldMap(identity)(monoid))
    }
  }

  test("parFoldMap is consistent") {
    forAll { (list: ParList[Int]) =>
      val monoid = Monoid.sumInt
      assert(list.foldMap(identity)(monoid) == list.parFoldMap(identity)(monoid))
    }
  }

  def checkMonoid[A: Arbitrary](name: String, param: Monoid[A], gen: Gen[A]): Unit = {
    test(s"$name no-op") {
      forAll(gen) { value: A =>
        assert(param.combine(param.default, value) == value)
        assert(param.combine(value, param.default) == value)
      }
    }
    test(s"$name assoc") {
      forAll(gen, gen, gen) { (value1: A, value2: A, value3: A) =>
        assert(param.combine(param.combine(value1, value2), value3) == param.combine(value1, param.combine(value2, value3)))
      }
    }
  }

  val genInt = Gen.choose(Int.MinValue, Int.MaxValue)
  val genDouble = Gen.choose(Float.MinValue, Float.MaxValue).map(_.toDouble)

  checkMonoid("sum int", Monoid.sumInt, genInt)
  checkMonoid("sum double", Monoid.sumDouble, genDouble)
  checkMonoid("zip", Monoid.zip(Monoid.sumDouble, Monoid.sumInt), Gen.zip(genDouble, genInt))
  checkMonoid("min sample", Monoid.minSample, Gen.option(sampleGen))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 100)
  implicit val doubleArb: Arbitrary[Double] = Arbitrary(Gen.choose(-100.0f, 100.0f).map(_.toDouble))


  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples),
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
