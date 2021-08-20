package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("wordCount all counts are > 0") {
    forAll { (words: List[String]) =>
      assert(wordCount(words).values.forall(_ > 0))
    }
  }

  test("wordCount - size(filter)") {
    forAll { (words: List[String]) =>
      wordCount(words).foreach { case (word, count) =>
        val filtered = words.filter(_ == word)
        assert(count == size(filtered))
      }
    }
  }

  test("pattern is consistent with the foldLeft in the standard library") {
    forAll { (numbers: List[Int], default: Int, combine: (Int, Int) => Int) =>
      assert(foldLeft(numbers, default)(combine) == numbers.foldLeft(default)(combine))
    }
  }

  test("pattern noop") {
    forAll { (numbers: List[Int]) =>
      assert(foldLeft(numbers, List.empty[Int])(_ :+ _) == numbers)
    }
  }

  test("map consistent with List map") {
    forAll { (numbers: List[Int], update: Int => Int) =>
      assert(map(numbers)(update) == numbers.map(update))
    }
  }

  test("reverse consistent with List reverse") {
    forAll { (numbers: List[Int]) =>
      assert(reverse(numbers) == numbers.reverse)
    }
  }

  test("lastOption consistent with List lastOption") {
    forAll { (numbers: List[Int]) =>
      assert(lastOption(numbers) == numbers.lastOption)
    }
  }

  test("generalMin consistent with List minOption") {
    forAll { (numbers: List[Int]) =>
      assert(generalMin(numbers)(implicitly) == numbers.minOption)
    }
  }
}
