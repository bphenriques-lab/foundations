package exercises.dataprocessing

object ForLoopExercises {

  def sum(numbers: List[Int]): Int = foldLeft(numbers, 0) { (r, n) => r + n }

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  def size[A](items: List[A]): Int = foldLeft(items, 0) { (r, _) => r + 1 }

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  def min(numbers: List[Int]): Option[Int] = foldLeft(numbers, Option.empty[Int]) { (r, n) =>
    r match {
      case Some(currentMin) if n < currentMin => Some(n)
      case None => Some(n)
      case _ => r
    }
  }

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  def wordCount(words: List[String]): Map[String, Int] = foldLeft(words, Map.empty[String, Int]) { (result, word) =>
    val frequency = result.getOrElse(word, 0)
    result.updated(word, frequency + 1)
  }

  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  def foldLeft[T, R](list: List[T], initialResult: R)(combine: (R, T) => R): R = {
    var state = initialResult
    for (element <- list) state = combine(state, element)
    state
  }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    foldLeft(elements, List.empty[To]) { (result, el) => result :+ update(el) }

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    foldLeft(elements, List.empty[A]) { (result, el) => el :: result }

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    foldLeft(elements, Option.empty[A]) { (_, el) => Some(el) }

  // g. Can you generalise `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A](elements: List[A])(implicit orderWith: Ordering[A]): Option[A] =
    foldLeft(elements, Option.empty[A]) {
      case (None, current) => Some(current)
      case (Some(currentMin), current) => Some(orderWith.min(currentMin, current))
    }

}
