package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val n = data.length / 2
    if (n == 0) data
    else {
      def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] =
        (left, right) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xTail, y :: yTail) =>
            if (x < y) Seq(x) ++ merge(xTail, right)
            else Seq(y) ++ merge(left, yTail)
        }

      val (left, right) = data splitAt n
      merge(mergeSort(left), mergeSort(right))
    }
  }

  val list = List(32, 51, 26, 73, 123, 63, 73, 90, 1, 2, 57, 2, 79, 34, 82, 53,
    16, 18, 19, 27, 12)

  println(mergeSort(list).mkString(" "))
}
