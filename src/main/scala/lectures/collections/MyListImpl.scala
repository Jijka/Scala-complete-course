package lectures.collections

import scala.annotation.tailrec

/**
  * Представим, что по какой-то причине Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода:
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, чтобы выполнить задание:
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  case class MyList(data: List[Int]) {

    def flatMap(f: Int => MyList) = MyList(data.flatMap(inp => f(inp).data))

    def map(f: Int => Int): MyList = flatMap(x => MyList(List(f(x))))

    def filter(p: Int => Boolean): MyList = flatMap { x =>
      if (p(x)) MyList(List(x)) else MyList(List.empty[Int])
    }

    def foldLeft1[A](acc: A)(op: (A, A) => A): A = data match {
      case Nil          => acc
      case head :: tail => MyList(tail).foldLeft(op(acc, head))(op)
    }

    def foldLeft[A](zero: A)(op: (A, A) => A): A = {
      @tailrec
      def go(acc: A, myList: List[Int]): A = myList match {
        case Nil          => acc
        case head :: tail => go(op(acc, head), tail)
      }

      go(zero, data)
    }
  }

  require(
    MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(
    MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0)(_ + _) == 21)
  require(MyList(Nil).foldLeft(0)(_ + _) == 0)

}
