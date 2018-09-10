package lectures.functions

import scala.annotation.tailrec

/**
  * Цель упражнения: вычислить 9 - е число Фибоначчи
  * Для этого раскомментируйте строчку в методе fibs и исправьте ошибку компиляции.
  *
  * Данная реализация вычисления чисел фибоначчи крайне не оптимальна (имеет показатеьную сложность O(a.pow(n)) )
  * Для того, что бы в этом убедиться, Вы можете раскомментировать
  * строчку с вычислением 1000-ого числа фибоначчи
  *
  */
object Fibonacci extends App {

  // Task 2
  def fibs(num: Int): Int = num match {
    case 0 | 1 => num
    case _     => fibs(num - 1) + fibs(num - 2)
  }

  println(fibs(16))
//  println(fibs(1000))
}

/**
  * Цель упражнения: используя приемы динамического программирования,
  * реализовать более оптимальный алгоритм подсчета чисел фибоначчи
  * Для этого нужно реализовать функцию fibsImpl.
  * Сигнатуру функции Вы можете расширять по своему усмотрению,
  * но реализация должна удовлетворять следующим требованиям
  * * * * метод fibsImpl - должен быть tail recursive
  * * * * параметр acc - аккумулятор посчитанных значений
  *
  */
object Fibonacci2 extends App {

  val fibs: Stream[BigInt] = 0 #:: fibs.scanLeft(1: BigInt)(_ + _)

  def fibs2(num: Int): BigInt = {
    val acc = Array(BigInt(1), BigInt(1), BigInt(2))
    if (num <= 3) acc(num - 1)
    else fibsImpl(num, acc)(num - 1)
  }

  @tailrec
  private def fibsImpl(num: Int, acc: Array[BigInt]): Array[BigInt] =
    num match {
      case x if x == acc.length => acc
      case _                    => fibsImpl(num, acc :+ acc(acc.length - 2) + acc(acc.length - 1))
    }

  def time[T](f: => T): Unit = {
    val strt = System.currentTimeMillis()
    val res = f
    println(System.currentTimeMillis() - strt)
    println(res)
  }

  time(fibs(100))
  time(fibs2(100))

}
