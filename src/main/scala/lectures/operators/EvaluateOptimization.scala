package lectures.operators

import java.util.concurrent.TimeUnit

import lectures.functions.{
  Computation,
  CurriedComputation,
  Data,
  FunctionalComputation
}

/**
  * В задачке из lectures.functions.Computations мы реализовали
  * один и тот же метод 3-мя разными способами
  *
  * Пришло время оценить, насколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого
  * * в классах CurriedComputation и FunctionalComputation уберите extends App, оставьте extends Data
  * * раскомментируйте код, выполните в циклах вызов 3-х имплементаций,
  * * оцените разницу во времени выполнения и объясните ее происхожение
  *
  */
object EvaluateOptimization extends App with Data {

  case class Times(count: Int)

  def time[T](timeFunction: () => Long)(f: => T)(
      implicit c: Times = Times(1)): Long = {
    val computationStartTimestamp = timeFunction()
    for (_ <- 1 to c.count) { f }
    val endTime = timeFunction()
    val elapsedTime = endTime - computationStartTimestamp
    println(
      s"Elapsed time in computation(): $elapsedTime ms $computationStartTimestamp $endTime")
    elapsedTime
  }

  implicit val times: Times = Times(10)

  def hundredTimesMillis[T](f: => T): Long =
    time(System.currentTimeMillis) { f }

  def hundredTimesNanos[T](f: => T): Long =
    time(System.nanoTime) { f }

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 Computation.computation
  val elapsed = hundredTimesMillis {
    Computation.computation(filterData, dataArray)
  }

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction
  val partiallyAppliedElapsed = hundredTimesMillis {
    CurriedComputation.partiallyAppliedCurriedFunction(dataArray)
  }

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 FunctionalComputation.filterApplied
  val appliedElapsed = hundredTimesMillis {
    FunctionalComputation.filterApplied(dataArray)
  }

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

  println(
    s"Difference is about ${partiallyAppliedElapsed - appliedElapsed} milliseconds")

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 Computation.computation
  val elapsedNano = hundredTimesNanos {
    Computation.computation(filterData, dataArray)
  }

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction
  val partiallyAppliedElapsedNano = hundredTimesNanos {
    CurriedComputation.partiallyAppliedCurriedFunction(dataArray)
  }

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 FunctionalComputation.filterApplied
  val appliedElapsedNano = hundredTimesNanos {
    FunctionalComputation.filterApplied(dataArray)
  }

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

  println(
    s"Difference is about ${(partiallyAppliedElapsedNano - appliedElapsedNano)/1e-6} milliseconds")
}
