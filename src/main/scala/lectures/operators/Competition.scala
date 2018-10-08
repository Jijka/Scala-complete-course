package lectures.operators

/**
  * Проходит чемпионат по спортивному киданю костей)
  * Сражаются "Наши" и "Приезжие"
  *
  * Каждый член команды бросил кубик и должен сравнить свой результат с каждым результатом из команды соперника
  *
  * Итог сравнений должн быть записан в ассоциативный массив в таком виде
  * val results: Array[(String, Int)] = (("Artem vs John" -> 3), ("Artem vs James" -> 5), ... )
  * При этом числовое значение должно быть получено как разность между результатами первого и второго игроков
  *
  * Когда составлен массив results, надо подсчитать, чья взяла.
  * Если результат встречи >0, то finalResult увеличивается на единицу
  * Если <0, уменьшается
  *
  * В итоге надо
  * исправить ошибки компиляции
  * напечатать:
  * * "Наша взяла", если наших побед больше, т.е. finalResult > 0
  * * "Продули", если победили приезжие
  * * "Победила дружба" в случае ничьи
  *
  * Для решения задачи раскомментируйте тело объекта Competition
  * В целях упрощения можно поменять тип исходных данных
  */
object Competition extends App {

  val locals = Map("Artem" -> 6,
                   "Sergey" -> 5,
                   "Anton" -> 2,
                   "Vladimir" -> "2",
                   "Alexander" -> 4D)
  val foreigners = Map[String, Int]("John" -> 3,
                                    "James" -> 1,
                                    "Tom" -> 2,
                                    "Dick" -> 5,
                                    "Eric" -> 6)

  val results: Map[String, Int] = for (l <- locals;
                                       f <- foreigners) yield {
    val localName = l._1
    val localValue: Int = l._2 match {
      case s: String => s.toInt
      case d: Double => d.toInt
      case i: Int    => i
      case _         => throw new Exception("Недопустимый входящий параметр")
    }
    val foreignName = f._1
    val foreignValue = f._2
    (s"$localName vs $foreignName", localValue - foreignValue)
  }

  val finalResult = for { (_, r) <- results } yield {
    if (r > 0) 1
    else if (r == 0) 0
    else -1
  }

  if (finalResult.sum > 0) println("Наша взяла")
  else if (finalResult.sum < 0) println("Продули")
  else println("Победила дружба")
}
