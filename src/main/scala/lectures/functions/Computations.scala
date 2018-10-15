package lectures.functions

/**
  *
  * В объекте 'Computation' в методе computation сравниваются 2 массива.
  * Результатом сравнения будет массив, содержащий слова, принадлежащие обоим массивам
  * В данном случа результатом будет массив, содержащий 2 элемента Array("Клара", "Карла")
  *
  * С помощью Thread.sleep имитируется прододжительное вычисление
  */
trait Data {
  val filterData: String =
    "Клара у Карла украла корралы, Карл у Клары украл кларнет"
  val dataArray: Array[String] = "Клара Цеткин обожала Карла Маркса".split(" ")
}

object Computation extends Data {

  def computation(filterData: String,
                  dataProducer: Array[String]): Array[String] = {
    //PRODUCE WORDS ARRAY FROM A STRING
    val filterArray = filterData.split(" ")

    // LEAVE ONLY EQUAL WORDS IN BOTH ARRAYS
    dataProducer.filter(dataItem => filterArray.contains(dataItem))
  }

  computation(filterData, dataArray).foreach(println)

}

/**
  * Допишите curriedComputation, так, что бы после вызова partiallyAppliedCurriedFunction
  * результат был бы тем же, что и в предыдущем случае
  *
  * Раскомментируйте последнюю строчку
  *
  * Какой тип имеет partiallyAppliedCurriedFunction - ?
  */
object CurriedComputation extends Data {

  def curriedComputation(filterData: String)(
      dataProducer: Array[String]): Array[String] = {

    //PRODUCE WORDS ARRAY FROM A STRING
    val filterArray = filterData.split(" ")

    // LEAVE ONLY EQUAL WORDS IN BOTH ARRAYS
    dataProducer.filter(dataItem => filterArray.contains(dataItem))
  }

  val partiallyAppliedCurriedFunction: Array[String] => Array[String] =
    curriedComputation(filterData)

  partiallyAppliedCurriedFunction(dataArray).foreach(println)
}

/**
  * Допишите реализации методов так, что бы результат совпадал с предыдущими.
  *
  * При этом постарайтесь минимизировать количество разбиений строки filterData на отдельные слова.
  */
object FunctionalComputation extends Data {

  def functionalComputation(
      filterArray: Array[String]): Array[String] => Array[String] =
    data => data.filter(filterArray.contains(_))

  val filterApplied: Array[String] => Array[String] = functionalComputation(
    filterData.split(" "))

  filterApplied(dataArray).foreach(println)
}
