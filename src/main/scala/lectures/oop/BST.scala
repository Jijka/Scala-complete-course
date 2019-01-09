package lectures.oop

/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def +(newValue: Int): BST

  def add(newValue: Int): BST

  def withLeft(newLeft: BST): BST

  def withRight(newRight: BST): BST

  def find(value: Int): Option[BST]

  def fold(agg: Int)(f: (Int, Int) => Int): Int

  def foreach[U](f: Any => U): U

  def toList: List[BSTLevel]

}

case class BSTLevel(bst: BST, level: Int) extends Ordered[BSTLevel] {
  override def compare(that: BSTLevel): Int = this.level compare that.level
}

object BST {

  private def recurse(root: BST, elems: List[Int] = List.empty[Int]): BST =
    elems match {
      case Nil          => root
      case head :: tail => recurse(root + head, tail)
    }

  def apply(elem: Int): BST = BSTImpl(elem)

  def apply(root: BST, list: List[Int]): BST = recurse(root, list)

  def apply(root: BST, elems: Int*): BST = recurse(root, elems.toList)

}

case class BSTImpl(value: Int,
                   left: Option[BST] = None,
                   right: Option[BST] = None)
    extends BST {

  def +(newValue: Int): BST = add(newValue)

  def withLeft(newLeft: BST): BST = this.copy(left = Some(newLeft))

  def withRight(newRight: BST): BST = this.copy(right = Some(newRight))

  def add(newValue: Int): BST = {
    if (newValue < value)
      withLeft(left.map(_ + newValue).getOrElse(BST(newValue)))
    else if (newValue > value)
      withRight(right.map(_ + newValue).getOrElse(BST(newValue)))
    else this
  }

  def find(value: Int): Option[BST] = this match {
    case BSTImpl(`value`, _, _)                    => Some(this)
    case BSTImpl(root, Some(l), _) if value < root => l.find(value)
    case BSTImpl(root, _, Some(r)) if value > root => r.find(value)
  }

  def fold(agg: Int)(f: (Int, Int) => Int): Int = this match {
    case BSTImpl(v, None, None)       => v
    case BSTImpl(v, Some(l), Some(r)) => f(l.fold(agg)(f), f(v, r.fold(agg)(f)))
    case BSTImpl(v, Some(l), None)    => f(v, l.fold(agg)(f))
    case BSTImpl(v, None, Some(r))    => f(v, r.fold(agg)(f))
  }

  def foreach[U](f: Any => U): U = this match {
    case BSTImpl(v, None, None) => f(v)
    case BSTImpl(v, Some(l), Some(r)) =>
      f(v)
      l.foreach(f)
      r.foreach(f)
    case BSTImpl(v, Some(l), None) =>
      f(v)
      l.foreach(f)
    case BSTImpl(v, None, Some(r)) =>
      f(v)
      r.foreach(f)
  }

  def toList: List[BSTLevel] = {
    def go(bst: BST,
           level: Int,
           acc: List[BSTLevel] = List.empty[BSTLevel]): List[BSTLevel] =
      bst match {
        case bst @ BSTImpl(_, Some(l), Some(r)) =>
          go(r, level + 1, go(l, level + 1, acc :+ BSTLevel(bst, level)))
        case bst @ BSTImpl(_, Some(l), None) =>
          go(l, level + 1, acc :+ BSTLevel(bst, level))
        case bst @ BSTImpl(_, None, Some(r)) =>
          go(r, level + 1, acc :+ BSTLevel(bst, level))
        case bst @ BSTImpl(_, None, None) =>
          acc :+ BSTLevel(bst, level)

        case _ => acc
      }

    go(this, 1)
  }

  lazy val list = this.toList

  val deep = list.max.level

  override def toString: String =
    this.value + System.lineSeparator() + this.left + this.right
  //  override def toString: String = {
  //    println(this.foreach(identity))
  //    ""
  //    def stringify(bst: BST, level: Int): Int = this match {
  //      case BSTImpl(_, Some(l), Some(r)) => Math.max(stringify(l, level + 1), stringify(r, level + 1))
  //      case BSTImpl(_, Some(l), _) => stringify(l, level + 1)
  //      case BSTImpl(_, _, Some(r)) => stringify(r, level + 1)
  //      case _                      => level
  //    }
  //
  //    stringify(this, 0)
  //  }

  //    value + "\n" + "[l=" + left.getOrElse("Empty") + "][ r=" + right.getOrElse(
  //      "Empty") + "]"

}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  //  val root: BST = BSTImpl(maxValue / 2)
  val root: BST = BST(100)

  // generator goes here
//  val generatedNodes = List(15, 190, 3, 91, 205, 171, 155, 303, 13, 17)
  val generatedNodes = List.fill(nodesCount)((Math.random() * maxValue).toInt)

  val tree: BST = BST(root, generatedNodes)

//   add marker items
  val testTree = tree + markerItem + markerItem2 + markerItem3


//   check that search is correct
    require(testTree.find(markerItem).isDefined)
    require(testTree.find(markerItem).isDefined)
    require(testTree.find(markerItem).isDefined)

    println(testTree)

}
