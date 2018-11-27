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

  def prefix_traverse: Int

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
    case _                                         => None
  }

  def prefix_traverse: Int = this match {
    case BSTImpl(`value`, _, _) => `value`
    case BSTImpl(_, Some(l), _) => l.prefix_traverse
  }

  override def toString: String =
    value + "\n" + "[l=" + left.getOrElse("Empty") + "][ r=" + right.getOrElse(
      "Empty") + "]"
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)

  // generator goes here
  val generatedNodes = List.fill(nodesCount)((Math.random() * maxValue).toInt)

  val tree: BST = BST(root, generatedNodes)

  // add marker items
  val testTree = tree + markerItem + markerItem2 + markerItem3

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(testTree)

}
