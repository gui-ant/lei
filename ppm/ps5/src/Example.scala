sealed trait MyTree[+A]

case object Empty extends MyTree[Nothing]

case class Node[A](value: A, left: MyTree[A], right: MyTree[A]) extends MyTree[A]

case class Example[A](myField: MyTree[A])

object Example {

  /*
  a) Complete the function maximum that returns the maximum element in a Tree[Int].
  */
  def maximum(t: MyTree[Int]): Int = t match {
    case Empty => 0
    case Node(v, l, r) => v.max(maximum(l).max(maximum(r)))
  }

  /*
  b) Complete the function depth that returns the maximum path length from the root of a tree to any leaf.
  */
  def depth[A](t: MyTree[A]): Int = t match {
    case Empty => 0
    case Node(v, l, r) => (1 + depth(l)).max(1 + depth(r))
  }

  def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case Empty => Empty
    case Node(v, l, r) => Node(f(v), map(l)(f), map(r)(f))
  }

  def main(args: Array[String]): Unit = {
    val tree1 = Node(42, Node(8, Node(5, Empty, Node(10, Empty, Node(1, Empty, Empty))), Empty), Empty)
    val t = Example(tree1)
    println(s"Maximum element of the tree: ${Example.maximum(tree1)}")
    println(s"Maximum depth of the tree: ${Example.depth(tree1)}")
    println(s"map: ${Example.map(tree1)(x => x * x)}")
  }
}
