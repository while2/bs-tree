package rb_tree

import Color._

object Color {
  type Color = Int
  val R = 0
  val B = 1
  val DB = 2
}

trait Tree {
  def ins(tar: Int): Tree
  def insert(tar: Int) = {
    ins(tar) match {
      case NonEmpty(l, k, c, r) => NonEmpty(l, k, B, r)
      case _ => this
    }
  }

  def display(level: Int = 0)
  def check_height(): Int
}

object Empty extends Tree {
  def ins(tar: Int) = NonEmpty(Empty, tar, R, Empty)

  def display(level: Int) = {}
  def check_height() = 0;
}

case class NonEmpty(left: Tree, key: Int, color: Color, right: Tree) extends Tree {
  def ins(tar: Int): Tree = {
    if (tar < key) NonEmpty(left.ins(tar), key, color, right).fix_ins
    else if (tar > key) NonEmpty(left, key, color, right.ins(tar)).fix_ins
    else this
  }

  def fix_ins() = {
    this match {
      case NonEmpty(NonEmpty(NonEmpty(a, x, R, b), y, R, c), z, B, d) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
      case NonEmpty(a, x, B, NonEmpty(b, y, R, NonEmpty(c, z, R, d))) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
      case NonEmpty(NonEmpty(a, x, R, NonEmpty(b, y, R, c)), z, B, d) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
      case NonEmpty(a, x, B, NonEmpty(NonEmpty(b, y, R, c), z, R, d)) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
      case _ => this
    }
  }

  def display(level: Int) = {
    left.display(level + 1)
    print("\t" * level + key + '(' + color + ')' + '\n')
    right.display(level + 1)
  }

  def check_height = {
    val a = left.check_height
    val b = right.check_height
    if (a != b) throw new RuntimeException("Unbalanced")
    a + color
  }
}










