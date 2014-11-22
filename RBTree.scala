package rb_tree

import Color._

object Color {
  type Color = Int
  val R = 0
  val B = 1
  val B2 = 2
}

trait Tree {
  val color: Color

  def ins(tar: Int): Tree
  def insert(tar: Int) = ins(tar) match {
    case NonEmpty(l, k, c, r) => NonEmpty(l, k, B, r)
    case _ => Empty
  }

  def del(tar: Int): Tree
  def delete(tar: Int) = del(tar) match {
    case NonEmpty(l, k, c, r) => NonEmpty(l, k, B, r)
    case _ => Empty
  }

  def increase(): Tree = this match {
    case NonEmpty(left, key, color, right) => NonEmpty(left, key, color + B, right)
    case _ => Empty2
  }
  def decrease(): Tree = this match {
    case NonEmpty(left, key, color, right) => NonEmpty(left, key, color - B, right)
    case _ => Empty
  }

  def fix_del(): Tree
  def rotate_left(): Tree
  def rotate_right(): Tree
  def search(tar: Int): Boolean
  def display(level: Int = 0)
  def check_height(): Int
}

object Empty extends Tree {
  val color = B

  def ins(tar: Int) = NonEmpty(Empty, tar, R, Empty)
  def del(tar: Int) = Empty

  def fix_del() = Empty
  def rotate_left() = Empty
  def rotate_right() = Empty
  def search(tar: Int) = false
  def display(level: Int): Unit = {}
  def check_height() = 0
}

object Empty2 extends Tree {
  val color = B2

  def ins(tar: Int): Tree = throw new RuntimeException("Non implemented")
  def del(tar: Int): Tree = throw new RuntimeException("Non implemented")
  def fix_del() = throw new RuntimeException("Non implemented")
  def rotate_left() = throw new RuntimeException("Non implemented")
  def rotate_right() = throw new RuntimeException("Non implemented")
  def search(tar: Int): Boolean = throw new RuntimeException("Non implemented")
  def display(level: Int) = throw new RuntimeException("Non implemented")
  def check_height(): Int = throw new RuntimeException("Non implemented")
}

case class NonEmpty(left: Tree, key: Int, color: Color, right: Tree) extends Tree {
  def ins(tar: Int): Tree = {
    if (tar < key) NonEmpty(left.ins(tar), key, color, right).fix_ins
    else if (tar > key) NonEmpty(left, key, color, right.ins(tar)).fix_ins
    else this
  }

  def fix_ins() = this match {
    case NonEmpty(NonEmpty(NonEmpty(a, x, R, b), y, R, c), z, B, d) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
    case NonEmpty(a, x, B, NonEmpty(b, y, R, NonEmpty(c, z, R, d))) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
    case NonEmpty(NonEmpty(a, x, R, NonEmpty(b, y, R, c)), z, B, d) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
    case NonEmpty(a, x, B, NonEmpty(NonEmpty(b, y, R, c), z, R, d)) => NonEmpty(NonEmpty(a, x, B, b), y, R, NonEmpty(c, z, B, d))
    case _ => this
  }

  def del(tar: Int) = {
    if (tar < key) NonEmpty(left.del(tar), key, color, right).fix_del
    else if (tar > key) NonEmpty(left, key, color, right.del(tar)).fix_del
    else if (left == Empty) {
      if (color == B) right.increase
      else right
    } else right match {
      case NonEmpty(rl, rk, rc, rr) => NonEmpty(left, rk, color, right.del(rk)).fix_del
      case _ => {
        if (color == B) left.increase
        else left
      }
    }
  }

  def rotate_left() = right match {
    case NonEmpty(rl, rk, rc, rr) => NonEmpty(NonEmpty(left, key, rc, rl), rk, color, rr)
  }

  def rotate_right() = left match {
    case NonEmpty(ll, lk, lc, lr) => NonEmpty(ll, lk, color, NonEmpty(lr, key, lc, right))
  }

  def fix_del(): Tree = {
    if (left.color == B2) right match {
      case NonEmpty(rl, rk, B, rr) => {
        if (rr.color == R) NonEmpty(NonEmpty(left.decrease, key, B, rl), rk, color, rr.increase)
        else if (rl.color == R) NonEmpty(left, key, color, right.rotate_right).fix_del
        else NonEmpty(left.decrease, key, color + B, right.decrease)
      }
      case Empty => increase
      case _ => rotate_left match {
        case NonEmpty(l, k, c, r) => NonEmpty(l.fix_del, k, c, r)
      }
    }
    else if (right.color == B2) left match {
      case NonEmpty(ll, lk, B, lr) => {
        if (ll.color == R) NonEmpty(ll.increase, lk, color, NonEmpty(lr, key, B, right.decrease))
        else if (lr.color == R) NonEmpty(left.rotate_left, key, color, right).fix_del
        else NonEmpty(left.decrease, key, color + B, right.decrease).fix_del
      }
      case Empty => increase
      case _ => rotate_right match {
        case NonEmpty(l, k, c, r) => NonEmpty(l, k, c, r.fix_del)
      }
    }
    else this
  }

  def search(tar: Int) = {
    if (tar < key) left.search(tar)
    else if (tar > key) right.search(tar)
    else true
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