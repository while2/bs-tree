package avl_tree

import math._

trait Tree {
  val height: Int

  def insert(tar: Int): Tree
  def delete(tar: Int): Tree

  def rotate_left(): Tree
  def rotate_right(): Tree

  def display(level: Int = 0)
  def check_height(): Int
}

object Empty extends Tree {
  val height = 0

  def insert(tar: Int) = NonEmpty(Empty, tar, Empty)
  def delete(tar: Int) = Empty

  def rotate_left() = Empty
  def rotate_right() = Empty

  def display(level: Int) = {}
  def check_height() = 0
}

case class NonEmpty(left: Tree, key: Int, right: Tree) extends Tree {
  val height = max(left.height, right.height) + 1

  def insert(tar: Int) = {
    if (tar < key)
      NonEmpty(left.insert(tar), key, right).balance
    else if (tar > key)
      NonEmpty(left, key, right.insert(tar)).balance
    else
      this
  }

  def delete(tar: Int) = {
    if (tar < key)
      NonEmpty(left.delete(tar), key, right).balance
    else if (tar > key)
      NonEmpty(left, key, right.delete(tar)).balance
    else {
      right match {
        case NonEmpty(rl, rk, rr) => NonEmpty(left, rk, right.delete(rk)).balance
        case _ => left
      }
    }
  }

  def rotate_left() = {
    right match {
      case NonEmpty(rl, rk, rr) => NonEmpty(NonEmpty(left, key, rl), rk, rr)
    }
  }

  def rotate_right() = {
    left match {
      case NonEmpty(ll, lk, lr) => NonEmpty(ll, lk, NonEmpty(lr, key, right))
    }
  }

  def balance() = {
    if (left.height > right.height + 1) {
      left match {
        case NonEmpty(ll, lk, lr) => {
          if (ll.height >= lr.height) rotate_right
          else NonEmpty(left.rotate_left, key, right).rotate_right
        }
      }
    } else if (left.height + 1 < right.height) {
      right match {
        case NonEmpty(rl, rk, rr) => {
          if (rl.height <= rr.height) rotate_left
          else NonEmpty(left, key, right.rotate_right).rotate_left
        }
      }
    } else
      this
  }

  def display(level: Int) = {
    left.display(level + 1)
    print("\t" * level + key + '(' + height + ')' + '\n')
    right.display(level + 1)
  }

  def check_height(): Int = {
    val lh = left.check_height
    val rh = right.check_height
    if (abs(lh - rh) > 1) throw new RuntimeException("Unbalanced")
    max(lh, rh) + 1
  }
}
