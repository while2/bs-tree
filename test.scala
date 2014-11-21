import math._
import avl_tree._

object test extends App {
  override def main(args: Array[String]) = {
    var root: Tree = Empty
    for (nums <- (1 to 10).permutations) {
      root = Empty
      for (key <- nums) {
        root = root.insert(key)
        root.check_height
      }
    }
    root.display()
    println(root.search(1), root.search(100))
  }
}