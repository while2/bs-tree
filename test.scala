import math._
import rb_tree._
import util._

object test extends App {
  override def main(args: Array[String]) = {
    var root: Tree = Empty
    for (t <- 1 to 1000) {
      if (Random.nextInt(3) == 0) {
        root = root.insert(Random.nextInt(100))
      } else {
        root = root.delete(Random.nextInt(100))
      }
      root.check_height
      root.display()
    }
  }
}