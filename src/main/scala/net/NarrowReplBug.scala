package net

import shapeless.syntax.singleton._

object NarrowReplBug {

  def main(args: Array[String]): Unit = {

    println(1.narrow)

  }

}
