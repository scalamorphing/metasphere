package expert.scalamorphing.metasphere.layout.metasphere.disk

import expert.scalamorphing.metasphere.layout.metasphere.Tree

import scala.collection.immutable.TreeMap

case class TextTree(override val node: String) extends Tree[String, TextTree] {
  override var children: TreeMap[Int, TextTree] = TreeMap.empty
}

object TextTree {

}