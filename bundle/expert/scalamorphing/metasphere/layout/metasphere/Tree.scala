package expert.scalamorphing.metasphere.layout.metasphere

import scala.collection.immutable.TreeMap

trait Tree[Value, Node <: Tree[Value, _]] {
  val node: Value
  var children: TreeMap[Int, Node]
}
