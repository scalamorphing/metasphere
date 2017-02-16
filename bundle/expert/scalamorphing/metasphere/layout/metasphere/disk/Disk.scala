package expert.scalamorphing.metasphere.layout.metasphere.disk

import expert.scalamorphing.metasphere.layout.metasphere.{ Level, PolarPoint, Radiant }
import expert.scalamorphing.metasphere.layout.metasphere.sector.Sector
import expert.scalamorphing.metasphere.tool.Parser.Node

import scala.collection.immutable.TreeMap

case class Disk(center: Sector) {
  val sectors = this.center.flatten
  val paths = this.center.sort
  val rings = Sector.rings(this.sectors)

  println(s"RINGS: ${rings.length}")

  val radiants = {
    for {
      chain <- this.paths
    } yield {
      val level = chain.level
      var childrenCount = 1

      def parentRadiant: Radiant = if (chain.parent != null) {
        assert(chain.parent.level.index == level.index - 1)

        childrenCount = chain.parent.children.size

        chain.parent.radiant
      } else {
        Radiant.Center
      }

      var delta = {
        parentRadiant.to.angle - parentRadiant.from.angle
      }

      println(s"chain.level: ${chain.level.index}")
      println(s"chain.index: ${chain.number}")
      println(s"delta: ${Math.floor(delta * 180.0 / Math.PI)}")
      println(s"shift: ${parentRadiant.from.angle * 180.0 / Math.PI}")

      def angle(number: Int): Double = {
        var result = parentRadiant.from.angle + delta * number.toDouble / childrenCount.toDouble
        result
      }

      def radius(n: Int) = n.toDouble / rings.length.toDouble

      val localRadiant = Radiant(
        from = PolarPoint(
          radius = radius(chain.level.index),
          angle = angle(chain.number)
        ),
        to = PolarPoint(
          radius = radius(chain.level.index + 1),
          angle = angle(chain.number + 1)
        )
      )

      println(s"From angle: ${localRadiant.from.angle}")
      println(s"To angle: ${localRadiant.to.angle}")

      println(localRadiant)

      chain.radiant = localRadiant

      if (childrenCount == 1) {
        List(localRadiant)
      } else {
        if (delta > Math.PI) {
          List(localRadiant)
        } else {
          List(localRadiant)
        }
      }
    }
  } flatten
}

object Disk {
  def apply(node: Node): Disk = {
    new Disk(
      makeIndex(null, node, 0, 0)
    )
  }

  def makeIndex(parent: Sector, node: Node, level: Int = 0, shift: Int = 0): Sector = {
    var sector = Sector(node.name, shift)(parent, Level(level))
    var childShift = 0
    val childrenIndices = for {
      child <- node.children
    } yield {
      childShift += 1
      makeIndex(null, child, level + 1, childShift - 1)
    }

    childrenIndices.foreach {
      child =>
        {
          sector.insert(child)
        }
    }
    sector
  }
}