package expert.scalamorphing.metasphere.layout.metasphere.sector

import expert.scalamorphing.metasphere.layout.metasphere._
import expert.scalamorphing.metasphere.layout.metasphere.path.Path

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable

case class PointPair(polar: PolarPoint, xy: CartesianPoint)

class Sector(var name: String, val number: Int,
  rawChildren: TreeMap[Int, Sector] = TreeMap.empty)(implicit
  var parent: Sector = Sector.Root,
  var implicitLevel: Level = Level(-1),
  var radiant: Radiant = Radiant.Center,
  val path: Path = Path.Root(null))
    extends Tree[Int, Sector] {

  import Sector._

  final def getId: String = {
    s"${this.level}-${this.number}" + {
      if (this.parent != null) {
        this.parent.getId
      }
    }
  }

  def anticolor(maxLevel: Int): String = {
    val weight = (this.level.index * 10 + this.number * 5) / ((maxLevel.toDouble + 1) * 20)

    s"rgb(${255 - Math.floor(200.0 * (1.0 - weight)) % 256}, ${255 - Math.floor(200.0 * weight) % 256}, ${255 - Math.floor(200.0 * weight) % 256})"
  }

  def color(maxLevel: Int): String = {
    val weight = (this.level.index * 10 + this.number * 5) / ((maxLevel.toDouble + 1) * 20)

    s"rgb(${Math.floor(200.0 * (1.0 - weight)) % 256}, ${Math.floor(200.0 * weight) % 256}, ${Math.floor(200.0 * weight) % 256})"
  }

  def points(center: CartesianPoint, radius: Double): Array[PointPair] = {
    val corners = Array(
      PolarPoint(radiant.from.radius, radiant.to.angle),
      PolarPoint(radiant.to.radius, radiant.from.angle)
    )

    Array(
      PointPair(
        PolarPoint(radiant.from.radius * radius, radiant.from.angle),
        CartesianPoint(radiant.from.xy.x * radius + center.x, radiant.from.xy.y * radius + center.y)
      ),
      PointPair(
        PolarPoint(corners(0).radius * radius, corners(0).angle),
        CartesianPoint(corners(0).xy.x * radius + center.x, corners(0).xy.y * radius + center.y)
      ),
      PointPair(
        PolarPoint(radiant.to.radius * radius, radiant.to.angle),
        CartesianPoint(radiant.to.xy.x * radius + center.x, radiant.to.xy.y * radius + center.y)
      ),
      PointPair(
        PolarPoint(corners(1).radius * radius, corners(1).angle),
        CartesianPoint(corners(1).xy.x * radius + center.x, corners(1).xy.y * radius + center.y)
      )
    )
  }

  def textPoints(center: CartesianPoint, radius: Double, levelCount: Int): Array[PointPair] = {
    val relativeLevel = this.level.index.toDouble / levelCount.toDouble
    val toRadius = relativeLevel
    val fromRadius = relativeLevel + 1.0 / levelCount.toDouble
    val dradius = (fromRadius - toRadius) / 4.0
    val dangle = (radiant.from.angle - radiant.to.angle) / 8.0
    val corners = Array(
      PolarPoint(fromRadius, radiant.from.angle - dangle),
      PolarPoint(fromRadius, radiant.to.angle),
      PolarPoint(toRadius + dradius, radiant.to.angle),
      PolarPoint(toRadius + dradius, radiant.from.angle - dangle)
    ).reverse

    Array(
      PointPair(
        PolarPoint(corners(0).radius * radius, corners(0).angle),
        CartesianPoint(corners(0).xy.x * radius + center.x, corners(0).xy.y * radius + center.y)
      ),
      PointPair(
        PolarPoint(corners(1).radius * radius, corners(1).angle),
        CartesianPoint(corners(1).xy.x * radius + center.x, corners(1).xy.y * radius + center.y)
      ),
      PointPair(
        PolarPoint(corners(2).radius * radius, corners(2).angle),
        CartesianPoint(corners(2).xy.x * radius + center.x, corners(2).xy.y * radius + center.y)
      ),
      PointPair(
        PolarPoint(corners(3).radius * radius, corners(3).angle),
        CartesianPoint(corners(3).xy.x * radius + center.x, corners(3).xy.y * radius + center.y)
      )
    )
  }

  @tailrec
  final def getRoot: Sector = {
    if (level.index <= 0) {
      this
    } else {
      this.parent.getRoot
    }
  }

  def info(indent: Int = 1): String = {
    s"""
       |Sector(
       |${"  " * indent}[$number, $level, parent: ${if (parent != null) parent.level else null}]
       |${this.children.map { case (_, child) => child.info(indent + 1) } mkString " "})
     """.stripMargin
  }

  override val node: Int = number
  override var children: TreeMap[Int, Sector] = rawChildren
  var level: Level = implicitLevel

  def updatePath(path: Path): Sector = {
    new Sector(this.name, this.number, this.children)(this.parent, this.level, this.radiant)
  }

  def updateLevel(parent: Sector, level: Level): Sector = {
    new Sector(this.name, this.number, {
      for {
        (number, child) <- this.children
      } yield {
        number -> child.updateLevel(this, Level(level.index + 1))
      }
    })(parent, level, this.radiant)
  }

  def link(parent: Sector, newLevel: Level): Sector = {
    this.parent = parent
    this.level = newLevel
    this
  }

  def insert(newChild: Sector): Sector = {
    val childLevel = Level(this.level.index + 1)
    this.children = this.children.updated(newChild.number, newChild.link(this, childLevel))
    this
  }

  def sort: SortedSector = {
    import Sector.ordering

    val queue = mutable.PriorityQueue[Sector](this)(ordering)

    val sortedSector: SortedSector = {
      for {
        current <- queue if queue.nonEmpty
      } yield {
        queue.enqueue(current.children.values.toSeq: _*)

        current
      }
    } toArray

    sortedSector
  }

  def flatten: Array[Sector] = {
    {
      this.children.flatMap(
        {
          case (_, child) =>
            child.flatten
        }
      ).toList ::: List(this)
    }.toArray
  }
}

object Sector {

  type SortedSector = Array[Sector]

  implicit val ordering = new Ordering[Sector] {
    override def compare(x: Sector, y: Sector): Int = {
      val compared = Array[Boolean](
        x.level.index > y.level.index,
        y.level.index > x.level.index,
        x.number > y.number,
        y.number > x.number
      ).map(_.compare(false))

      val xIndexLess = 1 - compared(0)
      val yIndexLess = 1 - compared(1)
      val xNumberLess = 1 - compared(2)
      val yNumberLess = 1 - compared(3)

      if (xIndexLess == yIndexLess) {
        if (xNumberLess == yNumberLess) {
          0
        } else {
          if (xNumberLess == 1) {
            1
          } else {
            -1
          }
        }
      } else {
        if (xIndexLess == 1) {
          1
        } else {
          -1
        }
      }

    }
  }

  def apply(name: String, number: Int)(implicit
    parent: Sector = Sector.Root,
    level: Level = Level(-1),
    radiant: Radiant = Radiant.Center): Sector = {
    new Sector(name, number)(parent, level, radiant)
  }

  val Root: Sector = Sector("", 0)(null, Level(0), Radiant.Center)

  def rings(allSectors: Array[Sector]): Array[Ring] = {
    val rings = Array[Ring]()
    val maxLevel = allSectors.map(_.level.index).max
    (0 to maxLevel).map {
      level =>
        Ring(
          level,
          allSectors.filter {
            sector => sector.level.index == level
          }
        )
    } toArray
  }

  def sortByRing(sector: Array[Sector]): Array[Sector] = {

    val queue = mutable.PriorityQueue[Sector](sector: _*)(ordering)

    val sorted: SortedSector = {
      for {
        current <- queue if queue.nonEmpty
      } yield {
        current
      }
    } toArray

    sorted
  }
}