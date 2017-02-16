package expert.scalamorphing.metasphere.layout.metasphere

import javafx.scene.shape.SVGPath

import scala.collection.immutable.TreeMap
import MetaSphere._
import expert.scalamorphing.metasphere.layout.metasphere.disk.Disk
import expert.scalamorphing.metasphere.layout.metasphere.sector.Sector
import expert.scalamorphing.metasphere.tool.Parser.Node

import scala.collection.mutable
import scala.languageFeature.postfixOps
import monix.execution.Scheduler.Implicits.global
import monix.reactive._
import org.scalajs.dom.raw.SVGPathElement

import scalatags.Text.all._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags
import scalatags.Text.svgTags._
import scalatags.Text.svgTags.ArrayNode
import scalatags.Text.{ Frag, svgAttrs }
import org.scalajs.dom.{ Event, MouseEvent, document }
import org.w3c.dom.html.HTMLDOMImplementation

case class Level(index: Int)

case class Point(x: Double, y: Double)

case class Ring(level: Int, sectors: Array[Sector]) {
  def color(maxLevel: Int): String = {
    val weight = (level.toDouble + 1) / (maxLevel.toDouble + 1)

    s"rgb(${Math.floor(200.0 * (1.0 - weight))}, ${Math.floor(200.0 * weight)}, ${Math.floor(200.0 * weight)})"
  }
}

case class CartesianPoint(x: Double, y: Double) {
  override def toString: String = s"$x $y"
}

case class PolarPoint(radius: Double, angle: Double) {
  def xy: CartesianPoint = CartesianPoint(radius * Math.cos(angle), radius * Math.sin(angle))
}

object PolarPoint {
  val Center = PolarPoint(
    radius = 0,
    angle = 0
  )
}

case class Radiant(from: PolarPoint, to: PolarPoint)

object Radiant {
  def Center = Radiant(
    from = PolarPoint.Center,
    to = PolarPoint(
      radius = PolarPoint.Center.radius,
      angle = Math.PI * 2.0
    )
  )
}

case class MetaSphere(id: String)(node: Node) {
  document.getElementById(id).innerHTML = {
    svg(
      svgAttrs.viewBox := "0 0 100 100",
      svgAttrs.width := "800",
      svgAttrs.height := "800",
      svgAttrs.id := "svg"
    )(
        {
          val disk = Disk(node)
          val rings = disk.rings.reverse
          val radiants = disk.radiants.reverse
          val sectors = disk.sectors
          ArrayNode(
            /*rings.map { ring => {
            println(ring.level)
            circle(
              svgAttrs.r := (45.0 * (ring.level + 1.0) / rings.length).toString,
              svgAttrs.cx := 50.0.toString,
              svgAttrs.cy := 50.0.toString,
              svgAttrs.fill := ring.color(rings.length),
              svgAttrs.stroke := "rgb(0, 0, 0)",
              svgAttrs.strokeWidth := "1"
            )
            }
          } ++ */ sectors.map {
              sector =>
                {
                  val points = sector.textPoints(CartesianPoint(50.0, 50.0), 45.0, rings.length)
                  val sectorString = Array(
                    s"M ${points(0).xy.toString}",
                    s"A ${points(0).polar.radius} ${points(0).polar.radius} 0 0 1 ${points(1).xy.toString}",
                    "z"
                  ).mkString(" ")

                  svgTags.defs(
                    svgTags.path(
                      svgAttrs.id := s"sector-${sector.getId}",
                      svgAttrs.d := sectorString,
                      svgAttrs.stroke := sector.anticolor(rings.length),
                      svgAttrs.strokeWidth := "0.4",
                      svgAttrs.fill := sector.color(rings.length)
                    )()
                  )
                }
            } ++ sectors.flatMap {
              sector =>
                {
                  val points = sector.points(CartesianPoint(50.0, 50.0), 45.0)
                  val sectorString = Array(
                    s"M ${points(0).xy.toString}",
                    s"A ${points(0).polar.radius} ${points(0).polar.radius} 0 0 1 ${points(1).xy.toString}",
                    s"L ${points(1).xy.toString} ${points(2).xy.toString}",
                    s"A ${points(2).polar.radius} ${points(2).polar.radius} 0 0 0 ${points(3).xy.toString}",
                    s"L ${points(3).xy.toString} ${points(0).xy.toString}",
                    "z"
                  ).mkString(" ")
                  Seq(svgTags.path(
                    svgAttrs.d := sectorString,
                    svgAttrs.stroke := sector.anticolor(rings.length),
                    svgAttrs.strokeWidth := "0.4",
                    svgAttrs.fill := sector.color(rings.length),
                    svgAttrs.id := s"area-${sector.getId}",
                    svgAttrs.`class` := "sector-area"
                  ))
                }
            } ++ sectors.map {
              sector =>
                {
                  val points = sector.points(CartesianPoint(50.0, 50.0), 45.0)
                  svgTags.text(
                    svgAttrs.fontSize := s"${(points(2).polar.radius - points(0).polar.radius) / 3.0}",
                    svgAttrs.fontFamily := "sans-serif",
                    svgAttrs.fill := sector.anticolor(rings.length),
                    svgAttrs.offset := "5%"
                  )(
                      svgTags.textPath(
                        svgAttrs.xLinkHref := s"#sector-${sector.getId}"
                      )(
                          sector.name
                        )
                    )
                }
            } /*++ radiants.map {
            radiant => {
              line(
                svgAttrs.x1 := (50.0 + 45 * radiant.from.radius * Math.cos(radiant.from.angle)).toString,
                svgAttrs.y1 := (50.0 + 45 * radiant.from.radius * Math.sin(radiant.from.angle)).toString,
                svgAttrs.x2 := (50.0 + 45 * radiant.to.radius * Math.cos(radiant.from.angle)).toString,
                svgAttrs.y2 := (50.0 + 45 * radiant.to.radius * Math.sin(radiant.from.angle)).toString,
                svgAttrs.stroke := "rgb(0, 0, 0)",
                svgAttrs.strokeWidth := "0.5"
              )
            }
          }*/
          )
        }
      ).render
  }
  val sectorAreas = document.getElementsByClassName("sector-area")
  val length = sectorAreas.length
  var i = 0
  while (i < length) {
    sectorAreas(i).addEventListener("click", (event: MouseEvent) => {
      println(s"Clicked ${event.target.asInstanceOf[SVGPathElement].id}")
    }, true)
    i += 1
  }
}

object MetaSphere {
  type Radius = Double
  type Angle = Double
}
