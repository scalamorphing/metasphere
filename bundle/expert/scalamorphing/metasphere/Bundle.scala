package expert.scalamorphing.metasphere

import expert.scalamorphing.metasphere.widget.Editor

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object Bundle extends JSApp {
  @JSExport
  def main(): Unit = {
    Editor.render
  }
  main()
}
