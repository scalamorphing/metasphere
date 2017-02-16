package expert.scalamorphing.metasphere.widget

import expert.scalamorphing.metasphere.layout.metasphere.MetaSphere
import expert.scalamorphing.metasphere.tool.Parser
import expert.scalamorphing.metasphere.tool.Parser.Node
import org.scalajs.dom.{ Event, KeyboardEvent, document }

import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._
import scalatags.Text.svgTags.ArrayNode
import scalatags.Text.{ Frag, svgAttrs }
import org.scalajs.dom.html.TextArea

case class Input(id: String) {
  implicit val delimiters = Set[Char](" \n\r\t": _*)
  import expert.scalamorphing.slowparser.api._

  val input = document.createElement("textarea")
  input.setAttribute("style", "width: 400px; height: 400px; background-color: lightyellow; float: left;")
  document.getElementById(id).appendChild(input)

  input.addEventListener("keydown", (event: KeyboardEvent) => {
    val cursor = input.asInstanceOf[TextArea].selectionStart
    val text = input.asInstanceOf[TextArea].value
    val currentLineStart = text.substring(0, cursor).reverse.dropWhile(_ != '\n').length
    val currentLineEnd = cursor + text.substring(cursor).takeWhile(_ != '\n').length
    val currentLine = text.substring(currentLineStart, currentLineEnd)
    val currentPrefix = text.substring(currentLineStart, cursor)
    val currentSuffix = text.substring(cursor, currentLineEnd)
    val prefixText = text.substring(0, currentLineStart)
    val suffixText = text.substring(currentLineEnd)
    if (event.keyCode == 9) {
      input.asInstanceOf[TextArea].value = prefixText + " " * 2 + currentLine + suffixText
      input.asInstanceOf[TextArea].setSelectionRange(cursor + 2, cursor + 2)
      event.preventDefault()
      event.stopPropagation()
    }
    if (event.keyCode == 13) {
      val indentation = currentLine.takeWhile(_ == ' ').length + {
        if (currentPrefix.endsWith("{")) {
          2
        } else if (currentPrefix.endsWith("}")) {
          -2
        } else {
          0
        }
      }

      input.asInstanceOf[TextArea].value = prefixText +
        currentPrefix +
        "\n" + " " * indentation + currentSuffix + suffixText
      input.asInstanceOf[TextArea].setSelectionRange(cursor + indentation + 1, cursor + indentation + 1)
      event.preventDefault()
      event.stopPropagation()
    }
  }, useCapture = false)

  input.addEventListener("keyup", (event: KeyboardEvent) => {
    val maybeNode: Option[Node] = Parser.parse(input.asInstanceOf[TextArea].value)

    maybeNode match {
      case Some(node) =>
        MetaSphere("main")(node)
      case None =>
        println("parsing failed")
    }
  }, false)

  val submit = document.createElement("input")
  submit.setAttribute("style", "width: 400px; height: 40px; backround-color: blue; color: red; float: left;")
  submit.setAttribute("type", "button")
  submit.setAttribute("value", "Synchronize")

  document.getElementById(id).appendChild(submit)

  submit.addEventListener("click", (event: Event) => {
    val maybeNode: Option[Node] = Parser.parse(input.asInstanceOf[TextArea].value)

    maybeNode match {
      case Some(node) =>
        MetaSphere("main")(node)
      case None =>
        println("parsing failed")
    }
  }, useCapture = true)

  val text: String =
    """alpha{ gamma {} delta {} sigma {}}""".stripMargin

  val maybeNode: Option[Node] = Parser.parse(text)

  maybeNode match {
    case Some(node) =>
      MetaSphere("main")(node)
    case None =>
      println("parsing failed")
  }
}
