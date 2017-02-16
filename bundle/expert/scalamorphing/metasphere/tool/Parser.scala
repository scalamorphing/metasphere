package expert.scalamorphing.metasphere.tool

object Parser {
  implicit val delimiters = Set[Char](" \n\r\t" :_*)
  import expert.scalamorphing.slowparser.api._

  val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ"
  lazy val string: P[String] = P(
    CharsWhile(chars.indexOf(_) >= 0).!
  ).map(_.toString)

  case class Node(name: String, children: Array[Node])

  lazy val node: P[Node] = P(string ~ "{" ~ children ~ "}").map {
    case (name, nodeChildren) => Node(name, nodeChildren.toArray)
  }

  lazy val children: P[List[Node]] = P(node.~/.rep).map {
    nodes => nodes.toList
  }

  lazy val root: P[Node] = P(Start ~ node ~ End)

  def parse(unsafeNode: String): Option[Node] = {
    root.parse(unsafeNode) match {
      case Parsed.Success(result, _) => {
        Some(result)
      }
      case error: Parsed.Failure => {
        println(error)
        None
      }
    }
  }
}