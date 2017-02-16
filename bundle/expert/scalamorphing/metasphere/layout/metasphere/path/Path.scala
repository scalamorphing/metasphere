package expert.scalamorphing.metasphere.layout.metasphere.path

import expert.scalamorphing.metasphere.layout.metasphere.sector.Sector

class Path(val parent: Path, val chains: Array[Sector] = Array(), val children: Array[Path] = Array()) {
  def +(that: Path): Path = {
    Path(this.parent, (this.chains.toList ++ that.chains.toList).toArray, that.children)
  }

  def append(child: Path): Path = {
    Path(this.parent, this.chains, (this.children.toList ::: List(child)).toArray)
  }
}

object Path {
  def apply(parent: Path, chains: Array[Sector], children: Array[Path]) =
    new Path(parent, chains, children)

  case class Root(root: Sector) extends Path(null, Array(root))
}
