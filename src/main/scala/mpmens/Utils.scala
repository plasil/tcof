package mpmens

object Utils {
  /** Internal method used in pretty-printing solving results */
  private[mpmens] def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n") + (if (str.endsWith("\n")) "\n" else "")

  private var randomNameIdx = 0

  private[mpmens] def randomName = {
    val name = f"<$randomNameIdx%06d>"
    randomNameIdx = randomNameIdx + 1
    name
  }
}
