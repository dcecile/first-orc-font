package firstOrc

final case class Glyph(
  char: Char,
  bits: Seq[Seq[Boolean]],
  flexs: Seq[Int]
) {
  lazy val tallBits: Seq[Seq[Boolean]] =
    bits.zip(flexs)
      .flatMap({ case (bitRow, flex) =>
        Seq.fill(flex)(bitRow)
      })
}

object Glyph {
  val size = 6

  def build(char: Char)(
    blank0: String,
    row0: String, flex0: Int,
    row1: String, flex1: Int,
    row2: String, flex2: Int,
    row3: String, flex3: Int,
    row4: String, flex4: Int,
    row5: String, flex5: Int,
    blank1: String
  ): Glyph = {
    val rows = Seq(row0, row1, row2, row3, row4, row5)
    val flexs = Seq(flex0, flex1, flex2, flex3, flex4, flex5)
    if (flexs.sum != size * 2) {
      throw new IllegalArgumentException(s"Wrong flex sum: $char ${flexs.sum}")
    }
    for (row <- rows) {
      row match {
        case parser(bits) => ()
        case _ => throw new IllegalArgumentException(s"Wrong row format: $char $row")
      }
    }
    val bits = rows
      .map(_.substring(2, 2 + size * 2))
      .map(row => (0 until (size * 2) by 2)
        .map(row(_))
        .map(_ match {
          case '[' => true
          case ' ' => false
        }))
    Glyph(char, bits, flexs)
  }

  val parser = """^  (  |\[\]){6}  $""".r
}
