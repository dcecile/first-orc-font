package firstOrc

object Font {
  val glyphs: Seq[Glyph] = Seq(
    Glyph.build('A')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "                "),
    Glyph.build('a')(
      "                ",
      "                ", 4,
      "    [][][][][]  ", 1,
      "          [][]  ", 2,
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 3,
      "    [][][][][]  ", 1,
      "                "),
    Glyph.build('B')(
      "                ",
      "  [][][][][]    ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][]    ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('b')(
      "                ",
      "  [][]          ", 1,
      "  [][]          ", 4,
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('C')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 3,
      "  [][]          ", 1,
      "  [][]          ", 3,
      "  [][]    [][]  ", 3,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('c')(
      "                ",
      "                ", 4,
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 2,
      "  [][]          ", 2,
      "  [][]    [][]  ", 2,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('D')(
      "                ",
      "  [][][][][]    ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 7,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('d')(
      "                ",
      "          [][]  ", 1,
      "          [][]  ", 4,
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('E')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]          ", 4,
      "  [][][][]      ", 1,
      "  [][][][]      ", 1,
      "  [][]          ", 4,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('e')(
      "                ",
      "                ", 4,
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 3,
      "  [][][][][][]  ", 1,
      "  [][]          ", 2,
      "    [][][][][]  ", 1,
      "                "),
    Glyph.build('F')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]          ", 4,
      "  [][][][]      ", 1,
      "  [][][][]      ", 1,
      "  [][]          ", 1,
      "  [][]          ", 4,
      "                "),
    Glyph.build('f')(
      "                ",
      "    [][][][][]  ", 1,
      "    [][]    []  ", 3,
      "  [][][][]      ", 2,
      "    [][]        ", 1,
      "    [][]        ", 4,
      "  [][][][]      ", 1,
      "                "),
    Glyph.build('G')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]          ", 5,
      "  [][]  [][][]  ", 1,
      "  [][]    [][]  ", 3,
      "  [][][][][][]  ", 1,
      "          [][]  ", 1,
      "                "),
    Glyph.build('g')(
      "                ",
      "                ", 4,
      "    [][][][][]  ", 1,
      "  [][]  [][]    ", 4,
      "  [][][][][]    ", 1,
      "        [][]    ", 1,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('H')(
      "                ",
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "                "),
    Glyph.build('h')(
      "                ",
      "  [][]          ", 1,
      "  [][]          ", 4,
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "                "),
    Glyph.build('I')(
      "                ",
      "  [][][][][][]  ", 1,
      "      [][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 7,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('i')(
      "                ",
      "      [][]      ", 3,
      "                ", 1,
      "    [][][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 5,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('J')(
      "                ",
      "    [][][][][]  ", 1,
      "        [][]    ", 1,
      "        [][]    ", 1,
      "        [][]    ", 6,
      "  [][]  [][]    ", 2,
      "  [][][][]      ", 1,
      "                "),
    Glyph.build('j')(
      "                ",
      "          [][]  ", 3,
      "                ", 1,
      "        [][][]  ", 1,
      "          [][]  ", 4,
      "  [][]    [][]  ", 2,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('K')(
      "                ",
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 3,
      "  [][]  [][]    ", 2,
      "  [][][][]      ", 2,
      "  [][][][][]    ", 1,
      "  [][]    [][]  ", 3,
      "                "),
    Glyph.build('k')(
      "                ",
      "  [][]          ", 3,
      "  [][]    [][]  ", 2,
      "  [][]  [][][]  ", 2,
      "  [][][][]      ", 2,
      "  [][][][][]    ", 1,
      "  [][]    [][]  ", 2,
      "                "),
    Glyph.build('L')(
      "                ",
      "  [][]          ", 1,
      "  [][]          ", 1,
      "  [][]          ", 1,
      "  [][]          ", 7,
      "  [][]    [][]  ", 1,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('l')(
      "                ",
      "    [][][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 7,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('M')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][][][][][]  ", 1,
      "  []  []  [][]  ", 1,
      "  []  []  [][]  ", 4,
      "  []      [][]  ", 1,
      "  []      [][]  ", 4,
      "                "),
    Glyph.build('m')(
      "                ",
      "                ", 4,
      "    [][][][][]  ", 1,
      "  [][][][][][]  ", 1,
      "  []  []  [][]  ", 1,
      "  []  []  [][]  ", 3,
      "  []      [][]  ", 2,
      "                "),
    Glyph.build('N')(
      "                ",
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 3,
      "  [][][]  [][]  ", 2,
      "  [][][][][][]  ", 2,
      "  [][]  [][][]  ", 2,
      "  [][]    [][]  ", 2,
      "                "),
    Glyph.build('n')(
      "                ",
      "                ", 4,
      "  [][][][][]    ", 1,
      "    [][]  [][]  ", 1,
      "    [][]  [][]  ", 1,
      "    [][]  [][]  ", 4,
      "  [][][]  [][]  ", 1,
      "                "),
    Glyph.build('O')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 7,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('o')(
      "                ",
      "                ", 4,
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('P')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 5,
      "  [][][][][]    ", 1,
      "  [][]          ", 1,
      "  [][]          ", 3,
      "                "),
    Glyph.build('p')(
      "                ",
      "                ", 4,
      "  [][][][][][]  ", 1,
      "    [][]  [][]  ", 1,
      "    [][]  [][]  ", 3,
      "    [][][][]    ", 1,
      "    [][]        ", 2,
      "                "),
    Glyph.build('Q')(
      "                ",
      "    [][][][]    ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 7,
      "    [][][][]    ", 1,
      "          [][]  ", 1,
      "                "),
    Glyph.build('q')(
      "                ",
      "                ", 4,
      "    [][][][]    ", 1,
      "  [][]  [][]    ", 1,
      "  [][]  [][]    ", 3,
      "  [][][][][]    ", 1,
      "        [][][]  ", 2,
      "                "),
    Glyph.build('R')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][]    ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "                "),
    Glyph.build('r')(
      "                ",
      "                ", 4,
      "  [][]  [][][]  ", 1,
      "    [][][][][]  ", 1,
      "    [][]  [][]  ", 1,
      "    [][]        ", 1,
      "    [][]        ", 4,
      "                "),
    Glyph.build('S')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]          ", 4,
      "  [][][][][][]  ", 1,
      "  [][][][][][]  ", 1,
      "          [][]  ", 4,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('s')(
      "                ",
      "                ", 4,
      "  [][][][][][]  ", 1,
      "  [][]          ", 3,
      "    [][][][][]  ", 1,
      "          [][]  ", 2,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('T')(
      "                ",
      "  [][][][][][]  ", 1,
      "  []  [][]  []  ", 2,
      "      [][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 1,
      "      [][]      ", 6,
      "                "),
    Glyph.build('t')(
      "                ",
      "      [][]      ", 1,
      "      [][]      ", 4,
      "  [][][][][][]  ", 1,
      "      [][]      ", 1,
      "      [][]      ", 4,
      "      [][][][]  ", 1,
      "                "),
    Glyph.build('U')(
      "                ",
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 7,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('u')(
      "                ",
      "                ", 4,
      "  [][]  [][]    ", 1,
      "  [][]  [][]    ", 1,
      "  [][]  [][]    ", 1,
      "  [][]  [][]    ", 4,
      "    [][][][][]  ", 1,
      "                "),
    Glyph.build('V')(
      "                ",
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 5,
      "    []    []    ", 3,
      "    [][][][]    ", 1,
      "      [][]      ", 1,
      "                "),
    Glyph.build('v')(
      "                ",
      "                ", 4,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 3,
      "    [][][][]    ", 2,
      "      [][]      ", 1,
      "                "),
    Glyph.build('W')(
      "                ",
      "  []      [][]  ", 1,
      "  []      [][]  ", 5,
      "  []  []  [][]  ", 1,
      "  []  []  [][]  ", 3,
      "  [][][][][][]  ", 1,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('w')(
      "                ",
      "                ", 4,
      "  []      [][]  ", 1,
      "  []      [][]  ", 3,
      "  []  []  [][]  ", 2,
      "  [][][][][][]  ", 1,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('X')(
      "                ",
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 3,
      "    [][][][]    ", 2,
      "      [][]      ", 2,
      "    [][][][]    ", 2,
      "  [][]    [][]  ", 2,
      "                "),
    Glyph.build('x')(
      "                ",
      "                ", 4,
      "  [][]    [][]  ", 2,
      "  [][][][][][]  ", 1,
      "      [][]      ", 1,
      "    [][][][]    ", 2,
      "  [][]    [][]  ", 2,
      "                "),
    Glyph.build('Y')(
      "                ",
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "    [][][][]    ", 1,
      "      [][]      ", 1,
      "      [][]      ", 4,
      "    [][][]      ", 1,
      "                "),
    Glyph.build('y')(
      "                ",
      "                ", 4,
      "    [][]  [][]  ", 1,
      "    [][]  [][]  ", 4,
      "      [][][]    ", 1,
      "  []    [][]    ", 1,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('Z')(
      "                ",
      "  [][][][][][]  ", 1,
      "  []    [][][]  ", 2,
      "      [][][]    ", 4,
      "    [][][]      ", 2,
      "  [][][]    []  ", 2,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('z')(
      "                ",
      "                ", 4,
      "  [][][][][][]  ", 1,
      "  []    [][][]  ", 2,
      "      [][]      ", 2,
      "  [][][]    []  ", 2,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('0')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 2,
      "  [][]  []  []  ", 3,
      "  []  []  [][]  ", 3,
      "  [][]    [][]  ", 2,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('1')(
      "                ",
      "        [][]    ", 2,
      "    [][][][]    ", 2,
      "        [][]    ", 1,
      "        [][]    ", 1,
      "        [][]    ", 5,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('2')(
      "                ",
      "    [][][][][]  ", 1,
      "    []    [][]  ", 2,
      "          [][]  ", 4,
      "      [][][]    ", 2,
      "    [][]        ", 2,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('3')(
      "                ",
      "    [][][][][]  ", 1,
      "    []    [][]  ", 1,
      "          [][]  ", 4,
      "      [][][]    ", 1,
      "          [][]  ", 4,
      "  [][][][][][]  ", 1,
      "                "),
    Glyph.build('4')(
      "                ",
      "      [][][]    ", 1,
      "    []  [][]    ", 4,
      "  []    [][]    ", 2,
      "  [][][][][][]  ", 1,
      "        [][]    ", 3,
      "      [][][][]  ", 1,
      "                "),
    Glyph.build('5')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]          ", 1,
      "  [][]          ", 4,
      "  [][][][][][]  ", 1,
      "          [][]  ", 4,
      "  [][][][][]    ", 1,
      "                "),
    Glyph.build('6')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 2,
      "  [][]          ", 3,
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 4,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('7')(
      "                ",
      "  [][][][][][]  ", 1,
      "  [][]    [][]  ", 2,
      "          [][]  ", 3,
      "        [][]    ", 1,
      "        [][]    ", 3,
      "      [][]      ", 2,
      "                "),
    Glyph.build('8')(
      "                ",
      "    [][][][]    ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "    [][][][]    ", 1,
      "  [][]    [][]  ", 4,
      "    [][][][]    ", 1,
      "                "),
    Glyph.build('9')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 1,
      "  [][]    [][]  ", 4,
      "  [][][][][][]  ", 1,
      "          [][]  ", 3,
      "        [][]    ", 2,
      "                "),
    Glyph.build('-')(
      "                ",
      "                ", 1,
      "                ", 4,
      "    [][][][]    ", 1,
      "    [][][][]    ", 1,
      "                ", 1,
      "                ", 4,
      "                "),
    Glyph.build('/')(
      "                ",
      "          [][]  ", 2,
      "        [][]    ", 2,
      "      [][]      ", 1,
      "      [][]      ", 3,
      "    [][]        ", 2,
      "  [][]          ", 2,
      "                "),
    Glyph.build('?')(
      "                ",
      "    [][][][][]  ", 1,
      "  [][]    [][]  ", 3,
      "          [][]  ", 2,
      "      [][]      ", 2,
      "                ", 2,
      "      [][]      ", 2,
      "                "),
    Glyph.build(' ')(
      "                ",
      "                ", 1,
      "                ", 1,
      "                ", 1,
      "                ", 1,
      "                ", 1,
      "                ", 7,
      "                "))

  val unknownGlyph: Glyph =
    Glyph.build('\uFFFD')(
      "                ",
      "  []  []  []    ", 2,
      "    []  []  []  ", 2,
      "  []  []  []    ", 2,
      "    []  []  []  ", 2,
      "  []  []  []    ", 2,
      "    []  []  []  ", 2,
      "                ")
}
