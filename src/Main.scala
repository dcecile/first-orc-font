package firstOrc

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.Pixel
import com.sksamuel.scrimage.nio.PngWriter
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Paths
import scalaj.http.Http
import scalaj.http.HttpResponse

object Main {
  def main(args: Array[String]): Unit = {
    buildFont(Config(
      name = "Short",
      bits = _.bits,
      heightChange = _ * 1))
    buildFont(Config(
      name = "Tall",
      bits = _.tallBits,
      heightChange = _ * 2))
  }

  final case class Config(
    name: String,
    bits: Glyph => Seq[Seq[Boolean]],
    heightChange: Int => Int
  ) {
    def paddedHeight = heightChange(Glyph.size) + 2
  }

  def buildFont(implicit config: Config) {
    val pixels = buildFontPixels()
    val requestBytes = packImage(pixels)
    val response = sendRequest(requestBytes)
    handleResponse(response)
  }

  val paddedWidth = Glyph.size + 2

  def buildFontPixels()(implicit config: Config): Seq[Pixel] =
    buildInfoPixels() ++ buildGlyphsPixels()

  def buildInfoPixels()(implicit config: Config): Seq[Pixel] = {
    val infoString = buildInfoJson().noSpaces
    val initialPixels = convertString(infoString)
    padWithBlank(
      initialPixels,
      calculateInfoSize(initialPixels))
  }

  def buildInfoJson()(implicit config: Config): Json = {
    implicit def stringToJson: String => Json = _.asJson
    implicit def intToJson: Int => Json = _.asJson
    implicit def booleanToJson: Boolean => Json = _.asJson
    Json.obj(
      ("f", s"First Orc ${config.name}"),
      ("s", "Regular"),
      ("w", 400),
      ("d", "Dan Cecile"),
      ("du", "https://dcecile.github.io/first-orc-font"),
      ("c", "2017"),
      ("mj", 0),
      ("mn", 1),
      ("o", true))
  }

  def convertString(string: String): Seq[Pixel] =
    string.getBytes(UTF_8)
      .map(convertValue(_))

  def convertValue(value: Byte): Pixel =
    convertValue(value.toInt & 0xFF)

  val convertValue: Vector[Pixel] = (0 to 255)
    .map({
      case 0 => Pixel(0, 0, 0, 255)
      case value => Pixel(value, 255, 255, 255)
    })
    .toVector

  def calculateInfoSize(initialPixels: Seq[Pixel]): Int = {
    val rows = (initialPixels.length + paddedWidth - 1) / paddedWidth
    rows * paddedWidth
  }

  def padWithBlank(pixels: Seq[Pixel], fullLength: Int): Seq[Pixel] = {
    val padding = fullLength - pixels.length
    pixels ++ Seq.fill(padding)(blankPixel)
  }

  val blankPixel = convertValue(255)

  def buildGlyphsPixels()(implicit config: Config): Seq[Pixel] =
    (Font.glyphs :+ Font.unknownGlyph)
      .filter(_.char != ' ')
      .map(buildGlyphPixels)
      .flatten

  def buildGlyphPixels(glyph: Glyph)(implicit config: Config): Seq[Pixel] = {
    val left = buildStringGlyphColumn(glyph.char.toString)
    val right = buildBlankGlyphColumn()
    val top = buildBlankGlyphRow()
    val bottom = top
    val data = buildDataGlyphRows(config.bits(glyph))
    val centerColumns = (top +: data :+ bottom).transpose
    (left +: centerColumns :+ right).transpose.flatten
  }

  def buildStringGlyphColumn(string: String)(implicit config: Config): Seq[Pixel] = {
    val initialPixels = convertString(string)
    padWithBlank(initialPixels, config.paddedHeight)
  }

  def buildBlankGlyphColumn()(implicit config: Config): Seq[Pixel] =
    Seq.fill(config.paddedHeight)(blankPixel)

  def buildBlankGlyphRow(): Seq[Pixel] =
    Seq.fill(Glyph.size)(blankPixel)

  def buildDataGlyphRows(bits: Seq[Seq[Boolean]]): Seq[Seq[Pixel]] =
    bits.map(_.map({
      case false => blankPixel
      case true => filledPixel
    }))

  val filledPixel = convertValue(0)

  def packImage(pixels: Seq[Pixel]): Array[Byte] = {
    val height = pixels.length / paddedWidth
    val image = Image(paddedWidth, height, pixels.toArray)
    image.bytes(PngWriter.MaxCompression)
  }

  def sendRequest(requestBytes: Array[Byte]): HttpResponse[Array[Byte]] =
    Http("http://127.0.0.1:5000/compile-to-otf")
      .postData(requestBytes)
      .asBytes

  def handleResponse(response: HttpResponse[Array[Byte]]): Unit = {
    require(response.isSuccess, s"Successful request ${response}")
    val contentDispositionOption = response.header("Content-Disposition")
    require(contentDispositionOption.nonEmpty, s"Content-Disposition header ${response}")
    val filename = contentDispositionOption.get.replaceFirst("^.*=", "")
    val bytes = response.body
    writeBytes(filename, bytes)
    println(s"Written ${bytes.length} bytes to ${filename}")
  }

  def writeBytes(filename: String, bytes: Array[Byte]): Unit = {
    val path = Paths.get(filename)
    Files.write(path, bytes)
  }
}
