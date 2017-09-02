package firstOrc

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.Pixel
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Paths
import scalaj.http.Http
import scalaj.http.HttpResponse

object Main {
  def main(args: Array[String]): Unit = {
    val pixels = buildFontPixels()
    val requestBytes = packImage(pixels)
    val response = sendRequest(requestBytes)
    handleResponse(response)
  }

  val paddedSize = Glyph.size + 2

  def buildFontPixels(): Seq[Pixel] =
    buildInfoPixels() ++ buildGlyphsPixels()

  def buildInfoPixels(): Seq[Pixel] = {
    val infoString = infoJson.noSpaces
    val initialPixels = convertString(infoString)
    padWithBlank(
      initialPixels,
      calculateInfoSize(initialPixels))
  }

  val infoJson: Json = {
    implicit def stringToJson: String => Json = _.asJson
    implicit def intToJson: Int => Json = _.asJson
    implicit def booleanToJson: Boolean => Json = _.asJson
    Json.obj(
      ("f", "First Orc Short"),
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
    .map(_ match {
      case 0 => Pixel(0, 0, 0, 255)
      case value => Pixel(value, 255, 255, 255)
    })
    .toVector

  def calculateInfoSize(initialPixels: Seq[Pixel]): Int = {
    val rows = (initialPixels.length + paddedSize - 1) / paddedSize
    rows * paddedSize
  }

  def padWithBlank(pixels: Seq[Pixel], fullLength: Int): Seq[Pixel] = {
    val padding = fullLength - pixels.length
    pixels ++ Seq.fill(padding)(blankPixel)
  }

  val blankPixel = convertValue(255)

  def buildGlyphsPixels(): Seq[Pixel] =
    (Font.glyphs :+ Font.unknownGlyph)
      .filter(_.char != ' ')
      .map(buildGlyphPixels)
      .flatten

  def buildGlyphPixels(glyph: Glyph): Seq[Pixel] = {
    val left = buildStringGlyphColumn(glyph.char.toString)
    val right = buildBlankGlyphColumn()
    val top = buildBlankGlyphRow()
    val bottom = top
    val data = buildDataGlyphRows(glyph.bits)
    val centerColumns = (top +: data :+ bottom).transpose
    (left +: centerColumns :+ right).transpose.flatten
  }

  def buildStringGlyphColumn(string: String): Seq[Pixel] = {
    val initialPixels = convertString(string)
    padWithBlank(initialPixels, paddedSize)
  }

  def buildBlankGlyphColumn(): Seq[Pixel] =
    Seq.fill(paddedSize)(blankPixel)

  def buildBlankGlyphRow(): Seq[Pixel] =
    Seq.fill(Glyph.size)(blankPixel)

  def buildDataGlyphRows(bits: Seq[Seq[Boolean]]): Seq[Seq[Pixel]] =
    bits.map(_.map(_ match {
      case false => blankPixel
      case true => filledPixel
    }))

  val filledPixel = convertValue(0)

  def packImage(pixels: Seq[Pixel]): Array[Byte] = {
    val height = pixels.length / paddedSize
    val image = Image(paddedSize, height, pixels.toArray)
    image.bytes
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
