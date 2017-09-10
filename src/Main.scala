package firstOrc

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.Pixel
import com.sksamuel.scrimage.nio.PngWriter
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import org.zeroturnaround.zip.ByteSource
import org.zeroturnaround.zip.FileSource
import org.zeroturnaround.zip.ZipEntrySource
import org.zeroturnaround.zip.ZipUtil
import scalaj.http.Http
import scalaj.http.HttpResponse

object Main {
  def main(args: Array[String]): Unit = {
    val files = prepareFiles()
    val output = s"${Config.basePath}.zip"
    ZipUtil.pack(files.toArray, new File(output))
    println(s"Zipped to '${output}'")
  }

  def prepareFiles(): Seq[ZipEntrySource] =
    Seq(
      buildFont(Config(
        variant = "Short",
        bits = _.bits,
        heightChange = _ * 1)),
      buildFont(Config(
        variant = "Tall",
        bits = _.tallBits,
        heightChange = _ * 2)),
      Seq(
        new FileSource(
          Config.getPathForFile("README.txt"),
          new File("USAGE.md")),
        new FileSource(
          Config.getPathForFile("LICENSE.txt"),
          new File("LICENSE.md")))).flatten

  final case class Config(
    variant: String,
    bits: Glyph => Seq[Seq[Boolean]],
    heightChange: Int => Int
  ) {
    val familyName = s"${Config.baseName} ${variant}"
    val paddedHeight = heightChange(Glyph.size) + 2
    val postscriptFontName =
      s"${familyName.replace(" ", "")}-${Config.styleName}"
    def getPathForExtension(extension: String): String =
      Config.getPathForFile(s"${postscriptFontName}.${extension}")
  }

  object Config {
    val baseName = "First Orc"
    val styleName = "Regular"
    val majorVersion = 0
    val minorVersion = 2
    val basePath =
      f"${baseName} v${majorVersion}.${minorVersion}%03d".replace(" ", "_")
    def getPathForFile(filename: String): String =
      s"${basePath}/${filename}"
  }

  def buildFont(implicit config: Config): Seq[ByteSource] = {
    println(s"Building '${config.variant}' font")
    val pixels = buildFontPixels()
    val imageBytes = packImage(pixels)
    Seq(
      new ByteSource(config.getPathForExtension("png"), imageBytes),
      compileFont(imageBytes, "ttf"))
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
      ("f", config.familyName),
      ("s", Config.styleName),
      ("w", 400),
      ("d", "Dan Cecile"),
      ("du", "https://dcecile.github.io/first-orc-font"),
      ("c", "2017"),
      ("mj", Config.majorVersion),
      ("mn", Config.minorVersion),
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

  def compileFont(
    imageBytes: Array[Byte],
    extension: String
  )(
    implicit config: Config
  ): ByteSource = {
    val response = sendRequest(imageBytes, extension)
    val responseBytes = handleResponse(response)
    println(s"Got ${extension.toUpperCase} font")
    new ByteSource(
      config.getPathForExtension(extension),
      responseBytes)
  }

  def sendRequest(
    requestBytes: Array[Byte],
    extension: String
  ): HttpResponse[Array[Byte]] = {
    val uri = s"${bitFontMakeHost}/compile-to-${extension}"
    println(s"Sending request to ${uri}")
    Http(uri)
      .timeout(connTimeoutMs = 1000, readTimeoutMs = 20000)
      .postData(requestBytes)
      .asBytes
  }

  val bitFontMakeHost = "https://bitfontmake.herokuapp.com"

  def handleResponse(
    response: HttpResponse[Array[Byte]]
  ): Array[Byte] = {
    require(response.isSuccess, s"Successful request ${response}")
    response.body
  }
}
