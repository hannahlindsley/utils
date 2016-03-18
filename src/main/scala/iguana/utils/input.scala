
package iguana.utils.input

import java.io._
import java.net.URI
import iguana.utils._
import iguana.utils.Unicode._

import org.apache.commons.io.IOUtils

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing.MurmurHash3


class Input(private val characters: Array[Int],
            val tabWidth: Int = 8,
            val uri: URI = URI.create("inmemory:///")) {

  import MurmurHash3._

  private lazy val hash = finalizeHash(mixLast(mix(arrayHash(characters), tabWidth), uri.hashCode()), 3)

  private val lineColumns = calcLineColumns

  def apply(i: Int): Int = characters(i)

  def length: Int = characters.length

  def lineNumber(i: Int): Int =
    if (i < 0 || i >= lineColumns.length) 0 else lineColumns(i).lineNumber

  def columnNumber(i: Int): Int =
    if (i < 0 || i >= lineColumns.length) 0 else lineColumns(i).columnNumber

  def getPositionInfo(leftExtent: Int, rightExtent: Int) {
    PositionInfo(leftExtent,
      rightExtent - leftExtent,
      lineNumber(leftExtent),
      columnNumber(leftExtent),
      lineNumber(rightExtent),
      columnNumber(rightExtent))
  }

  /**
    * Inserts the contents of the given input instance to this input
    * at the given input position, and returns a new input instance.
    *
    * @param i     the input position in this input instance at which the insertion should occur
    * @param input the input whose content should be inserted
    * @return      a new input instance containing both inputs
    */
  def insert(i: Int, input: Input): Input = {
    if (i < 0) throw new IllegalArgumentException("i cannot be negative.")
    if (i > length) throw new IllegalArgumentException("i cannot be greater than " + length)

    val newChars = Array.ofDim[Int](length + input.length - 1)

    System.arraycopy(characters, 0, newChars, 0, i)
    System.arraycopy(input.characters, 0, newChars, i, input.length - 1)
    System.arraycopy(characters, i, newChars, i + input.length - 1, length - i - 1)

    newChars(newChars.length - 1) = -1 // EOF
    new Input(newChars)
  }

  private def calcLineColumns: Array[LineColumn] = {
    @tailrec
    def loop(i: Int, lineNumber: Int, columnNumber: Int, buffer: mutable.Buffer[LineColumn]): Unit = {
      if (characters(i) == EOF) return

      buffer += LineColumn(lineNumber, columnNumber)

      characters(i) match {
        case '\n' => loop(i + 1, lineNumber + 1, 1, buffer)
        case '\r' => loop(i + 1, lineNumber, 1, buffer)
        case '\t' => loop(i + 1, lineNumber, columnNumber + tabWidth, buffer)
        case _    => loop(i + 1, lineNumber, columnNumber + 1, buffer)
      }
    }

    val buffer = ArrayBuffer.empty[LineColumn]
    loop(0, 1, 1, buffer)
    buffer.toArray
  }

  override def equals(o: Any): Boolean = o match {
    case i:Input => i.hash == hash &&
                    i.characters.sameElements(characters) &&
                    i.tabWidth == tabWidth &&
                    i.uri == uri
    case _ => false
  }

  override def hashCode(): Int = hash

  override def toString = "[" + characters.map(charName(_)).mkString(",") + "]"
}

object Input {

  lazy val empty: Input = new Input(Array(EOF))

  def apply(c: Char): Input = new Input(Array(c, EOF))

  def apply(a: Array[Int]): Input = new Input(a)

  def apply(s: String): Input = new Input(streamToIntArray(IOUtils.toInputStream(s)))

  def apply(f: File): Input =
    new Input(streamToIntArray(new FileInputStream(f)), uri = f.toURI())

  private def streamToIntArray(in: InputStream): Array[Int] = {

    import org.apache.commons.io.input.BOMInputStream

    val bomIn = new BOMInputStream(in, false)
    val reader = new BufferedReader(new InputStreamReader(bomIn))

    @tailrec
    def loop(in: Reader, buffer: ArrayBuffer[Int]): Unit = {
      val curr = in.read()
      if (curr == -1) return
      else if (!Character.isHighSurrogate(curr.asInstanceOf[Char])) {
        buffer += curr
      } else {
        val next = in.read()
        if (next != -1) buffer += Character.toCodePoint(curr.asInstanceOf[Char], next.asInstanceOf[Char])
      }
      loop(in, buffer)
    }

    val buffer = ArrayBuffer[Int]()
    loop(reader, buffer)
    buffer += EOF
    reader.close()

    buffer.toArray
  }

}

case class PositionInfo(offset: Int,
                        length: Int,
                        startLineNumber: Int,
                        startColumnNumber: Int,
                        endLineNumber: Int,
                        endColumnNumber: Int)

case class LineColumn(lineNumber: Int, columnNumber: Int)