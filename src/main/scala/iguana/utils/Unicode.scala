
package iguana.utils

import java.lang.Character._

object Unicode {

  val MAX_UTF32_VAL = 0x10FFFF

  def getCodePoints(category: String): Option[Set[Int]] =
    categoriesMap.get(category)

  def charName(c: Int): String =
    if (c == EOF)
      "EOF"
    else if (isPrintableAscii(c))
      c.toChar + ""
    else {
      val s = "\\u" + "%04X".format(c)
      if ((s == "\\u000D") || (s == "\\u000A")) "\\" + s else s
    }

  def isPrintableAscii(codePoint: Int): Boolean =
    '\u0020' < codePoint && codePoint < '\u007f'

  private val categoryNames: Map[Byte, String] = Map(
    UNASSIGNED -> "Cn",
    UPPERCASE_LETTER -> "Lu",
    LOWERCASE_LETTER -> "Ll",
    TITLECASE_LETTER -> "Lt",
    MODIFIER_LETTER -> "Lm",
    OTHER_LETTER -> "Lo",
    NON_SPACING_MARK -> "Mn",
    ENCLOSING_MARK -> "Me",
    COMBINING_SPACING_MARK -> "Mc",
    DECIMAL_DIGIT_NUMBER -> "Nd",
    LETTER_NUMBER -> "Nl",
    OTHER_NUMBER -> "No",
    SPACE_SEPARATOR -> "Zs",
    LINE_SEPARATOR -> "Zl",
    PARAGRAPH_SEPARATOR -> "Zp",
    CONTROL -> "Cc",
    FORMAT -> "Cf",
    PRIVATE_USE -> "Co",
    SURROGATE -> "Cs",
    DASH_PUNCTUATION -> "Pd",
    START_PUNCTUATION -> "Ps",
    END_PUNCTUATION -> "Pe",
    CONNECTOR_PUNCTUATION -> "Pc",
    OTHER_PUNCTUATION -> "Po",
    MATH_SYMBOL -> "Sm",
    CURRENCY_SYMBOL -> "Sc",
    MODIFIER_SYMBOL -> "Sk",
    OTHER_SYMBOL -> "So",
    INITIAL_QUOTE_PUNCTUATION -> "Pi",
    FINAL_QUOTE_PUNCTUATION -> "Pf")

  private val categoriesMap: Map[String, Set[Int]] =
    (0 to MAX_UTF32_VAL).map(i => (categoryNames(getType(i).asInstanceOf[Byte]), i))
      .groupBy(_._1)                // group by category name
      .mapValues(_.map(_._2).toSet) // transform the values of the form list(String, int) to a set of Int

}