// Character.java - Character class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.io.Serializable;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 10, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1,
 * online API docs for JDK 1.2 beta from http://www.javasoft.com,
 * and The Unicode Standard Version 2.0.
 * Status: Believed complete and correct for JDK 1.1; 1.2 methods
 * unimplemented.
 */

public final class Character implements Serializable, Comparable
{
  public static final char MIN_VALUE = '\u0000';
  public static final char MAX_VALUE = '\uffff';

  public static final int MIN_RADIX = 2;
  public static final int MAX_RADIX = 36;

  public static final Class TYPE = VMClassLoader.getPrimitiveClass('C');

  // Space.
  public static final byte SPACE_SEPARATOR     = 12;
  public static final byte LINE_SEPARATOR      = 13;
  public static final byte PARAGRAPH_SEPARATOR = 14;

  // Letters.
  public static final byte UPPERCASE_LETTER = 1;
  public static final byte LOWERCASE_LETTER = 2;
  public static final byte TITLECASE_LETTER = 3;
  public static final byte MODIFIER_LETTER  = 4;
  public static final byte OTHER_LETTER     = 5;

  // Numbers.
  public static final byte DECIMAL_DIGIT_NUMBER =  9;
  public static final byte LETTER_NUMBER        = 10;
  public static final byte OTHER_NUMBER         = 11;

  // Marks.
  public static final byte NON_SPACING_MARK     = 6;
  public static final byte ENCLOSING_MARK       = 7;
  public static final byte COMBINING_SPACING_MARK = 8;

  // Punctuation.
  public static final byte DASH_PUNCTUATION      = 20;
  public static final byte START_PUNCTUATION     = 21;
  public static final byte END_PUNCTUATION       = 22;
  public static final byte CONNECTOR_PUNCTUATION = 23;
  public static final byte OTHER_PUNCTUATION     = 24;

  // Symbols.
  public static final byte MATH_SYMBOL     = 25;
  public static final byte CURRENCY_SYMBOL = 26;
  public static final byte MODIFIER_SYMBOL = 27;
  public static final byte OTHER_SYMBOL    = 28;

  // Format controls.
  public static final byte CONTROL = 15;
  // Note: The JCL book says that both FORMAT and PRIVATE_USE are 18.
  // However, FORMAT is actually 16.
  public static final byte FORMAT  = 16;

  // Others.
  public static final byte UNASSIGNED  = 0;
  public static final byte PRIVATE_USE = 18;
  public static final byte SURROGATE   = 19;

  private static final long serialVersionUID = 3786198910865385080L;

  public Character (char ch)
  {
    value = ch;
  }

  public char charValue ()
  {
    return value;
  }

  // See if a character is a digit.  If so, return the corresponding
  // value.  Otherwise return -1.
  private static native int digit_value (char ch);

  public static int digit (char ch, int radix)
  {
    if (radix < MIN_RADIX || radix > MAX_RADIX)
      return -1;

    int d = digit_value (ch);
    if (d == -1)
      {
	if (ch >= 'A' && ch <= 'Z')
	  d = ch - 'A' + 10;
	else if (ch >= 'a' && ch <= 'z')
	  d = ch - 'a' + 10;
	else
	  return -1;
      }
    return d >= radix ? -1 : d;
  }

  public boolean equals (Object obj)
  {
    // Don't need to compare OBJ to null as instanceof will do this.
    if (obj instanceof Character)
      return value == ((Character) obj).value;
    return false;
  }

  public static char forDigit (int d, int rdx)
  {
    if (d < 0 || d >= rdx || rdx < MIN_RADIX || rdx > MAX_RADIX)
      return '\u0000';
    if (d < 10)
      return (char) ('0' + d);
    // The Java Language Spec says to use lowercase, while the JCL
    // says to use uppercase.  We go with the former.
    return (char) ('a' + d - 10);
  }

  public static native int getNumericValue (char ch);
  public static native int getType (char ch);

  public int hashCode ()
  {
    return value;
  }

  public static boolean isDefined (char ch)
  {
    return getType (ch) != UNASSIGNED;
  }

  public static boolean isDigit (char ch)
  {
    return digit_value (ch) != -1;
  }

  // The JCL book says that the argument here is a Character.  That is
  // wrong.
  public static boolean isIdentifierIgnorable (char ch)
  {
    // This information comes from the Unicode Standard.  It isn't
    // auto-generated as it doesn't appear in the unidata table.
    return ((ch >= '\u0000' && ch <= '\u0008')
	    || (ch >= '\u000e' && ch <= '\u001b')
	    // JDK 1.2 docs say that these are ignorable.  The Unicode
	    // Standard is somewhat ambiguous on this issue.
	    || (ch >= '\u007f' && ch <= '\u009f')
	    || (ch >= '\u200c' && ch <= '\u200f')
	    // JCl says 200a through 200e, but that is a typo.  The
	    // Unicode standard says the bidi controls are 202a
	    // through 202e.
	    || (ch >= '\u202a' && ch <= '\u202e')
	    || (ch >= '\u206a' && ch <= '\u206f')
	    || ch == '\ufeff');
  }

  public static boolean isISOControl (char c)
  {
    return ((c >= '\u0000' && c <= '\u001f')
	    || (c >= '\u007f' && c <= '\u009f'));
  }

  public static boolean isJavaIdentifierPart (char ch)
  {
    if (isIdentifierIgnorable (ch) || isDigit (ch))
      return true;
    int type = getType (ch);
    return (type == COMBINING_SPACING_MARK || type == NON_SPACING_MARK
	    || type == CURRENCY_SYMBOL || type == CONNECTOR_PUNCTUATION
	    || type == UPPERCASE_LETTER || type == LOWERCASE_LETTER
	    || type == TITLECASE_LETTER || type == MODIFIER_LETTER
	    || type == OTHER_LETTER || type == LETTER_NUMBER);
  }

  public static boolean isJavaIdentifierStart (char ch)
  {
    int type = getType (ch);
    return (type == CURRENCY_SYMBOL || type == CONNECTOR_PUNCTUATION
	    || type == UPPERCASE_LETTER || type == LOWERCASE_LETTER
	    || type == TITLECASE_LETTER || type == MODIFIER_LETTER
	    || type == OTHER_LETTER);
  }

  // Deprecated in 1.2.
  public static boolean isJavaLetter (char ch)
  {
    return ch == '$' || ch == '_' || isLetter (ch);
  }

  // Deprecated in 1.2.
  public static boolean isJavaLetterOrDigit (char ch)
  {
    return ch == '$' || ch == '_' || isLetterOrDigit (ch);
  }

  public static boolean isLetter (char ch)
  {
    int type = getType (ch);
    return (type == UPPERCASE_LETTER || type == LOWERCASE_LETTER
	    || type == TITLECASE_LETTER || type == MODIFIER_LETTER
	    || type == OTHER_LETTER);
  }

  public static boolean isLetterOrDigit (char ch)
  {
    return isDigit (ch) || isLetter (ch);
  }

  public static native boolean isLowerCase (char ch);

  // Deprecated in JCL.
  public static boolean isSpace (char ch)
  {
    return ch == '\n' || ch == '\t' || ch == '\f' || ch == '\r' || ch == ' ';
  }

  public static native boolean isSpaceChar (char ch);
  public static native boolean isTitleCase (char ch);

  public static boolean isUnicodeIdentifierPart (char ch)
  {
    if (isIdentifierIgnorable (ch) || isDigit (ch))
      return true;
    int type = getType (ch);
    return (type == CONNECTOR_PUNCTUATION || type == LETTER_NUMBER
	    || type == COMBINING_SPACING_MARK || type == NON_SPACING_MARK
	    || type == UPPERCASE_LETTER || type == LOWERCASE_LETTER
	    || type == TITLECASE_LETTER || type == MODIFIER_LETTER
	    || type == OTHER_LETTER);
  }

  public static boolean isUnicodeIdentifierStart (char ch)
  {
    return isLetter (ch);
  }

  public static native boolean isUpperCase (char ch);

  public static boolean isWhitespace (char ch)
  {
    return ((ch >= '\u0009' && ch <= '\r')
	    || (ch >= '\u001c' && ch <= '\u001f')
	    || (ch != '\u00a0' && ch != '\ufeff' && isSpaceChar (ch)));
  }

  public static native char toLowerCase (char ch);
  public static native char toTitleCase (char ch);
  public static native char toUpperCase (char ch);

  public String toString ()
  {
    return String.valueOf(value);
  }

  public int compareTo (Character anotherCharacter)
  {
    return value - anotherCharacter.value;
  }

  public int compareTo (Object o)
  {
    return compareTo ((Character) o);
  }

  // Private data.
  private char value;

  public static class Subset
  {
    protected Subset (String name)
    {
      this.name = name;
    }

    public final boolean equals (Object obj)
    {
      return obj == this;
    }

    public final int hashCode ()
    {
      return super.hashCode ();
    }

    public final String toString ()
    {
      return name;
    }

    // Name of this subset.
    private String name;
  }

  public static final class UnicodeBlock extends Subset
  {
    private UnicodeBlock (String name, char start, char end)
    {
      super (name);
      this.start = start;
      this.end = end;
    }

    public static UnicodeBlock of (char c)
    {
      // A special case we need.
      if (c == '\uFEFF')
	return SPECIALS;

      // Do a binary search to find the correct subset.
      int hi = blocks.length;
      int lo = 0;
      while (hi > lo)
	{
	  int mid = (hi + lo) / 2;
	  UnicodeBlock ub = blocks[mid];
	  if (c < ub.start)
	    hi = mid;
	  else if (c > ub.end)
	    lo = mid;
	  else
	    return ub;
	}

      return null;
    }

    // Start and end characters.
    private char start, end;

    // Everything from here to the end of UnicodeBlock is
    // automatically generated by the blocks.pl script.
    public static final UnicodeBlock BASIC_LATIN = new UnicodeBlock ("Basic Latin", '\u0000', '\u007F');
    public static final UnicodeBlock LATIN_1_SUPPLEMENT = new UnicodeBlock ("Latin-1 Supplement", '\u0080', '\u00FF');
    public static final UnicodeBlock LATIN_EXTENDED_A = new UnicodeBlock ("Latin Extended-A", '\u0100', '\u017F');
    public static final UnicodeBlock LATIN_EXTENDED_B = new UnicodeBlock ("Latin Extended-B", '\u0180', '\u024F');
    public static final UnicodeBlock IPA_EXTENSIONS = new UnicodeBlock ("IPA Extensions", '\u0250', '\u02AF');
    public static final UnicodeBlock SPACING_MODIFIER_LETTERS = new UnicodeBlock ("Spacing Modifier Letters", '\u02B0', '\u02FF');
    public static final UnicodeBlock COMBINING_DIACRITICAL_MARKS = new UnicodeBlock ("Combining Diacritical Marks", '\u0300', '\u036F');
    public static final UnicodeBlock GREEK = new UnicodeBlock ("Greek", '\u0370', '\u03FF');
    public static final UnicodeBlock CYRILLIC = new UnicodeBlock ("Cyrillic", '\u0400', '\u04FF');
    public static final UnicodeBlock ARMENIAN = new UnicodeBlock ("Armenian", '\u0530', '\u058F');
    public static final UnicodeBlock HEBREW = new UnicodeBlock ("Hebrew", '\u0590', '\u05FF');
    public static final UnicodeBlock ARABIC = new UnicodeBlock ("Arabic", '\u0600', '\u06FF');
    public static final UnicodeBlock SYRIAC__ = new UnicodeBlock ("Syriac  ", '\u0700', '\u074F');
    public static final UnicodeBlock THAANA = new UnicodeBlock ("Thaana", '\u0780', '\u07BF');
    public static final UnicodeBlock DEVANAGARI = new UnicodeBlock ("Devanagari", '\u0900', '\u097F');
    public static final UnicodeBlock BENGALI = new UnicodeBlock ("Bengali", '\u0980', '\u09FF');
    public static final UnicodeBlock GURMUKHI = new UnicodeBlock ("Gurmukhi", '\u0A00', '\u0A7F');
    public static final UnicodeBlock GUJARATI = new UnicodeBlock ("Gujarati", '\u0A80', '\u0AFF');
    public static final UnicodeBlock ORIYA = new UnicodeBlock ("Oriya", '\u0B00', '\u0B7F');
    public static final UnicodeBlock TAMIL = new UnicodeBlock ("Tamil", '\u0B80', '\u0BFF');
    public static final UnicodeBlock TELUGU = new UnicodeBlock ("Telugu", '\u0C00', '\u0C7F');
    public static final UnicodeBlock KANNADA = new UnicodeBlock ("Kannada", '\u0C80', '\u0CFF');
    public static final UnicodeBlock MALAYALAM = new UnicodeBlock ("Malayalam", '\u0D00', '\u0D7F');
    public static final UnicodeBlock SINHALA = new UnicodeBlock ("Sinhala", '\u0D80', '\u0DFF');
    public static final UnicodeBlock THAI = new UnicodeBlock ("Thai", '\u0E00', '\u0E7F');
    public static final UnicodeBlock LAO = new UnicodeBlock ("Lao", '\u0E80', '\u0EFF');
    public static final UnicodeBlock TIBETAN = new UnicodeBlock ("Tibetan", '\u0F00', '\u0FFF');
    public static final UnicodeBlock MYANMAR_ = new UnicodeBlock ("Myanmar ", '\u1000', '\u109F');
    public static final UnicodeBlock GEORGIAN = new UnicodeBlock ("Georgian", '\u10A0', '\u10FF');
    public static final UnicodeBlock HANGUL_JAMO = new UnicodeBlock ("Hangul Jamo", '\u1100', '\u11FF');
    public static final UnicodeBlock ETHIOPIC = new UnicodeBlock ("Ethiopic", '\u1200', '\u137F');
    public static final UnicodeBlock CHEROKEE = new UnicodeBlock ("Cherokee", '\u13A0', '\u13FF');
    public static final UnicodeBlock UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS = new UnicodeBlock ("Unified Canadian Aboriginal Syllabics", '\u1400', '\u167F');
    public static final UnicodeBlock OGHAM = new UnicodeBlock ("Ogham", '\u1680', '\u169F');
    public static final UnicodeBlock RUNIC = new UnicodeBlock ("Runic", '\u16A0', '\u16FF');
    public static final UnicodeBlock KHMER = new UnicodeBlock ("Khmer", '\u1780', '\u17FF');
    public static final UnicodeBlock MONGOLIAN = new UnicodeBlock ("Mongolian", '\u1800', '\u18AF');
    public static final UnicodeBlock LATIN_EXTENDED_ADDITIONAL = new UnicodeBlock ("Latin Extended Additional", '\u1E00', '\u1EFF');
    public static final UnicodeBlock GREEK_EXTENDED = new UnicodeBlock ("Greek Extended", '\u1F00', '\u1FFF');
    public static final UnicodeBlock GENERAL_PUNCTUATION = new UnicodeBlock ("General Punctuation", '\u2000', '\u206F');
    public static final UnicodeBlock SUPERSCRIPTS_AND_SUBSCRIPTS = new UnicodeBlock ("Superscripts and Subscripts", '\u2070', '\u209F');
    public static final UnicodeBlock CURRENCY_SYMBOLS = new UnicodeBlock ("Currency Symbols", '\u20A0', '\u20CF');
    public static final UnicodeBlock COMBINING_MARKS_FOR_SYMBOLS = new UnicodeBlock ("Combining Marks for Symbols", '\u20D0', '\u20FF');
    public static final UnicodeBlock LETTERLIKE_SYMBOLS = new UnicodeBlock ("Letterlike Symbols", '\u2100', '\u214F');
    public static final UnicodeBlock NUMBER_FORMS = new UnicodeBlock ("Number Forms", '\u2150', '\u218F');
    public static final UnicodeBlock ARROWS = new UnicodeBlock ("Arrows", '\u2190', '\u21FF');
    public static final UnicodeBlock MATHEMATICAL_OPERATORS = new UnicodeBlock ("Mathematical Operators", '\u2200', '\u22FF');
    public static final UnicodeBlock MISCELLANEOUS_TECHNICAL = new UnicodeBlock ("Miscellaneous Technical", '\u2300', '\u23FF');
    public static final UnicodeBlock CONTROL_PICTURES = new UnicodeBlock ("Control Pictures", '\u2400', '\u243F');
    public static final UnicodeBlock OPTICAL_CHARACTER_RECOGNITION = new UnicodeBlock ("Optical Character Recognition", '\u2440', '\u245F');
    public static final UnicodeBlock ENCLOSED_ALPHANUMERICS = new UnicodeBlock ("Enclosed Alphanumerics", '\u2460', '\u24FF');
    public static final UnicodeBlock BOX_DRAWING = new UnicodeBlock ("Box Drawing", '\u2500', '\u257F');
    public static final UnicodeBlock BLOCK_ELEMENTS = new UnicodeBlock ("Block Elements", '\u2580', '\u259F');
    public static final UnicodeBlock GEOMETRIC_SHAPES = new UnicodeBlock ("Geometric Shapes", '\u25A0', '\u25FF');
    public static final UnicodeBlock MISCELLANEOUS_SYMBOLS = new UnicodeBlock ("Miscellaneous Symbols", '\u2600', '\u26FF');
    public static final UnicodeBlock DINGBATS = new UnicodeBlock ("Dingbats", '\u2700', '\u27BF');
    public static final UnicodeBlock BRAILLE_PATTERNS = new UnicodeBlock ("Braille Patterns", '\u2800', '\u28FF');
    public static final UnicodeBlock CJK_RADICALS_SUPPLEMENT = new UnicodeBlock ("CJK Radicals Supplement", '\u2E80', '\u2EFF');
    public static final UnicodeBlock KANGXI_RADICALS = new UnicodeBlock ("Kangxi Radicals", '\u2F00', '\u2FDF');
    public static final UnicodeBlock IDEOGRAPHIC_DESCRIPTION_CHARACTERS = new UnicodeBlock ("Ideographic Description Characters", '\u2FF0', '\u2FFF');
    public static final UnicodeBlock CJK_SYMBOLS_AND_PUNCTUATION = new UnicodeBlock ("CJK Symbols and Punctuation", '\u3000', '\u303F');
    public static final UnicodeBlock HIRAGANA = new UnicodeBlock ("Hiragana", '\u3040', '\u309F');
    public static final UnicodeBlock KATAKANA = new UnicodeBlock ("Katakana", '\u30A0', '\u30FF');
    public static final UnicodeBlock BOPOMOFO = new UnicodeBlock ("Bopomofo", '\u3100', '\u312F');
    public static final UnicodeBlock HANGUL_COMPATIBILITY_JAMO = new UnicodeBlock ("Hangul Compatibility Jamo", '\u3130', '\u318F');
    public static final UnicodeBlock KANBUN = new UnicodeBlock ("Kanbun", '\u3190', '\u319F');
    public static final UnicodeBlock BOPOMOFO_EXTENDED = new UnicodeBlock ("Bopomofo Extended", '\u31A0', '\u31BF');
    public static final UnicodeBlock ENCLOSED_CJK_LETTERS_AND_MONTHS = new UnicodeBlock ("Enclosed CJK Letters and Months", '\u3200', '\u32FF');
    public static final UnicodeBlock CJK_COMPATIBILITY = new UnicodeBlock ("CJK Compatibility", '\u3300', '\u33FF');
    public static final UnicodeBlock CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A = new UnicodeBlock ("CJK Unified Ideographs Extension A", '\u3400', '\u4DB5');
    public static final UnicodeBlock CJK_UNIFIED_IDEOGRAPHS = new UnicodeBlock ("CJK Unified Ideographs", '\u4E00', '\u9FFF');
    public static final UnicodeBlock YI_SYLLABLES = new UnicodeBlock ("Yi Syllables", '\uA000', '\uA48F');
    public static final UnicodeBlock YI_RADICALS = new UnicodeBlock ("Yi Radicals", '\uA490', '\uA4CF');
    public static final UnicodeBlock HANGUL_SYLLABLES = new UnicodeBlock ("Hangul Syllables", '\uAC00', '\uD7A3');
    public static final UnicodeBlock SURROGATES_AREA = new UnicodeBlock ("Surrogates Area", '\uD800', '\uDFFF');
    public static final UnicodeBlock PRIVATE_USE_AREA = new UnicodeBlock ("Private Use Area", '\uE000', '\uF8FF');
    public static final UnicodeBlock CJK_COMPATIBILITY_IDEOGRAPHS = new UnicodeBlock ("CJK Compatibility Ideographs", '\uF900', '\uFAFF');
    public static final UnicodeBlock ALPHABETIC_PRESENTATION_FORMS = new UnicodeBlock ("Alphabetic Presentation Forms", '\uFB00', '\uFB4F');
    public static final UnicodeBlock ARABIC_PRESENTATION_FORMS_A = new UnicodeBlock ("Arabic Presentation Forms-A", '\uFB50', '\uFDFF');
    public static final UnicodeBlock COMBINING_HALF_MARKS = new UnicodeBlock ("Combining Half Marks", '\uFE20', '\uFE2F');
    public static final UnicodeBlock CJK_COMPATIBILITY_FORMS = new UnicodeBlock ("CJK Compatibility Forms", '\uFE30', '\uFE4F');
    public static final UnicodeBlock SMALL_FORM_VARIANTS = new UnicodeBlock ("Small Form Variants", '\uFE50', '\uFE6F');
    public static final UnicodeBlock ARABIC_PRESENTATION_FORMS_B = new UnicodeBlock ("Arabic Presentation Forms-B", '\uFE70', '\uFEFE');
    public static final UnicodeBlock HALFWIDTH_AND_FULLWIDTH_FORMS = new UnicodeBlock ("Halfwidth and Fullwidth Forms", '\uFF00', '\uFFEF');
    public static final UnicodeBlock SPECIALS = new UnicodeBlock ("Specials", '\uFFF0', '\uFFFD');
    private static final UnicodeBlock[] blocks = {
      BASIC_LATIN,
      LATIN_1_SUPPLEMENT,
      LATIN_EXTENDED_A,
      LATIN_EXTENDED_B,
      IPA_EXTENSIONS,
      SPACING_MODIFIER_LETTERS,
      COMBINING_DIACRITICAL_MARKS,
      GREEK,
      CYRILLIC,
      ARMENIAN,
      HEBREW,
      ARABIC,
      SYRIAC__,
      THAANA,
      DEVANAGARI,
      BENGALI,
      GURMUKHI,
      GUJARATI,
      ORIYA,
      TAMIL,
      TELUGU,
      KANNADA,
      MALAYALAM,
      SINHALA,
      THAI,
      LAO,
      TIBETAN,
      MYANMAR_,
      GEORGIAN,
      HANGUL_JAMO,
      ETHIOPIC,
      CHEROKEE,
      UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS,
      OGHAM,
      RUNIC,
      KHMER,
      MONGOLIAN,
      LATIN_EXTENDED_ADDITIONAL,
      GREEK_EXTENDED,
      GENERAL_PUNCTUATION,
      SUPERSCRIPTS_AND_SUBSCRIPTS,
      CURRENCY_SYMBOLS,
      COMBINING_MARKS_FOR_SYMBOLS,
      LETTERLIKE_SYMBOLS,
      NUMBER_FORMS,
      ARROWS,
      MATHEMATICAL_OPERATORS,
      MISCELLANEOUS_TECHNICAL,
      CONTROL_PICTURES,
      OPTICAL_CHARACTER_RECOGNITION,
      ENCLOSED_ALPHANUMERICS,
      BOX_DRAWING,
      BLOCK_ELEMENTS,
      GEOMETRIC_SHAPES,
      MISCELLANEOUS_SYMBOLS,
      DINGBATS,
      BRAILLE_PATTERNS,
      CJK_RADICALS_SUPPLEMENT,
      KANGXI_RADICALS,
      IDEOGRAPHIC_DESCRIPTION_CHARACTERS,
      CJK_SYMBOLS_AND_PUNCTUATION,
      HIRAGANA,
      KATAKANA,
      BOPOMOFO,
      HANGUL_COMPATIBILITY_JAMO,
      KANBUN,
      BOPOMOFO_EXTENDED,
      ENCLOSED_CJK_LETTERS_AND_MONTHS,
      CJK_COMPATIBILITY,
      CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A,
      CJK_UNIFIED_IDEOGRAPHS,
      YI_SYLLABLES,
      YI_RADICALS,
      HANGUL_SYLLABLES,
      SURROGATES_AREA,
      PRIVATE_USE_AREA,
      CJK_COMPATIBILITY_IDEOGRAPHS,
      ALPHABETIC_PRESENTATION_FORMS,
      ARABIC_PRESENTATION_FORMS_A,
      COMBINING_HALF_MARKS,
      CJK_COMPATIBILITY_FORMS,
      SMALL_FORM_VARIANTS,
      ARABIC_PRESENTATION_FORMS_B,
      HALFWIDTH_AND_FULLWIDTH_FORMS,
      SPECIALS
    };
  }
}
