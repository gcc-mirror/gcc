// Character.java - Character class.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

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

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = char.class;

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
}
