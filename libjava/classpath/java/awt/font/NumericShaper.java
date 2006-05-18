/* NumericShaper.java
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.awt.font;

import java.io.Serializable;
import java.lang.Character.UnicodeBlock;

/**
 * This class handles numeric shaping.  A shaper can either be contextual
 * or not.  A non-contextual shaper will always translate ASCII digits
 * in its input into the target Unicode range.  A contextual shaper will
 * change the target Unicode range depending on the characters it has
 * previously processed.
 *
 * @author Michael Koch
 * @author Tom Tromey
 *
 * @since 1.4
 * @specnote This class does not handle LIMBU or OSMANYA.
 * @specnote The JDK does not seem to properly handle ranges without a
 * digit zero, such as TAMIL.  This implementation does.
 */
public final class NumericShaper implements Serializable
{
  private static final long serialVersionUID = -8022764705923730308L;

  /** Convenience constant representing all the valid Unicode ranges.  */
  public static final int ALL_RANGES  = 524287;

  /**
   * Constant representing the Unicode ARABIC range.  Shaping done
   * using this range will translate to the arabic decimal characters.
   * Use EASTERN_ARABIC if you want to shape to the eastern arabic
   * (also known as the extended arabic) decimal characters.
   */
  public static final int ARABIC  = 2;

  /** Constant representing the Unicode BENGALI range.  */
  public static final int BENGALI  = 16;

  /** Constant representing the Unicode DEVANAGARI range.  */
  public static final int DEVANAGARI  = 8;

  /**
   * Constant representing the Unicode extended arabic range.
   * In Unicode there are two different sets of arabic digits;
   * this selects the extended or eastern set.
   */
  public static final int EASTERN_ARABIC  = 4;

  /**
   * Constant representing the Unicode ETHIOPIC range.  Note that
   * there is no digit zero in this range; an ASCII digit zero
   * is left unchanged when shaping to this range.
   */
  public static final int ETHIOPIC  = 65536;

  /**
   * Constant representing the Unicode EUROPEAN range.  For
   * contextual shaping purposes, characters in the various
   * extended Latin character blocks are recognized as EUROPEAN.
   */
  public static final int EUROPEAN  = 1;

  /** Constant representing the Unicode GUJARATI range.  */
  public static final int GUJARATI  = 64;

  /** Constant representing the Unicode GURMUKHI range.  */
  public static final int GURMUKHI  = 32;

  /** Constant representing the Unicode KANNADA range.  */
  public static final int KANNADA  = 1024;

  /** Constant representing the Unicode KHMER range.  */
  public static final int KHMER  = 131072;

  /** Constant representing the Unicode LAO range.  */
  public static final int LAO  = 8192;

  /** Constant representing the Unicode MALAYALAM range.  */
  public static final int MALAYALAM  = 2048;

  /** Constant representing the Unicode MONGOLIAN range.  */
  public static final int MONGOLIAN  = 262144;

  /** Constant representing the Unicode MYANMAR range.  */
  public static final int MYANMAR  = 32768;

  /** Constant representing the Unicode ORIYA range.  */
  public static final int ORIYA  = 128;

  /**
   * Constant representing the Unicode TAMIL range.  Note that
   * there is no digit zero in this range; an ASCII digit zero
   * is left unchanged when shaping to this range.
   */
  public static final int TAMIL  = 256;

  /** Constant representing the Unicode TELUGU range.  */
  public static final int TELUGU  = 512;

  /** Constant representing the Unicode THAI range.  */
  public static final int THAI  = 4096;

  /** Constant representing the Unicode TIBETAN range.  */
  public static final int TIBETAN  = 16384;

  /**
   * This table holds the zero digits for each language.  This is hard-coded
   * because the values will not change and the table layout is tied to the
   * other constants in this class in any case.  In the two places where a
   * language does not have a zero digit, the character immediately preceeding
   * the one digit is used instead.  These languages are special-cased in
   * the shaping code.
   */
  private static final char[] zeroDigits =
  {
    '0',      // EUROPEAN
    '\u0660', // ARABIC
    '\u06f0', // EASTERN_ARABIC
    '\u0966', // DEVANAGARI
    '\u09e6', // BENGALI
    '\u0a66', // GURMUKHI
    '\u0ae6', // GUJARATI
    '\u0b66', // ORIYA
    '\u0be6', // TAMIL - special case as there is no digit zero
    '\u0c66', // TELUGU
    '\u0ce6', // KANNADA
    '\u0d66', // MALAYALAM
    '\u0e50', // THAI
    '\u0ed0', // LAO
    '\u0f20', // TIBETAN
    '\u1040', // MYANMAR
    '\u1368', // ETHIOPIC - special case as there is no digit zero
    '\u17e0', // KHMER
    '\u1810'  // MONGOLIAN
  };

  /**
   * The default initial context for this shaper, specified as
   * an integer from 0 to 18.
   */
  private int key;

  /**
   * The target ranges handled by this shaper.  If the shaper
   * is not contextual, the high bit of this field will be set.
   * @specnote This was discovered by reading the serialization spec
   */
  private int mask;

  /**
   * Create a new numeric shaper.  The key given is a constant from
   * this class, the constructor turns it into its internal form.
   * @param key the key to use, as one of the manifest constants
   * @param mask a mask of languages to shape for
   */
  private NumericShaper (int key, int mask)
  {
    // This internal form is a bit goofy, but it is specified by
    // the serialization spec.
    this.key = Integer.numberOfTrailingZeros(key);
    this.mask = mask;
  }

  /**
   * Return an integer representing all the languages for which this
   * shaper will shape.  The result is taken by "or"ing together
   * the constants representing the various languages.
   */
  public int getRanges ()
  {
    return mask & ALL_RANGES;
  }

  /**
   * Return true if this shaper is contextual, false if it is not.
   */
  public boolean isContextual ()
  {
    return mask > 0;
  }

  /**
   * Shape the text in the given array.  The starting context will
   * be the context passed to the shaper at creation time.
   * @param text the text to shape
   * @param start the index of the starting character of the array
   * @param count the number of characters in the array
   */
  public void shape (char[] text, int start, int count)
  {
    shape (text, start, count, 1 << key);
  }

  /**
   * Given a unicode block object, return corresponding language constant.
   * If the block is not recognized, returns zero.  Note that as there
   * is no separate ARABIC block in Character, this case must
   * be specially handled by the caller; EASTERN_ARABIC is preferred when
   * both are specified.
   * @param b the unicode block to classify
   * @return the language constant, or zero if not recognized
   */
  private int classify(UnicodeBlock b)
  {
    if (b == null)
      return 0;
    // ARABIC is handled by the caller; from testing we know
    // that EASTERN_ARABIC takes precedence.
    if (b == UnicodeBlock.ARABIC)
      return EASTERN_ARABIC;
    if (b == UnicodeBlock.BENGALI)
      return BENGALI;
    if (b == UnicodeBlock.DEVANAGARI)
      return DEVANAGARI;
    if (b == UnicodeBlock.ETHIOPIC)
      return ETHIOPIC;
    if (b == UnicodeBlock.BASIC_LATIN
        || b == UnicodeBlock.LATIN_1_SUPPLEMENT
        || b == UnicodeBlock.LATIN_EXTENDED_A
        || b == UnicodeBlock.LATIN_EXTENDED_ADDITIONAL
        || b == UnicodeBlock.LATIN_EXTENDED_B)
      return EUROPEAN;
    if (b == UnicodeBlock.GUJARATI)
      return GUJARATI;
    if (b == UnicodeBlock.GURMUKHI)
      return GURMUKHI;
    if (b == UnicodeBlock.KANNADA)
      return KANNADA;
    if (b == UnicodeBlock.KHMER)
      return KHMER;
    if (b == UnicodeBlock.LAO)
      return LAO;
    if (b == UnicodeBlock.MALAYALAM)
      return MALAYALAM;
    if (b == UnicodeBlock.MONGOLIAN)
      return MONGOLIAN;
    if (b == UnicodeBlock.MYANMAR)
      return MYANMAR;
    if (b == UnicodeBlock.ORIYA)
      return ORIYA;
    if (b == UnicodeBlock.TAMIL)
      return TAMIL;
    if (b == UnicodeBlock.TELUGU)
      return TELUGU;
    if (b == UnicodeBlock.THAI)
      return THAI;
    if (b == UnicodeBlock.TIBETAN)
      return TIBETAN;
    return 0;
  }

  /**
   * Shape the given text, using the indicated initial context.
   * If this shaper is not a contextual shaper, then the given context
   * will be ignored.
   * @param text the text to shape
   * @param start the index of the first character of the text to shape
   * @param count the number of characters to shape in the text
   * @param context the initial context
   * @throws IllegalArgumentException if the initial context is invalid
   */
  public void shape (char[] text, int start, int count, int context)
  {
    int currentContext;
    if (isContextual())
      {
        if (Integer.bitCount(context) != 1 || (context & ~ALL_RANGES) != 0)
          throw new IllegalArgumentException("invalid context argument");
        // If the indicated context is not one we are handling, reset it.
        if ((context & mask) == 0)
          currentContext = -1;
        else
          currentContext = Integer.numberOfTrailingZeros(context);
      }
    else
      currentContext = key;

    for (int i = 0; i < count; ++i)
      {
        char c = text[start + i];
        if (c >= '0' && c <= '9')
          {
            if (currentContext >= 0)
              {
                // Shape into the current context.
                if (c == '0'
                  && ((1 << currentContext) == TAMIL
                      || (1 << currentContext) == ETHIOPIC))
                  {
                    // No digit 0 in this context; do nothing.
                  }
                else
                  text[start + i]
                    = (char) (zeroDigits[currentContext] + c - '0');
              }
          }
        else if (isContextual())
          {
            // if c is in a group, set currentContext; else reset it.
            int group = classify(UnicodeBlock.of(c));
            // Specially handle ARABIC.
            if (group == EASTERN_ARABIC && (mask & EASTERN_ARABIC) == 0
                && (mask & ARABIC) != 0)
              group = ARABIC;
            if ((mask & group) != 0)
              {
                // The character was classified as being in a group
                // we recognize, and it was selected by the shaper.
                // So, change the context.
                currentContext = Integer.numberOfTrailingZeros(group);
              }
          }
      }
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof NumericShaper))
      return false;
    NumericShaper tmp = (NumericShaper) obj;
    return key == tmp.key && mask == tmp.mask;
  }

  public int hashCode ()
  {
    return key ^ mask;
  }

  public String toString ()
  {
    // For debugging only.
    return "key=" + key + "; mask=" + mask;
  }

  /**
   * Return a non-contextual shaper which can shape to a single range.
   * All ASCII digits in the input text are translated to this language.
   * @param singleRange the target language
   * @return a non-contextual shaper for this language
   * @throws IllegalArgumentException if the argument does not name a
   * single language, as specified by the constants declared in this class
   */
  public static NumericShaper getShaper (int singleRange)
  {
    if (Integer.bitCount(singleRange) != 1)
      throw new IllegalArgumentException("more than one bit set in argument");
    if ((singleRange & ~ALL_RANGES) != 0)
      throw new IllegalArgumentException("argument out of range");
    return new NumericShaper(singleRange, Integer.MIN_VALUE | singleRange);
  }

  /**
   * Return a contextual shaper which can shape to any of the indicated
   * languages.  The default initial context for this shaper is EUROPEAN.
   * @param ranges the ranges to shape to
   * @return a contextual shaper which will target any of these ranges
   * @throws IllegalArgumentException if the argument specifies an
   * unrecognized range
   */
  public static NumericShaper getContextualShaper (int ranges)
  {
    if ((ranges & ~ALL_RANGES) != 0)
      throw new IllegalArgumentException("argument out of range");
    return new NumericShaper(EUROPEAN, ranges);
  }

  /**
   * Return a contextual shaper which can shape to any of the indicated
   * languages.  The default initial context for this shaper is given as
   * an argument.
   * @param ranges the ranges to shape to
   * @param defaultContext the default initial context
   * @return a contextual shaper which will target any of these ranges
   * @throws IllegalArgumentException if the ranges argument specifies an
   * unrecognized range, or if the defaultContext argument does not specify
   * a single valid range
   */
  public static NumericShaper getContextualShaper (int ranges,
                                                   int defaultContext)
  {
    if (Integer.bitCount(defaultContext) != 1)
      throw new IllegalArgumentException("more than one bit set in context");
    if ((ranges & ~ALL_RANGES) != 0 || (defaultContext & ~ALL_RANGES) != 0)
      throw new IllegalArgumentException("argument out of range");
    return new NumericShaper(defaultContext, ranges);
  }
}
