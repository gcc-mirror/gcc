/* InputSubset.java -- subsets of Unicode important in text input
   Copyright (C) 2002, 2003, 2005  Free Software Foundation, Inc.

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

package java.awt.im;

/**
 * Defines additional Unicode character blocks for use by input methods.
 * These constants encompass several Unicode blocks, or portions thereof, for
 * simplification over {@link Character.UnicodeBlock}.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.2
 * @status updated to 1.4
 */
public final class InputSubset extends Character.Subset
{
  /**
   * Constant for all Latin characters, including the characters in the
   * BASIC_LATIN, LATIN_1_SUPPLEMENT, LATIN_EXTENDED_A, LATIN_EXTENDED_B
   * Unicode character blocks.
   */
  public static final InputSubset LATIN = new InputSubset("LATIN");

  /**
   * Constant for the digits included in the BASIC_LATIN Unicode character
   * block.
   */
  public static final InputSubset LATIN_DIGITS
    = new InputSubset("LATIN_DIGITS");

  /**
   * Constant for all Han characters used in writing Traditional Chinese,
   * including a subset of the CJK unified ideographs as well as Traditional
   * Chinese Han characters that may be defined as surrogate characters.
   */
  public static final InputSubset TRADITIONAL_HANZI
    = new InputSubset("TRADITIONAL_HANZI");

  /**
   * Constant for all Han characters used in writing Simplified Chinese,
   * including a subset of the CJK unified ideographs as well as Simplified
   * Chinese Han characters that may be defined as surrogate characters.
   */
  public static final InputSubset SIMPLIFIED_HANZI
    = new InputSubset("SIMPLIFIED_HANZI");

  /**
   * Constant for all Han characters used in writing Japanese, including a
   * subset of the CJK unified ideographs as well as Japanese Han characters
   * that may be defined as surrogate characters.
   */
  public static final InputSubset KANJI = new InputSubset("KANJI");

  /**
   * Constant for all Han characters used in writing Korean, including a
   * subset of the CJK unified ideographs as well as Korean Han characters
   * that may be defined as surrogate characters.
   */
  public static final InputSubset HANJA = new InputSubset("HANJA");

  /**
   * Constant for the halfwidth katakana subset of the Unicode halfwidth and
   * fullwidth forms character block.
   */
  public static final InputSubset HALFWIDTH_KATAKANA
    = new InputSubset("HALFWIDTH_KATAKANA");

  /**
   * Constant for the fullwidth ASCII variants subset of the Unicode
   * halfwidth and fullwidth forms character block.
   *
   * @since 1.3
   */
  public static final InputSubset FULLWIDTH_LATIN
    = new InputSubset("FULLWIDTH_LATIN");

  /**
   * Constant for the fullwidth digits included in the Unicode halfwidth and
   * fullwidth forms character block.
   *
   * @since 1.3
   */
  public static final InputSubset FULLWIDTH_DIGITS
    = new InputSubset("FULLWIDTH_DIGITS");

  /**
   * Construct a subset.
   *
   * @param name the subset name
   */
  private InputSubset(String name)
  {
    super(name);
  }
} // class InputSubset
