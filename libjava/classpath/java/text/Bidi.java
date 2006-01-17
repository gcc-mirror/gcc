/* Bidi.java -- Bidirectional Algorithm implementation
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package java.text;

/**
 * Bidirectional Algorithm implementation.
 *
 * TODO/FIXME Only one method <code>requiresBidi</code> is implemented
 * for now by using <code>Character</code>. The full algorithm is <a
 * href="http://www.unicode.org/unicode/reports/tr9/">Unicode Standard
 * Annex #9: The Bidirectional Algorithm</a>. A full implementation is
 * <a href="http://fribidi.org/">GNU FriBidi</a>.
 */
public class Bidi
{
  /**
   * Returns false if all characters in the text between start and end
   * are all left-to-right text. This implementation is just calls
   * <code>Character.getDirectionality(char)</code> on all characters
   * and makes sure all characters are either explicitly left-to-right
   * or neutral in directionality (character types L, EN, ES, ET, AN,
   * CS, S and WS).
   */
  public static boolean requiresBidi(char[] text, int start, int end)
  {
    for (int i = start; i < end; i++)
      {
	byte dir = Character.getDirectionality(text[i]);
	if (dir != Character.DIRECTIONALITY_LEFT_TO_RIGHT
	    && dir != Character.DIRECTIONALITY_EUROPEAN_NUMBER
	    && dir != Character.DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR
	    && dir != Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
	    && dir != Character.DIRECTIONALITY_ARABIC_NUMBER
	    && dir != Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR
	    && dir != Character.DIRECTIONALITY_SEGMENT_SEPARATOR
	    && dir != Character.DIRECTIONALITY_WHITESPACE)
	  return true;
      }

    return false;
  }
}
