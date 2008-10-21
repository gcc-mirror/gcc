/* gnu/regexp/CharIndexed.java
   Copyright (C) 1998-2001, 2004, 2006 Free Software Foundation, Inc.

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

package gnu.java.util.regex;

/**
 * Defines the interface used internally so that different types of source
 * text can be accessed in the same way.  Built-in concrete classes provide
 * support for String, StringBuffer, InputStream and char[] types.
 * A class that is CharIndexed supports the notion of a cursor within a
 * block of text.  The cursor must be able to be advanced via the move()
 * method.  The charAt() method returns the character at the cursor position
 * plus a given offset.
 *
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A>
 */
public interface CharIndexed
{
    /**
     * Defines a constant (0xFFFF was somewhat arbitrarily chosen)
     * that can be returned by the charAt() function indicating that
     * the specified index is out of range.
     */
  char OUT_OF_BOUNDS = '\uFFFF';

    /**
     * Returns the character at the given offset past the current cursor
     * position in the input.  The index of the current position is zero.
     * It is possible for this method to be called with a negative index.
     * This happens when using the '^' operator in multiline matching mode
     * or the '\b' or '\<' word boundary operators.  In any case, the lower
     * bound is currently fixed at -2 (for '^' with a two-character newline).
     *
     * @param index the offset position in the character field to examine
     * @return the character at the specified index, or the OUT_OF_BOUNDS
     *   character defined by this interface.
     */
  char charAt (int index);

    /**
     * Shifts the input buffer by a given number of positions.  Returns
     * true if the new cursor position is valid.
     */
  boolean move (int index);

    /**
     * Shifts the input buffer by a given number of positions.  Returns
     * true if the new cursor position is valid or cursor position is at
     * the end of input.
     */
  boolean move1 (int index);	// I cannot think of a better name for this.

    /**
     * Returns true if the most recent move() operation placed the cursor
     * position at a valid position in the input.
     */
  boolean isValid ();

    /**
     * Returns another CharIndexed containing length characters to the left
     * of the given index. The given length is an expected maximum and
     * the returned CharIndexed may not necessarily contain so many characters.
     */
  CharIndexed lookBehind (int index, int length);

    /**
     * Returns the effective length of this CharIndexed
     */
  int length ();

    /**
     * Sets the REMatch last found on this input.
     */
  void setLastMatch (REMatch match);

    /**
     * Returns the REMatch last found on this input.
     */
  REMatch getLastMatch ();

    /**
     * Sets the information used for hitEnd().
     */
  void setHitEnd (REMatch match);

    /**
     * Returns whether the matcher has hit the end of input.
     */
  boolean hitEnd ();

    /**
     * Returns the anchor.
     */
  int getAnchor ();

    /**
     * Sets the anchor.
     */
  void setAnchor (int anchor);
}
