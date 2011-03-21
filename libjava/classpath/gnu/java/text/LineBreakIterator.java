/* LineBreakIterator.java - Default word BreakIterator.
   Copyright (C) 1999, 2001, 2004 Free Software Foundation, Inc.

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


package gnu.java.text;

import java.text.CharacterIterator;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 22, 1999
 * Written using The Unicode Standard, Version 2.0.
 */

public class LineBreakIterator extends BaseBreakIterator
{
  public Object clone ()
  {
    return new LineBreakIterator (this);
  }

  public LineBreakIterator ()
  {
  }

  private LineBreakIterator (LineBreakIterator other)
  {
    iter = (CharacterIterator) other.iter.clone();
  }

  // Some methods to tell us different properties of characters.
  private final boolean isNb (char c)
  {
    return (c == 0x00a0         // NO-BREAK SPACE
            || c == 0x2011      // NON-BREAKING HYPHEN
            || c == 0xfeff);    // ZERO WITH NO-BREAK SPACE
  }
  private final boolean isClose (int type)
  {
    return (type == Character.END_PUNCTUATION
            // Unicode book says "comma, period, ...", which I take to
            // mean "Po" class.
            || type == Character.OTHER_PUNCTUATION);
  }
  private final boolean isIdeo (char c)
  {
    return (c >= 0x3040 && c <= 0x309f         // Hiragana
            || c >= 0x30a0 && c <= 0x30ff      // Katakana
            || c >= 0x4e00 && c <= 0x9fff      // Han
            || c >= 0x3100 && c <= 0x312f);    // Bopomofo
  }

  public int next ()
  {
    int end = iter.getEndIndex();
    if (iter.getIndex() == end)
      return DONE;

    while (iter.getIndex() < end)
      {
        char c = iter.current();
        int type = Character.getType(c);

        char n = iter.next();

        if (n == CharacterIterator.DONE
            || type == Character.PARAGRAPH_SEPARATOR
            || type == Character.LINE_SEPARATOR)
          break;

        // Handle two cases where we must scan for non-spacing marks.
        int start = iter.getIndex();
        if (type == Character.SPACE_SEPARATOR
            || type == Character.START_PUNCTUATION
            || isIdeo (c))
          {
            while (n != CharacterIterator.DONE
                   && Character.getType(n) == Character.NON_SPACING_MARK)
              n = iter.next();
            if (n == CharacterIterator.DONE)
              break;

            if (type == Character.SPACE_SEPARATOR)
              {
                int nt = Character.getType(n);
                if (nt != Character.NON_SPACING_MARK
                    && nt != Character.SPACE_SEPARATOR
                    && ! isNb (n))
                  break;
              }
            else if (type == Character.START_PUNCTUATION)
              {
                if (isIdeo (n))
                  {
                    // Open punctuation followed by non spacing marks
                    // and then ideograph does not have a break in
                    // it.  So skip all this.
                    start = iter.getIndex();
                  }
              }
            else
              {
                // Ideograph preceded this character.
                if (isClose (Character.getType(n)))
                  break;
              }
          }
        iter.setIndex(start);
      }

    return iter.getIndex();
  }

  public int previous ()
  {
    int start = iter.getBeginIndex();
    if (iter.getIndex() == start)
      return DONE;

    while (iter.getIndex() >= start)
      {
        char c = iter.previous();
        if (c == CharacterIterator.DONE)
          break;
        int type = Character.getType(c);

        char n = iter.previous();
        if (n == CharacterIterator.DONE)
          break;
        iter.next();

        int nt = Character.getType(n);
        // Break after paragraph separators.
        if (nt == Character.PARAGRAPH_SEPARATOR
            || nt == Character.LINE_SEPARATOR)
          break;

        // Skip non-spacing marks.
        int init = iter.getIndex();
        while (n != CharacterIterator.DONE && nt == Character.NON_SPACING_MARK)
          {
            n = iter.previous();
            nt = Character.getType(n);
          }

        if (nt == Character.SPACE_SEPARATOR
            && type != Character.SPACE_SEPARATOR
            && type != Character.NON_SPACING_MARK
            && ! isNb (c))
          break;
        if (! isClose (type) && isIdeo (n))
          break;
        if (isIdeo (c) && nt != Character.START_PUNCTUATION)
          break;
        iter.setIndex(init);
      }

    return iter.getIndex();
  }
}
