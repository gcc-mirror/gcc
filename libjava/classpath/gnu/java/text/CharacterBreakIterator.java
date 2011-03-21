/* CharacterBreakIterator.java - Default character BreakIterator.
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
 * @date March 19, 1999
 * Written using The Unicode Standard, Version 2.0.
 */

public class CharacterBreakIterator extends BaseBreakIterator
{
  // Hangul Jamo constants from Unicode book.
  private static final int LBase = 0x1100;
  private static final int VBase = 0x1161;
  private static final int TBase = 0x11a7;
  private static final int LCount = 19;
  private static final int VCount = 21;
  private static final int TCount = 28;

  // Information about surrogates.
  private static final int highSurrogateStart = 0xD800;
  private static final int highSurrogateEnd = 0xDBFF;
  private static final int lowSurrogateStart = 0xDC00;
  private static final int lowSurrogateEnd = 0xDFFF;

  public Object clone ()
  {
    return new CharacterBreakIterator (this);
  }

  public CharacterBreakIterator ()
  {
  }

  private CharacterBreakIterator (CharacterBreakIterator other)
  {
    iter = (CharacterIterator) other.iter.clone();
  }

  // Some methods to tell us different properties of characters.
  private final boolean isL (char c)
  {
    return c >= LBase && c <= LBase + LCount;
  }
  private final boolean isV (char c)
  {
    return c >= VBase && c <= VBase + VCount;
  }
  private final boolean isT (char c)
  {
    return c >= TBase && c <= TBase + TCount;
  }
  private final boolean isLVT (char c)
  {
    return isL (c) || isV (c) || isT (c);
  }
  private final boolean isHighSurrogate (char c)
  {
    return c >= highSurrogateStart && c <= highSurrogateEnd;
  }
  private final boolean isLowSurrogate (char c)
  {
    return c >= lowSurrogateStart && c <= lowSurrogateEnd;
  }

  public int next ()
  {
    int end = iter.getEndIndex();
    if (iter.getIndex() == end)
      return DONE;

    char c;
    for (char prev = CharacterIterator.DONE; iter.getIndex() < end; prev = c)
      {
        c = iter.next();
        if (c == CharacterIterator.DONE)
          break;
        int type = Character.getType(c);

        // Break after paragraph separators.
        if (type == Character.PARAGRAPH_SEPARATOR)
          break;

        // Now we need some lookahead.
        char ahead = iter.next();
        iter.previous();
        if (ahead == CharacterIterator.DONE)
          break;
        int aheadType = Character.getType(ahead);

        if (aheadType != Character.NON_SPACING_MARK
            && ! isLowSurrogate (ahead)
            && ! isLVT (ahead))
          break;
        if (! isLVT (c) && isLVT (ahead))
          break;
        if (isL (c) && ! isLVT (ahead)
            && aheadType != Character.NON_SPACING_MARK)
          break;
        if (isV (c) && ! isV (ahead) && !isT (ahead)
            && aheadType != Character.NON_SPACING_MARK)
          break;
        if (isT (c) && ! isT (ahead)
            && aheadType != Character.NON_SPACING_MARK)
          break;

        if (! isHighSurrogate (c) && isLowSurrogate (ahead))
          break;
        if (isHighSurrogate (c) && ! isLowSurrogate (ahead))
          break;
        if (! isHighSurrogate (prev) && isLowSurrogate (c))
          break;
      }

    return iter.getIndex();
  }

  public int previous ()
  {
    if (iter.getIndex() == iter.getBeginIndex())
      return DONE;

    while (iter.getIndex() >= iter.getBeginIndex())
      {
        char c = iter.previous();
        if (c == CharacterIterator.DONE)
          break;
        int type = Character.getType(c);

        if (type != Character.NON_SPACING_MARK
            && ! isLowSurrogate (c)
            && ! isLVT (c))
          break;

        // Now we need some lookahead.
        char ahead = iter.previous();
        if (ahead == CharacterIterator.DONE)
          {
            iter.next();
            break;
          }
        char ahead2 = iter.previous();
        iter.next();
        iter.next();
        if (ahead2 == CharacterIterator.DONE)
          break;
        int aheadType = Character.getType(ahead);

        if (aheadType == Character.PARAGRAPH_SEPARATOR)
          break;

        if (isLVT (c) && ! isLVT (ahead))
          break;
        if (! isLVT (c) && type != Character.NON_SPACING_MARK
            && isL (ahead))
          break;
        if (! isV (c) && ! isT (c) && type != Character.NON_SPACING_MARK
            && isV (ahead))
          break;
        if (! isT (c) && type != Character.NON_SPACING_MARK
            && isT (ahead))
          break;

        if (isLowSurrogate (c) && ! isHighSurrogate (ahead))
          break;
        if (! isLowSurrogate (c) && isHighSurrogate (ahead))
          break;
        if (isLowSurrogate (ahead) && ! isHighSurrogate (ahead2))
          break;
      }

    return iter.getIndex();
  }
}
