// Default character BreakIterator.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.text;

import java.text.BreakIterator;
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
    iter = null;		// FIXME?
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

    int start = iter.getBeginIndex();
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
