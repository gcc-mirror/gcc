// Default word BreakIterator.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.text;

import java.text.BreakIterator;
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
    iter = null;
  }

  private LineBreakIterator (LineBreakIterator other)
  {
    iter = (CharacterIterator) other.iter.clone();
  }

  // Some methods to tell us different properties of characters.
  private final boolean isNb (char c)
  {
    return (c == 0x00a0		// NO-BREAK SPACE
	    || c == 0x2011	// NON-BREAKING HYPHEN
	    || c == 0xfeff);	// ZERO WITH NO-BREAK SPACE
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
    return (c >= 0x3040 && c <= 0x309f	       // Hiragana
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
