// Default sentence BreakIterator.

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
 * @date March 23, 1999
 * Written using The Unicode Standard, Version 2.0.
 */

public class SentenceBreakIterator extends BaseBreakIterator
{
  public Object clone ()
  {
    return new SentenceBreakIterator (this);
  }

  public SentenceBreakIterator ()
  {
    iter = null;
  }

  private SentenceBreakIterator (SentenceBreakIterator other)
  {
    iter = (CharacterIterator) other.iter.clone();
  }

  public int next ()
  {
    int end = iter.getEndIndex();
    if (iter.getIndex() == end)
      return DONE;

    while (iter.getIndex() < end)
      {
	char c = iter.current();
	if (c == CharacterIterator.DONE)
	  break;
	int type = Character.getType(c);

	char n = iter.next();
	if (n == CharacterIterator.DONE)
	  break;

	// Always break after paragraph separator.
	if (type == Character.PARAGRAPH_SEPARATOR)
	  break;

	if (c == '!' || c == '?')
	  {
	    // Skip close punctuation.
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.END_PUNCTUATION)
	      n = iter.next();
	    // Skip spaces.
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.SPACE_SEPARATOR)
	      n = iter.next();
	    // Skip optional paragraph separator.
	    if (n != CharacterIterator.DONE
		&& Character.getType(n) == Character.PARAGRAPH_SEPARATOR)
	      n = iter.next();

	    // There's always a break somewhere after `!' or `?'.
	    break;
	  }

	if (c == '.')
	  {
	    int save = iter.getIndex();
	    // Skip close punctuation.
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.END_PUNCTUATION)
	      n = iter.next();
	    // Skip spaces.  We keep count because we need at least
	    // one for this period to represent a terminator.
	    int spcount = 0;
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.SPACE_SEPARATOR)
	      {
		n = iter.next();
		++spcount;
	      }
	    if (spcount > 0)
	      {
		int save2 = iter.getIndex();
		// Skip over open puncutation.
		while (n != CharacterIterator.DONE
		       && Character.getType(n) == Character.START_PUNCTUATION)
		  n = iter.next();
		// Next character must not be lower case.
		if (n == CharacterIterator.DONE
		    || ! Character.isLowerCase(n))
		  {
		    iter.setIndex(save2);
		    break;
		  }
	      }
	    iter.setIndex(save);
	  }
      }

    return iter.getIndex();
  }

  private final int previous_internal ()
  {
    int start = iter.getBeginIndex();
    if (iter.getIndex() == start)
      return DONE;

    while (iter.getIndex() >= start)
      {
	char c = iter.previous();
	if (c == CharacterIterator.DONE)
	  break;

	char n = iter.previous();
	if (n == CharacterIterator.DONE)
	  break;
	iter.next();
	int nt = Character.getType(n);

	if (! Character.isLowerCase(c)
	    && (nt == Character.START_PUNCTUATION
		|| nt == Character.SPACE_SEPARATOR))
	  {
	    int save = iter.getIndex();
	    int save_nt = nt;
	    char save_n = n;
	    // Skip open punctuation.
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.START_PUNCTUATION)
	      n = iter.previous();
	    if (n == CharacterIterator.DONE)
	      break;
	    if (Character.getType(n) == Character.SPACE_SEPARATOR)
	      {
		// Must have at least once space after the `.'.
		int save2 = iter.getIndex();
		while (n != CharacterIterator.DONE
		       && Character.getType(n) == Character.SPACE_SEPARATOR)
		  n = iter.previous();
		// Skip close punctuation.
		while (n != CharacterIterator.DONE
		       && Character.getType(n) == Character.END_PUNCTUATION)
		  n = iter.previous();
		if (n == CharacterIterator.DONE || n == '.')
		  {
		    // Communicate location of actual end.
		    period = iter.getIndex();
		    iter.setIndex(save2);
		    break;
		  }
	      }
	    iter.setIndex(save);
	    nt = save_nt;
	    n = save_n;
	  }

	if (nt == Character.PARAGRAPH_SEPARATOR)
	  {
	    // Communicate location of actual end.
	    period = iter.getIndex();
	    break;
	  }
	else if (nt == Character.SPACE_SEPARATOR
		 || nt == Character.END_PUNCTUATION)
	  {
	    int save = iter.getIndex();
	    // Skip spaces.
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.SPACE_SEPARATOR)
	      n = iter.previous();
	    // Skip close punctuation.
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.END_PUNCTUATION)
	      n = iter.previous();
	    int here = iter.getIndex();
	    iter.setIndex(save);
	    if (n == CharacterIterator.DONE || n == '!' || n == '?')
	      {
		// Communicate location of actual end.
		period = here;
		break;
	      }
	  }
	else if (n == '!' || n == '?')
	  {
	    // Communicate location of actual end.
	    period = iter.getIndex();
	    break;
	  }
      }

    return iter.getIndex();
  }

  public int previous ()
  {
    // We want to skip over the first sentence end to the second one.
    // However, at the end of the string we want the first end.
    int here = iter.getIndex();
    period = here;
    int first = previous_internal ();
    if (here == iter.getEndIndex() || first == DONE)
      return first;
    iter.setIndex(period);
    return previous_internal ();
  }

  // This is used for communication between previous and
  // previous_internal.
  private int period;
}
