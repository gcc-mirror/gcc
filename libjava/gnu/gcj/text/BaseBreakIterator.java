// Base class for default BreakIterators.

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
 */

public abstract class BaseBreakIterator extends BreakIterator
{
  public int current ()
  {
    return iter.getIndex();
  }

  public int first ()
  {
    iter.first();
    return iter.getBeginIndex();
  }

  public int following (int pos)
  {
    int save = iter.getIndex();
    iter.setIndex(pos);
    int r = next ();
    iter.setIndex(save);
    return r;
  }

  public CharacterIterator getText ()
  {
    return iter;
  }

  public int last ()
  {
    iter.last();
    return iter.getEndIndex();
  }

  public int next (int n)
  {
    int r = iter.getIndex ();
    if (n > 0)
      {
	while (n > 0 && r != DONE)
	  {
	    r = next ();
	    --n;
	  }
      }
    else if (n < 0)
      {
	while (n < 0 && r != DONE)
	  {
	    r = previous ();
	    ++n;
	  }
      }
    return r;
  }

  public void setText (CharacterIterator newText)
  {
    iter = newText;
  }

  protected CharacterIterator iter;
}
