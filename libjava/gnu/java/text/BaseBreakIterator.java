/* BaseBreakIterator.java -- Base class for default BreakIterators
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package gnu.java.text;

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
    // Go past the last character.
    iter.next();
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
