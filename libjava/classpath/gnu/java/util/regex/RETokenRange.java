/* gnu/regexp/RETokenRange.java
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

final class RETokenRange extends REToken
{
  private char lo, hi;
  private boolean insens;

    RETokenRange (int subIndex, char lo, char hi, boolean ins)
  {
    super (subIndex);
    insens = ins;
    this.lo = lo;
    this.hi = hi;
  }

  int getMinimumLength ()
  {
    return 1;
  }

  int getMaximumLength ()
  {
    return 1;
  }

  REMatch matchThis (CharIndexed input, REMatch mymatch)
  {
    char c = input.charAt (mymatch.index);
    if (matchOneChar (c))
      {
	++mymatch.index;
	return mymatch;
      }
    return null;
  }

  boolean matchOneChar (char c)
  {
    if (c == CharIndexed.OUT_OF_BOUNDS)
      return false;
    boolean matches = (c >= lo) && (c <= hi);
    if (!matches && insens)
      {
	char c1 = toLowerCase (c, unicodeAware);
	matches = (c1 >= lo) && (c1 <= hi);
	if (!matches)
	  {
	    c1 = toUpperCase (c, unicodeAware);
	    matches = (c1 >= lo) && (c1 <= hi);
	  }
      }
    return matches;
  }

  boolean returnsFixedLengthMatches ()
  {
    return true;
  }

  int findFixedLengthMatches (CharIndexed input, REMatch mymatch, int max)
  {
    int index = mymatch.index;
    int numRepeats = 0;
    while (true)
      {
	if (numRepeats >= max)
	  break;
	char ch = input.charAt (index++);
	if (!matchOneChar (ch))
	  break;
	numRepeats++;
      }
    return numRepeats;
  }

  void dump (CPStringBuilder os)
  {
    os.append (lo).append ('-').append (hi);
  }
}
