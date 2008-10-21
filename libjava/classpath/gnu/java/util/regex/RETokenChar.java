/* gnu/regexp/RETokenChar.java
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

final class RETokenChar extends REToken
{
  private char[] ch;
  private boolean insens;

    RETokenChar (int subIndex, char c, boolean ins)
  {
    super (subIndex);
    insens = ins;
    ch = new char[1];
      ch[0] = c;
  }

  int getMinimumLength ()
  {
    return ch.length;
  }

  int getMaximumLength ()
  {
    return ch.length;
  }

  REMatch matchThis (CharIndexed input, REMatch mymatch)
  {
    if (matchOneString (input, mymatch.index))
      {
	mymatch.index += matchedLength;
	return mymatch;
      }
    // java.util.regex.Matcher#hitEnd() requires that the length of
    // partial match be counted.
    mymatch.index += matchedLength;
    input.setHitEnd (mymatch);
    return null;
  }

  private int matchedLength;
  private boolean matchOneString (CharIndexed input, int index)
  {
    matchedLength = 0;
    int z = ch.length;
    char c;
    for (int i = 0; i < z; i++)
      {
	c = input.charAt (index + i);
	if (!charEquals (c, ch[i]))
	  {
	    return false;
	  }
	++matchedLength;
      }
    return true;
  }

  private boolean charEquals (char c1, char c2)
  {
    if (c1 == c2)
      return true;
    if (!insens)
      return false;
    if (toLowerCase (c1, unicodeAware) == c2)
      return true;
    if (toUpperCase (c1, unicodeAware) == c2)
      return true;
    return false;
  }

  boolean returnsFixedLengthMatches ()
  {
    return true;
  }

  int findFixedLengthMatches (CharIndexed input, REMatch mymatch, int max)
  {
    int index = mymatch.index;
    int numRepeats = 0;
    int z = ch.length;
    while (true)
      {
	if (numRepeats >= max)
	  break;
	if (matchOneString (input, index))
	  {
	    index += z;
	    numRepeats++;
	  }
	else
	  break;
      }
    return numRepeats;
  }

  // Overrides REToken.chain() to optimize for strings
  boolean chain (REToken next)
  {
    if (next instanceof RETokenChar && ((RETokenChar) next).insens == insens)
      {
	RETokenChar cnext = (RETokenChar) next;
	int newsize = ch.length + cnext.ch.length;

	char[] chTemp = new char[newsize];

	System.arraycopy (ch, 0, chTemp, 0, ch.length);
	System.arraycopy (cnext.ch, 0, chTemp, ch.length, cnext.ch.length);

	ch = chTemp;
	if (cnext.next == null)
	  return false;
	return chain (cnext.next);
      }
    else
      return super.chain (next);
  }

  void dump (CPStringBuilder os)
  {
    os.append (ch);
  }
}
