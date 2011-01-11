/* gnu/regexp/RETokenLookBehind.java
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

/**
 * @author Ito Kazumitsu
 */
final class RETokenLookBehind extends REToken
{
  REToken re;
  boolean negative;

    RETokenLookBehind (REToken re, boolean negative) throws REException
  {
    super (0);
    this.re = re;
    this.negative = negative;
  }

  int getMaximumLength ()
  {
    return 0;
  }

  REMatch matchThis (CharIndexed input, REMatch mymatch)
  {
    int max = re.getMaximumLength ();
    CharIndexed behind = input.lookBehind (mymatch.index, max);
    REMatch trymatch = (REMatch) mymatch.clone ();
    int diff = behind.length () - input.length ();
    int curIndex = trymatch.index + diff;
    trymatch.index = 0;
    trymatch.offset = 0;
    RETokenMatchHereOnly stopper = new RETokenMatchHereOnly (curIndex);
    REToken re1 = (REToken) re.clone ();
    re1.chain (stopper);
    if (re1.match (behind, trymatch))
      {
        if (negative)
          return null;
        for (int i = 0; i < trymatch.start.length; i++)
          {
            if (trymatch.start[i] != -1 && trymatch.end[i] != -1)
              {
                trymatch.start[i] -= diff;
                if (trymatch.start[i] < 0)
                  trymatch.start[i] -= 1;
                trymatch.end[i] -= diff;
                if (trymatch.end[i] < 0)
                  trymatch.end[i] -= 1;
              }
          }
        trymatch.index = mymatch.index;
        trymatch.offset = mymatch.offset;
        return trymatch;
      }
    else
      {
        if (negative)
          return mymatch;
        return null;
      }
  }

  void dump (CPStringBuilder os)
  {
    os.append ("(?<");
    os.append (negative ? '!' : '=');
    re.dumpAll (os);
    os.append (')');
  }

  private static class RETokenMatchHereOnly extends REToken
  {

    int getMaximumLength ()
    {
      return 0;
    }

    private int index;

    RETokenMatchHereOnly (int index)
    {
      super (0);
      this.index = index;
    }

    REMatch matchThis (CharIndexed input, REMatch mymatch)
    {
      return (index == mymatch.index ? mymatch : null);
    }

    void dump (CPStringBuilder os)
    {
    }

  }
}
