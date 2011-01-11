/* gnu/regexp/RETokenEnd.java
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

final class RETokenEnd extends REToken
{
    /**
     * Indicates whether this token should match on a line break.
     */
  private String newline;
  private boolean check_java_line_terminators;

  /**
   * Indicates whether this token is a real one generated at compile time,
   * or a fake one temporarily added by RE#getMatchImpl.
   */
  private boolean fake = false;

    RETokenEnd (int subIndex, String newline)
  {
    super (subIndex);
    this.newline = newline;
    this.check_java_line_terminators = false;
  }

  RETokenEnd (int subIndex, String newline, boolean b)
  {
    super (subIndex);
    this.newline = newline;
    this.check_java_line_terminators = b;
  }

  void setFake (boolean fake)
  {
    this.fake = fake;
  }

  int getMaximumLength ()
  {
    return 0;
  }

  boolean match (CharIndexed input, REMatch mymatch)
  {
    if (!fake)
      return super.match (input, mymatch);
    return super.matchFake (input, mymatch);
  }

  REMatch matchThis (CharIndexed input, REMatch mymatch)
  {
    char ch = input.charAt (mymatch.index);
    if (ch == CharIndexed.OUT_OF_BOUNDS)
      return ((mymatch.eflags & RE.REG_NOTEOL) > 0) ? null : mymatch;
    if (check_java_line_terminators)
      {
        if (ch == '\n')
          {
            char ch1 = input.charAt (mymatch.index - 1);
            if (ch1 == '\r')
              return null;
            return mymatch;
          }
        if (ch == '\r')
          return mymatch;
        if (ch == '\u0085')
          return mymatch;       // A next-line character
        if (ch == '\u2028')
          return mymatch;       // A line-separator character
        if (ch == '\u2029')
          return mymatch;       // A paragraph-separator character
        return null;
      }
    if (newline != null)
      {
        char z;
        int i = 0;              // position in newline
        do
          {
            z = newline.charAt (i);
            if (ch != z)
              return null;
            ++i;
            ch = input.charAt (mymatch.index + i);
          }
        while (i < newline.length ());

        return mymatch;
      }
    return null;
  }

  boolean returnsFixedLengthMatches ()
  {
    return true;
  }

  int findFixedLengthMatches (CharIndexed input, REMatch mymatch, int max)
  {
    REMatch m = (REMatch) mymatch.clone ();
    REToken tk = (REToken) this.clone ();
    tk.chain (null);
    if (tk.match (input, m))
      return max;
    else
      return 0;
  }

  void dump (CPStringBuilder os)
  {
    os.append ('$');
  }
}
