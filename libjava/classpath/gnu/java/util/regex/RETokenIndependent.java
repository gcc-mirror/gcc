/* gnu/regexp/RETokenIndependent.java
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
final class RETokenIndependent extends REToken
{
  REToken re;

    RETokenIndependent (REToken re) throws REException
  {
    super (0);
    this.re = re;
  }

  int getMinimumLength ()
  {
    return re.getMinimumLength ();
  }

  int getMaximumLength ()
  {
    return re.getMaximumLength ();
  }

  REMatch matchThis (CharIndexed input, REMatch mymatch)
  {
    boolean b = re.match (input, mymatch);
    if (b)
      {
	// Once we have found a match, we do not see other possible matches.
	if (mymatch.backtrackStack != null)
	  mymatch.backtrackStack.clear ();
	return mymatch;

      }
    return null;
  }

  void dump (CPStringBuilder os)
  {
    os.append ("(?>");
    re.dumpAll (os);
    os.append (')');
  }
}
