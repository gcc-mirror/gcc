/* gnu/regexp/REToken.java
   Copyright (C) 1998-2001, 2004 Free Software Foundation, Inc.

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

package gnu.regexp;
import java.io.Serializable;

abstract class REToken implements Serializable {

  protected REToken next = null;
  protected REToken uncle = null;
  protected int subIndex;

  protected REToken(int subIndex) {
      this.subIndex = subIndex;
  }

  int getMinimumLength() {
    return 0;
  }

  void setUncle(REToken anUncle) {
    uncle = anUncle;
  }

    /** Returns true if the match succeeded, false if it failed. */
    abstract boolean match(CharIndexed input, REMatch mymatch);
  
    /** Returns true if the rest of the tokens match, false if they fail. */
    protected boolean next(CharIndexed input, REMatch mymatch) {
	if (next == null) {
	    if (uncle == null) {
		return true;
	    } else {
		return uncle.match(input, mymatch);
	    }
	} else {
	    return next.match(input, mymatch);
	}
    }
  
  boolean chain(REToken token) {
      next = token;
      return true; // Token was accepted
  }

    abstract void dump(StringBuffer os);

  void dumpAll(StringBuffer os) {
    dump(os);
    if (next != null) next.dumpAll(os);
  }
}
