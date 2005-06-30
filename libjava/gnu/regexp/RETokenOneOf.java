/* gnu/regexp/RETokenOneOf.java
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
import java.util.Vector;

final class RETokenOneOf extends REToken {
  private Vector options;
  private boolean negative;

  // This constructor is used for convenience when we know the set beforehand,
  // e.g. \d --> new RETokenOneOf("0123456789",false, ..)
  //      \D --> new RETokenOneOf("0123456789",true, ..)

  RETokenOneOf(int subIndex, String optionsStr, boolean negative, boolean insens) {
    super(subIndex);
    options = new Vector();
    this.negative = negative;
    for (int i = 0; i < optionsStr.length(); i++)
      options.addElement(new RETokenChar(subIndex,optionsStr.charAt(i),insens));
  }

  RETokenOneOf(int subIndex, Vector options, boolean negative) {
    super(subIndex);
    this.options = options;
    this.negative = negative;
  }

  int getMinimumLength() {
    int min = Integer.MAX_VALUE;
    int x;
    for (int i=0; i < options.size(); i++) {
      if ((x = ((REToken) options.elementAt(i)).getMinimumLength()) < min)
	min = x;
    }
    return min;
  }

    boolean match(CharIndexed input, REMatch mymatch) {
    if (negative && (input.charAt(mymatch.index) == CharIndexed.OUT_OF_BOUNDS)) 
      return false;

    REMatch newMatch = null;
    REMatch last = null;
    REToken tk;
    boolean isMatch;
    for (int i=0; i < options.size(); i++) {
	tk = (REToken) options.elementAt(i);
	REMatch tryMatch = (REMatch) mymatch.clone();
	if (tk.match(input, tryMatch)) { // match was successful
	    if (negative) return false;

	    if (next(input, tryMatch)) {
		// Add tryMatch to list of possibilities.
		if (last == null) {
		    newMatch = tryMatch;
		    last = tryMatch;
		} else {
		    last.next = tryMatch;
		    last = tryMatch;
		}
	    } // next succeeds
	} // is a match
    } // try next option

    if (newMatch != null) {
	if (negative) {
	    return false;
	} else {
	    // set contents of mymatch equal to newMatch

	    // try each one that matched
	    mymatch.assignFrom(newMatch);
	    return true;
	}
    } else {
	if (negative) {
	    ++mymatch.index;
	    return next(input, mymatch);
	} else {
	    return false;
	}
    }

    // index+1 works for [^abc] lists, not for generic lookahead (--> index)
  }

  void dump(StringBuffer os) {
    os.append(negative ? "[^" : "(?:");
    for (int i = 0; i < options.size(); i++) {
      if (!negative && (i > 0)) os.append('|');
      ((REToken) options.elementAt(i)).dumpAll(os);
    }
    os.append(negative ? ']' : ')');
  }  
}
