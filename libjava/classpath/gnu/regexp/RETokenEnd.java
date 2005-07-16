/* gnu/regexp/RETokenEnd.java
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

final class RETokenEnd extends REToken {
    /**
     * Indicates whether this token should match on a line break.
     */
  private String newline;

  RETokenEnd(int subIndex,String newline) { 
    super(subIndex);
    this.newline = newline;
  }

    boolean match(CharIndexed input, REMatch mymatch) {
	char ch = input.charAt(mymatch.index);
	if (ch == CharIndexed.OUT_OF_BOUNDS)
	    return ((mymatch.eflags & RE.REG_NOTEOL)>0) ? 
		false : next(input, mymatch);
	if (newline != null) {
	    char z;
	    int i = 0; // position in newline
	    do {
		z = newline.charAt(i);
		if (ch != z) return false;
		++i;
		ch = input.charAt(mymatch.index + i);
	    } while (i < newline.length());
	    
	    return next(input, mymatch);
	}
	return false;
    }

  void dump(StringBuffer os) {
    os.append('$');
  }
}
