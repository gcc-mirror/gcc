/*  gnu/regexp/RETokenStart.java
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

class RETokenStart extends REToken {
    private String newline; // matches after a newline
    private boolean check_java_line_terminators;
    
    RETokenStart(int subIndex, String newline) {
	super(subIndex);
	this.newline = newline;
	this.check_java_line_terminators = false;
    }

    RETokenStart(int subIndex, String newline, boolean b) {
    	super(subIndex);
        this.newline = newline;
        this.check_java_line_terminators = b;
    }

    int getMaximumLength() {
        return 0;
    }
    
    REMatch matchThis(CharIndexed input, REMatch mymatch) {
	// charAt(index-n) may be unknown on a Reader/InputStream. FIXME
	// Match after a newline if in multiline mode
	
	if (check_java_line_terminators) {
	    char ch = input.charAt(mymatch.index - 1);
	    if (ch != CharIndexed.OUT_OF_BOUNDS) {
		if (ch == '\n') return mymatch;
		if (ch == '\r') {
		    char ch1 = input.charAt(mymatch.index);
		    if (ch1 != '\n') return mymatch;
		    return null;
		}
		if (ch == '\u0085') return mymatch; // A next-line character
		if (ch == '\u2028') return mymatch; // A line-separator character
		if (ch == '\u2029') return mymatch; // A paragraph-separator character
	    }
	}

	if (newline != null) {
	    int len = newline.length();
	    if (mymatch.offset >= len) {
		boolean found = true;
		char z;
		int i = 0; // position in REToken.newline
		char ch = input.charAt(mymatch.index - len);
		do {
		    z = newline.charAt(i);
		    if (ch != z) {
			found = false;
			break;
		    }
		    ++i;
		    ch = input.charAt(mymatch.index - len + i);
		} while (i < len);
	    
		if (found) return mymatch;
	    }
	}
	
	// Don't match at all if REG_NOTBOL is set.
	if ((mymatch.eflags & RE.REG_NOTBOL) > 0) return null;
	
	if ((mymatch.eflags & RE.REG_ANCHORINDEX) > 0)
	    return (mymatch.anchor == mymatch.offset) ? 
		mymatch : null;
	else
	    return ((mymatch.index == 0) && (mymatch.offset == 0)) ?
		mymatch : null;
    }

    boolean returnsFixedLengthmatches() { return true; }

    int findFixedLengthMatches(CharIndexed input, REMatch mymatch, int max) {
        if (matchThis(input, mymatch) != null) return max;
	else return 0;
    }
    
    void dump(StringBuffer os) {
	os.append('^');
    }
}
