/* gnu/regexp/RETokenWordBoundary.java
   Copyright (C) 2001, 2004 Free Software Foundation, Inc.

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

/**
 * Represents a combination lookahead/lookbehind for POSIX [:alnum:].
 */
final class RETokenWordBoundary extends REToken {
    private boolean negated;
    private int where;
    static final int BEGIN = 1;
    static final int END = 2;

    RETokenWordBoundary(int subIndex, int where, boolean negated) {
	super(subIndex);
	this.where = where;
	this.negated = negated;
    }
    
    boolean match(CharIndexed input, REMatch mymatch) {
	// Word boundary means input[index-1] was a word character
	// and input[index] is not, or input[index] is a word character
	// and input[index-1] was not
	//  In the string "one two three", these positions match:
	//  |o|n|e| |t|w|o| |t|h|r|e|e|
	//  ^     ^ ^     ^ ^         ^
	boolean after = false;  // is current character a letter or digit?
	boolean before = false; // is previous character a letter or digit?
	char ch;

	// TODO: Also check REG_ANCHORINDEX vs. anchor
	if (((mymatch.eflags & RE.REG_ANCHORINDEX) != RE.REG_ANCHORINDEX) 
	    || (mymatch.offset + mymatch.index > mymatch.anchor)) {
	    if ((ch = input.charAt(mymatch.index - 1)) != CharIndexed.OUT_OF_BOUNDS) {
		before = Character.isLetterOrDigit(ch) || (ch == '_');
	    }
	}

	if ((ch = input.charAt(mymatch.index)) != CharIndexed.OUT_OF_BOUNDS) {
	    after = Character.isLetterOrDigit(ch) || (ch == '_');
	}

	// if (before) and (!after), we're at end (\>)
	// if (after) and (!before), we're at beginning (\<)
	boolean doNext = false;

	if ((where & BEGIN) == BEGIN) {
	    doNext = after && !before;
	}
	if ((where & END) == END) {
	    doNext ^= before && !after;
	}

	if (negated) doNext = !doNext;

	return (doNext ? next(input, mymatch) : false);
    }
    
    void dump(StringBuffer os) {
	if (where == (BEGIN | END)) {
	    os.append( negated ? "\\B" : "\\b" );
	} else if (where == BEGIN) {
	    os.append("\\<");
	} else {
	    os.append("\\>");
	}
    }
}
