/* gnu/regexp/REMatch.java
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

/**
 * An instance of this class represents a match
 * completed by a gnu.regexp matching function. It can be used
 * to obtain relevant information about the location of a match
 * or submatch.
 *
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A>
 */
public final class REMatch implements Serializable, Cloneable {
    private String matchedText;

    // These variables are package scope for fast access within the engine
    int eflags; // execution flags this match was made using

    // Offset in source text where match was tried.  This is zero-based;
    // the actual position in the source text is given by (offset + anchor).
    int offset;

    // Anchor position refers to the index into the source input
    // at which the matching operation began.
    // This is also useful for the ANCHORINDEX option.
    int anchor;

    // Package scope; used by RE.
    int index; // used while matching to mark current match position in input
    int[] start; // start positions (relative to offset) for each (sub)exp.
    int[] end;   // end positions for the same
    REMatch next; // other possibility (to avoid having to use arrays)

    public Object clone() {
	try {
	    REMatch copy = (REMatch) super.clone();
	    copy.next = null;

	    copy.start = (int[]) start.clone();
	    copy.end = (int[]) end.clone();

	    return copy;
	} catch (CloneNotSupportedException e) {
	    throw new Error(); // doesn't happen
	}
    }

    void assignFrom(REMatch other) {
	start = other.start;
	end = other.end;
	index = other.index;
	// need to deep clone?
	next = other.next;
    }

    REMatch(int subs, int anchor, int eflags) {
	start = new int[subs+1];
	end = new int[subs+1];
	this.anchor = anchor;
	this.eflags = eflags;
	clear(anchor);
    }

    void finish(CharIndexed text) {
	start[0] = 0;
	StringBuffer sb = new StringBuffer();
	int i;
	for (i = 0; i < end[0]; i++)
	    sb.append(text.charAt(i));
	matchedText = sb.toString();
	for (i = 0; i < start.length; i++) {
	    // If any subexpressions didn't terminate, they don't count
	    // TODO check if this code ever gets hit
	    if ((start[i] == -1) ^ (end[i] == -1)) {
		start[i] = -1;
		end[i] = -1;
	    }
	}
	next = null; // cut off alternates
    }
    
    /** Clears the current match and moves the offset to the new index. */
    void clear(int index) {
	offset = index;
	this.index = 0;
	for (int i = 0; i < start.length; i++) {
	    start[i] = end[i] = -1;
	}
	next = null; // cut off alternates
    }
    
    /**
     * Returns the string matching the pattern.  This makes it convenient
     * to write code like the following:
     * <P>
     * <code> 
     * REMatch myMatch = myExpression.getMatch(myString);<br>
     * if (myMatch != null) System.out.println("Regexp found: "+myMatch);
     * </code>
     */
    public String toString() {
	return matchedText;
    }
    
    /**
     * Returns the index within the input text where the match in its entirety
     * began.
     */
    public int getStartIndex() {
	return offset + start[0];
    }
    
    /**
     * Returns the index within the input string where the match in
     * its entirety ends.  The return value is the next position after
     * the end of the string; therefore, a match created by the
     * following call:
     *
     * <P>
     * <code>REMatch myMatch = myExpression.getMatch(myString);</code>
     * <P>
     * can be viewed (given that myMatch is not null) by creating
     * <P>
     * <code>String theMatch = myString.substring(myMatch.getStartIndex(),
     * myMatch.getEndIndex());</code>
     * <P>
     * But you can save yourself that work, since the <code>toString()</code>
     * method (above) does exactly that for you.  
     */
    public int getEndIndex() {
	return offset + end[0];
    }
  
    /**
     * Returns the string matching the given subexpression.  The subexpressions
     * are indexed starting with one, not zero.  That is, the subexpression
     * identified by the first set of parentheses in a regular expression
     * could be retrieved from an REMatch by calling match.toString(1).
     *
     * @param sub Index of the subexpression.
     */
    public String toString(int sub) {
	if ((sub >= start.length) || (start[sub] == -1)) return "";
	return (matchedText.substring(start[sub],end[sub]));
    }
    
    /** 
     * Returns the index within the input string used to generate this match
     * where subexpression number <i>sub</i> begins, or <code>-1</code> if
     * the subexpression does not exist.  The initial position is zero.
     *
     * @param sub Subexpression index
     * @deprecated Use getStartIndex(int) instead.
     */
    public int getSubStartIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = start[sub];
	return (x == -1) ? x : offset + x;
    }
    
    /** 
     * Returns the index within the input string used to generate this match
     * where subexpression number <i>sub</i> begins, or <code>-1</code> if
     * the subexpression does not exist.  The initial position is zero.
     *
     * @param sub Subexpression index
     * @since gnu.regexp 1.1.0
     */
    public int getStartIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = start[sub];
	return (x == -1) ? x : offset + x;
    }
  
    /** 
     * Returns the index within the input string used to generate this match
     * where subexpression number <i>sub</i> ends, or <code>-1</code> if
     * the subexpression does not exist.  The initial position is zero.
     *
     * @param sub Subexpression index
     * @deprecated Use getEndIndex(int) instead
     */
    public int getSubEndIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = end[sub];
	return (x == -1) ? x : offset + x;
    }
    
    /** 
     * Returns the index within the input string used to generate this match
     * where subexpression number <i>sub</i> ends, or <code>-1</code> if
     * the subexpression does not exist.  The initial position is zero.
     *
     * @param sub Subexpression index
     */
    public int getEndIndex(int sub) {
	if (sub >= start.length) return -1;
	int x = end[sub];
	return (x == -1) ? x : offset + x;
    }
    
    /**
     * Substitute the results of this match to create a new string.
     * This is patterned after PERL, so the tokens to watch out for are
     * <code>$0</code> through <code>$9</code>.  <code>$0</code> matches
     * the full substring matched; <code>$<i>n</i></code> matches
     * subexpression number <i>n</i>.
     *
     * @param input A string consisting of literals and <code>$<i>n</i></code> tokens.
     */
    public String substituteInto(String input) {
	// a la Perl, $0 is whole thing, $1 - $9 are subexpressions
	StringBuffer output = new StringBuffer();
	int pos;
	for (pos = 0; pos < input.length()-1; pos++) {
	    if ((input.charAt(pos) == '$') && (Character.isDigit(input.charAt(pos+1)))) {
		int val = Character.digit(input.charAt(++pos),10);
		if (val < start.length) {
		    output.append(toString(val));
		} 
	    } else output.append(input.charAt(pos));
	}
	if (pos < input.length()) output.append(input.charAt(pos));
	return output.toString();
    }
}
