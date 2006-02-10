/* gnu/regexp/RETokenRepeated.java
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

final class RETokenRepeated extends REToken {
    private REToken token;
    private int min,max;
    private boolean stingy;
    private boolean possessive;
    private boolean alwaysEmpty; // Special case of {0}
    
    RETokenRepeated(int subIndex, REToken token, int min, int max) {
	super(subIndex);
	this.token = token;
	this.min = min;
	this.max = max;
	alwaysEmpty = (min == 0 && max == 0);
    }

    /** Sets the minimal matching mode to true. */
    void makeStingy() {
	stingy = true;
    }
    
    /** Queries if this token has minimal matching enabled. */
    boolean isStingy() {
	return stingy;
    }

    /** Sets possessive matching mode to true. */
    void makePossessive() {
        possessive = true;
    }

    /** Queries if this token has possessive matching enabled. */
    boolean isPossessive() {
        return possessive;
    }
    
    /**
     * The minimum length of a repeated token is the minimum length
     * of the token multiplied by the minimum number of times it must
     * match.
     */
    int getMinimumLength() {
	return (min * token.getMinimumLength());
    }

    int getMaximumLength() {
        if (max == Integer.MAX_VALUE) return Integer.MAX_VALUE;
	int tmax = token.getMaximumLength();
	if (tmax == Integer.MAX_VALUE) return tmax;
	return (max * tmax);
    }

    boolean stopMatchingIfSatisfied = true;

    private static REMatch findDoables(REToken tk,
			CharIndexed input, REMatch mymatch) {

	    REMatch.REMatchList doables = new REMatch.REMatchList();

	    // try next repeat at all possible positions
	    for (REMatch current = mymatch;
		 current != null; current = current.next) {
		REMatch recurrent = (REMatch) current.clone();
		int origin = recurrent.index;
		tk = (REToken) tk.clone();
		tk.next = tk.uncle = null;
		if (tk.match(input, recurrent)) {
		    if (recurrent.index == origin) recurrent.empty = true;
		    // add all items in current to doables array
		    doables.addTail(recurrent);
		}
	    }
	    return doables.head;
    }

    // We do need to save every possible point, but the number of clone()
    // invocations here is really a killer for performance on non-stingy
    // repeat operators.  I'm open to suggestions...

    // Hypothetical question: can you have a RE that matches 1 times,
    // 3 times, 5 times, but not 2 times or 4 times?  Does having
    // the subexpression back-reference operator allow that?

    boolean match(CharIndexed input, REMatch mymatch) {
	// Possible positions for the next repeat to match at
	REMatch newMatch = mymatch;

	// {0} needs some special treatment.
	if (alwaysEmpty) {
	    REMatch result = matchRest(input, newMatch);
	    if (result != null) {
	        mymatch.assignFrom(result);
	        return true;
	    }
	    else {
	        return false;
	    }
	}

	// number of times we've matched so far
	int numRepeats = 0; 
	
	REMatch doables;
	int lastIndex = mymatch.index;
	boolean emptyMatchFound = false;

	while (numRepeats < min) {
	    doables = findDoables(token, input, newMatch);

	    // if none of the possibilities worked out, 
	    // it means that minimum number of repeats could not be found.
	    if (doables == null) return false;
	    
	    // reassign where the next repeat can match
	    newMatch = doables;
	    
	    // increment how many repeats we've successfully found
	    ++numRepeats;
	    
	    if (newMatch.empty) {
		numRepeats = min;
		emptyMatchFound = true;
		break;
	    }
	    lastIndex = newMatch.index;
	}

	Vector positions = new Vector();

	while (numRepeats <= max) {
	    // We want to check something like  
	    //    if (stingy)
	    // and neglect the further matching.  But experience tells
	    // such neglection may cause incomplete matching.
	    // For example, if we neglect the seemingly unnecessay
	    // matching, /^(b+?|a){1,2}?c/ cannot match "bbc".
	    // On the other hand, if we do not stop the unnecessary
	    // matching, /(([a-c])b*?\2)*/ matches "ababbbcbc"
	    // entirely when we wan to find only "ababb".
	    // In order to make regression tests pass, we do as we did.
	    if (stopMatchingIfSatisfied && stingy) {
		REMatch results = matchRest(input, newMatch);
		if (results != null) {
		    mymatch.assignFrom(results);
		    return true;
		}
	    }
	    positions.add(newMatch);
	    if (emptyMatchFound) break;

	    doables = findDoables(token, input, newMatch);
	    if (doables == null) break;

	    // doables.index == lastIndex occurs either
	    //   (1) when an empty string was the longest
	    //       that matched this token.
	    // or
	    //   (2) when the same string matches this token many times.
	    //       For example, "acbab" itself matches "a.*b" and
	    //       its substrings "acb" and "ab" also match.
	    //       In this case, we do not have to go further until
	    //       numRepeats == max because the more numRepeats grows,
	    //       the shorter the substring matching this token becomes.
	    //       So the previous succesful match must have bee the best
	    //       match.  But this is not necessarily the case if stingy.
	    if (doables.index == lastIndex) {
	        if (doables.empty) {
		    emptyMatchFound = true;
                }
	        else {
		    if (!stingy) break;
		}
	    }
	    numRepeats++;
	    newMatch = doables;
	    lastIndex = newMatch.index;
	}

	// We're greedy, but ease off until a true match is found.
	// At this point we've either got too many or just the right amount.
	// See if this numRepeats works with the rest of the regexp.

	REMatch.REMatchList allResults = new REMatch.REMatchList();

	int posCount = positions.size();
	int posIndex = (stingy ? 0 : posCount - 1);

	while (posCount-- > 0) {
	    REMatch m = (REMatch) positions.elementAt(posIndex);
            if (stingy) posIndex++; else posIndex--;

	    REMatch results = matchRest(input, m);
            if (results != null) {
	    	// Order these from longest to shortest
		// Start by assuming longest (more repeats)
		// If stingy the order is shortest to longest.
		allResults.addTail(results);
	    }
	    else {
		if (possessive) break;
	    }
	}

	if (allResults.head != null) {
	    mymatch.assignFrom(allResults.head); // does this get all?
	    return true;
	}
	// If we fall out, no matches.
	return false;
    }

    private REMatch matchRest(CharIndexed input, final REMatch newMatch) {
	REMatch current, single;
	REMatch.REMatchList doneIndex = new REMatch.REMatchList();
	// Test all possible matches for this number of repeats
	for (current = newMatch; current != null; current = current.next) {
	    // clone() separates a single match from the chain
	    single = (REMatch) current.clone();
	    if (next(input, single)) {
		// chain results to doneIndex
		doneIndex.addTail(single);
	    }
	}
	return doneIndex.head;
    }

    void dump(StringBuffer os) {
	os.append("(?:");
	token.dumpAll(os);
	os.append(')');
	if ((max == Integer.MAX_VALUE) && (min <= 1))
	    os.append( (min == 0) ? '*' : '+' );
	else if ((min == 0) && (max == 1))
	    os.append('?');
	else {
	    os.append('{').append(min);
	    if (max > min) {
		os.append(',');
		if (max != Integer.MAX_VALUE) os.append(max);
	    }
	    os.append('}');
	}
	if (stingy) os.append('?');
    }
}
