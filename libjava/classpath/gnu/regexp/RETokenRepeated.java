/* gnu/regexp/RETokenRepeated.java
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


package gnu.regexp;

import java.util.Vector;
import java.util.Arrays;

final class RETokenRepeated extends REToken {
    private REToken token;
    private int min,max;
    private boolean stingy;
    private boolean possessive;
    
    RETokenRepeated(int subIndex, REToken token, int min, int max) {
	super(subIndex);
	this.token = token;
	this.min = min;
	this.max = max;
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
		recurrent.matchFlags |= REMatch.MF_FIND_ALL;
		if (tk.match(input, recurrent)) {
		    for (REMatch m = recurrent; m != null; m = m.next) {
			m.matchFlags &= ~REMatch.MF_FIND_ALL;
		    }
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

        boolean stopMatchingIfSatisfied =
		(mymatch.matchFlags & REMatch.MF_FIND_ALL) == 0;

	REMatch newMatch = matchMinimum(input, mymatch);
	if (newMatch == null) return false;

	// Array of positions we have already visited
	int[] visited = initVisited();
	for (REMatch m = newMatch; m != null; m = m.next) {
	    visited = addVisited(m.index, visited);
	}

	int max1 = decreaseMax(max, min);

	newMatch = _match(input, newMatch, max1,
	    stopMatchingIfSatisfied, visited);
	if (newMatch != null) {
	    mymatch.assignFrom(newMatch);
	    return true;
	}
	return false;
    }

    private static int decreaseMax(int m, int n) {
        if (m == Integer.MAX_VALUE) return m;
	return m - n;
    }

    // Array visited is an array of character positions we have already
    // visited. visited[0] is used to store the effective length of the
    // array.
    private static int[] initVisited() {
	int[] visited = new int[32];
	visited[0] = 0;
	return visited;
    }

    private static boolean visitedContains(int n, int[] visited) {
	// Experience tells that for a small array like this,
	// simple linear search is faster than binary search.
	for (int i = 1; i < visited[0]; i++) {
	    if (n == visited[i]) return true;
	}
	return false;
    }

    private static int[] addVisited(int n, int[] visited) {
	if (visitedContains(n, visited)) return visited;
	if (visited[0] >= visited.length - 1) {
	    int[] newvisited = new int[visited.length + 32];
	    System.arraycopy(visited, 0, newvisited, 0, visited.length);
	    visited = newvisited;
	}
	visited[0]++;
	visited[visited[0]] = n;
	return visited;
    }

    private REMatch _match(CharIndexed input, REMatch mymatch,
    	    int max1, boolean stopMatchingIfSatisfied,
	    int[] visited) {

        if (max1 == 0) {
	    return matchRest(input, mymatch);
	}
	max1 = decreaseMax(max1, 1);

	REMatch.REMatchList allResults = new REMatch.REMatchList();

	// Depth-first search

	for (REMatch cur = mymatch; cur != null; cur = cur.next) {

	    REMatch cur1 = (REMatch) cur.clone();

	    if (stingy) {
	        REMatch results = matchRest(input, cur1);
	        if (results != null) {
	            if (stopMatchingIfSatisfied) {
		        return results;
		    }
		    allResults.addTail(results);
	        }
	    }

	    DO_THIS:
	    do {

	    boolean emptyMatchFound = false;
	    REMatch doables = findDoables(token, input, cur1);
	    if (doables == null) break DO_THIS;
	    if (doables.empty) emptyMatchFound = true;

	    if (!emptyMatchFound) {
	        REMatch.REMatchList list = new REMatch.REMatchList();
	        for (REMatch m = doables; m != null; m = m.next) {
	            REMatch m1 = (REMatch) m.clone();
		    int n = m1.index;
		    if (! visitedContains(n, visited)) {
		        visited = addVisited(n, visited);
		        list.addTail(m1);
		    }
	        }
	        if (list.head == null) break DO_THIS;
	        doables = list.head;
	    }

	    for (REMatch m = doables; m != null; m = m.next) {
	        if (! emptyMatchFound) {
	            REMatch m1 = _match(input, m, max1,
		        stopMatchingIfSatisfied, visited);
		    if (possessive) return m1;
		    if (m1 != null) {
	                if (stopMatchingIfSatisfied) {
		            return m1;
		        }
		        allResults.addTail(m1);
		    }
	        }
		else {
		    REMatch m1 = matchRest(input, m);
		    if (m1 != null) {
		        if (stopMatchingIfSatisfied) {
		            return m1;
		        }
		        allResults.addTail(m1);
		    }
		}
	    }

	    } while (false); // DO_THIS only once;

	    // This point itself is a candidate.
	    if (!stingy) {
		REMatch m2 = matchRest(input, cur1);
		if (m2 != null) {
		    if (stopMatchingIfSatisfied) {
		        return m2;
		    }
		    allResults.addTail(m2);
	        }
	    }
	}

        return allResults.head;
    }

    private REMatch matchMinimum(CharIndexed input, final REMatch mymatch) {
	// Possible positions for the next repeat to match at
	REMatch newMatch = mymatch;

	// number of times we've matched so far
	int numRepeats = 0; 
	
	while (numRepeats < min) {
	    REMatch doables = findDoables(token, input, newMatch);

	    // if none of the possibilities worked out, 
	    // it means that minimum number of repeats could not be found.
	    if (doables == null) return null;
	    
	    // reassign where the next repeat can match
	    newMatch = doables;
	    
	    // increment how many repeats we've successfully found
	    ++numRepeats;
	    
	    if (newMatch.empty) break;
	}
	return newMatch;
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
