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


package gnu.java.util.regex;

import java.util.ArrayList;

final class RETokenRepeated extends REToken {
    private REToken token;
    private int min,max;
    private boolean stingy;
    private boolean possessive;
    private int tokenFixedLength;
    
    RETokenRepeated(int subIndex, REToken token, int min, int max) {
	super(subIndex);
	this.token = token;
	this.min = min;
	this.max = max;
	if (token.returnsFixedLengthMatches()) {
	    tokenFixedLength = token.getMaximumLength();
	}
	else {
	    tokenFixedLength = -1;
	}
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

    // The comment "MUST make a clone" below means that some tests
    // failed without doing clone(),

    private static class DoablesFinder {
	private REToken tk;
	private CharIndexed input;
	private REMatch rematch;
	private boolean findFirst;

        private DoablesFinder(REToken tk, CharIndexed input, REMatch mymatch) {
	    this.tk = tk;
	    this.input = input;
	    this.rematch = (REMatch) mymatch.clone(); // MUST make a clone
	    this.rematch.backtrackStack = new BacktrackStack();
	    findFirst = true;
	}

	private REMatch find() {
	    int origin = rematch.index;
	    REMatch rem;
	    if (findFirst) {
		rem = tk.findMatch(input, rematch);
		findFirst = false;
	    }
	    else {
	        while (true) {
		    if (rematch.backtrackStack.empty()) {
			rem = null;
			break;
		    }
		    BacktrackStack.Backtrack bt = rematch.backtrackStack.pop();
		    rem = bt.token.backtrack(bt.input, bt.match, bt.param);
		    if (rem != null) break;
		}
	    }
	    if (rem == null) return null;
	    if (rem.index == origin) rem.empty = true;
	    rematch = rem;
	    return (REMatch) rem.clone(); // MUST make a clone.
	}

	boolean noMore() {
	    return rematch.backtrackStack.empty();
	}
    }

    REMatch findMatch(CharIndexed input, REMatch mymatch) {
        if (tokenFixedLength >= 0) return findMatchFixedLength(input, mymatch);
	BacktrackStack stack = new BacktrackStack();
	stack.push(new StackedInfo(input, 0, mymatch, null, null));
	return findMatch(stack);
    }

    REMatch backtrack(CharIndexed input, REMatch mymatch, Object param) {
        if (tokenFixedLength >= 0) return backtrackFixedLength(input, mymatch, param);
	return findMatch((BacktrackStack)param);
    }

    private static class StackedInfo extends BacktrackStack.Backtrack {
        int numRepeats;
	int[] visited;
        DoablesFinder finder;
        StackedInfo(CharIndexed input, int numRepeats, REMatch match,
	        int[] visited, DoablesFinder finder) {
	    super(null, input, match, null);
            this.numRepeats = numRepeats;
	    this.visited = visited;
            this.finder = finder;
	}
    }

    private static class FindMatchControlStack extends ArrayList {
	private void push(FindMatchControl control) {
	    add(control);
	}
	private FindMatchControl pop() {
	    return (FindMatchControl)remove(size()-1);
	}
	private boolean empty() {
	    return isEmpty();
	}
    }

    private static class FindMatchControl {
	DoablesFinder finder;
	FindMatchControl(DoablesFinder finder) {
	    this.finder = finder;
	}
    }

    private REMatch findMatch(BacktrackStack stack) {
	return findMatch(stack, new FindMatchControlStack());
    }

    private REMatch findMatch(BacktrackStack stack,
		FindMatchControlStack controlStack) {
	REMatch result = null;
	StackedInfo si = null;
	CharIndexed input = null;
        int numRepeats = 0;
        REMatch mymatch = null;
	int[] visited = null;
        DoablesFinder finder = null;

        // Avoid using recursive calls because a match can be very long.

	// This is the first entry point of this method.
	// If you want to call this method recursively and you need the
	// result returned, save necessary information in a FindMatchControl
	// object and push it to controlStack, then continue from this point.
	// You can check the result after exiting MAIN_LOOP.
	MAIN_LOOP0:
	while (true) {

	// This is the second entry point of this method.
	// If you want to call this method recursively but you do not need the
	// result returned, just continue from this point.
	MAIN_LOOP:
	while (true) {

	if (stack.empty()) break MAIN_LOOP;
	si = (StackedInfo)(stack.peek());
	input = si.input;
        numRepeats = si.numRepeats;
        mymatch = si.match;
	visited = si.visited;
        finder = si.finder;

	if (mymatch.backtrackStack == null)
	  mymatch.backtrackStack = new BacktrackStack();
	
	if (numRepeats >= max) {
	    stack.pop();
	    REMatch m1 = matchRest(input, mymatch);
	    if (m1 != null) {
		if (! stack.empty()) {
	            m1.backtrackStack.push(new BacktrackStack.Backtrack(
		        this, input, mymatch, stack));
		}
		result = m1;
		break MAIN_LOOP;
	    }
	    if (stingy) {
		continue MAIN_LOOP;
	    }
	    break MAIN_LOOP;
	}

        if (finder == null) {
	    finder = new DoablesFinder(token, input, mymatch);
	    si.finder = finder;
	}

        if (numRepeats < min) {
	    while (true) {
	        REMatch doable = finder.find();
	        if (doable == null) {
		    if (stack.empty()) return null;
		    stack.pop();
		    continue MAIN_LOOP;
		}
		if (finder.noMore()) stack.pop();
		int newNumRepeats = (doable.empty ? min : numRepeats + 1);
		stack.push(new StackedInfo(
		    input, newNumRepeats, doable, visited, null));
		continue MAIN_LOOP;
	    }
	}

	if (visited == null) visited = initVisited();

	if (stingy) {
	    REMatch nextMatch = finder.find();
	    if (nextMatch != null && !nextMatch.empty) {
	        stack.push(new StackedInfo(
	            input, numRepeats + 1, nextMatch, visited, null));
	    }
	    else {
		stack.pop();
	    }  	
	    REMatch m1 = matchRest(input, mymatch);
	    if (m1 != null) {
		if (!stack.empty()) {
	            m1.backtrackStack.push(new BacktrackStack.Backtrack(
		        this, input, mymatch, stack));
		}
		result = m1;
		break MAIN_LOOP;
	    }
	    else {
		continue MAIN_LOOP;
	    }
	}

	visited = addVisited(mymatch.index, visited);

	TryAnotherResult taresult = tryAnother(stack, input, mymatch, numRepeats, finder, visited);
	visited = taresult.visited;
	switch (taresult.status) {
	    case TryAnotherResult.TRY_FURTHER:
	        controlStack.push(new FindMatchControl(
		    finder));
	        continue MAIN_LOOP0;
	    case TryAnotherResult.RESULT_FOUND:
	        result = taresult.result;
	        break MAIN_LOOP;
	}

	if (!stack.empty()) {
	    stack.pop();
        }
	if (possessive) {
	    stack.clear();
	}
	REMatch m1 = matchRest(input, mymatch);
	if (m1 != null) {
	    if (! stack.empty()) {
	        m1.backtrackStack.push(new BacktrackStack.Backtrack(
	            this, input, mymatch, stack));
	    }
	    result = m1;
	    break MAIN_LOOP;
	}

	} // MAIN_LOOP

	if (controlStack.empty()) return result;
	FindMatchControl control = controlStack.pop();
	if (possessive) {
	    return result;
	}
	if (result != null) {
	    result.backtrackStack.push(new BacktrackStack.Backtrack(
                this, input, mymatch, stack));
	    return result;
	}

	finder = control.finder;

	TryAnotherResult taresult = tryAnother(stack, input, mymatch, numRepeats, finder, visited);
	visited = taresult.visited;
	switch (taresult.status) {
	    case TryAnotherResult.TRY_FURTHER:
	        controlStack.push(new FindMatchControl(
		    finder));
	        continue MAIN_LOOP0;
	    case TryAnotherResult.RESULT_FOUND:
	        return taresult.result;
	}
	continue MAIN_LOOP0;

	} // MAIN_LOOP0
    }

    private static class TryAnotherResult {
	REMatch result;
        int status;
        static final int RESULT_FOUND = 1;
	static final int TRY_FURTHER = 2;
	static final int NOTHING_FOUND = 3; 
	int[] visited;
    }

    private TryAnotherResult tryAnother(BacktrackStack stack,
		CharIndexed input, REMatch mymatch, int numRepeats,
		DoablesFinder finder, int[] visited) {

	TryAnotherResult taresult = new TryAnotherResult();
	taresult.visited = visited;

	DO_THIS:
	{

	    boolean emptyMatchFound = false;

	    DO_ONE_DOABLE:
	    while (true) {

	    REMatch doable = finder.find();
	    if (doable == null) {
		break DO_THIS;
	    }
	    if (doable.empty) emptyMatchFound = true;

	    if (!emptyMatchFound) {
		int n = doable.index;
		if (visitedContains(n, visited)) {
		    continue DO_ONE_DOABLE;
		}
		visited = addVisited(n, visited);
	        stack.push(new StackedInfo(
	            input, numRepeats + 1, doable, visited, null));
		taresult.visited = visited;
		taresult.status = TryAnotherResult.TRY_FURTHER;
		return taresult;
	    }
	    else {
	        REMatch m1 = matchRest(input, doable);
		if (possessive) {
		    taresult.result = m1;
		    taresult.status = TryAnotherResult.RESULT_FOUND;
		    return taresult;
		}
	        if (m1 != null) {
		    if (! stack.empty()) {
		        m1.backtrackStack.push(new BacktrackStack.Backtrack(
                            this, input, mymatch, stack));
		    }
		    taresult.result = m1;
		    taresult.status = TryAnotherResult.RESULT_FOUND;
		    return taresult;
		}
	    }

	    } // DO_ONE_DOABLE

	} // DO_THIS

	taresult.status = TryAnotherResult.NOTHING_FOUND;
	return taresult;

    }

    boolean match(CharIndexed input, REMatch mymatch) {
	setHitEnd(input, mymatch);
	REMatch m1 = findMatch(input, mymatch);
	if (m1 != null) {
	    mymatch.assignFrom(m1);
	    return true;
	}
	return false;
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

    private REMatch matchRest(CharIndexed input, final REMatch newMatch) {
	if (next(input, newMatch)) {
	    return newMatch;
	}
	return null;
    }

    private REMatch findMatchFixedLength(CharIndexed input, REMatch mymatch) {
	if (mymatch.backtrackStack == null)
	  mymatch.backtrackStack = new BacktrackStack();
        int numRepeats = token.findFixedLengthMatches(input, (REMatch)mymatch.clone(), max);
	if (numRepeats == Integer.MAX_VALUE) numRepeats = min;
	int count = numRepeats - min + 1;
        if (count <= 0) return null;
	int index = 0;
	if (!stingy) index = mymatch.index + (tokenFixedLength * numRepeats);
	else index = mymatch.index + (tokenFixedLength * min);
	return findMatchFixedLength(input, mymatch, index, count);
    }

    private REMatch backtrackFixedLength(CharIndexed input, REMatch mymatch,
    	    Object param) {
	int[] params = (int[])param;
        int index = params[0];
	int count = params[1];
	return findMatchFixedLength(input, mymatch, index, count);
    }        

    private REMatch findMatchFixedLength(CharIndexed input, REMatch mymatch,
    	    	    int index, int count) {
        REMatch tryMatch = (REMatch) mymatch.clone();
	while (true) {
	    tryMatch.index = index;
	    REMatch m = matchRest(input, tryMatch);
	    count--;
	    if (stingy) index += tokenFixedLength;
	    else index -= tokenFixedLength;
	    if (possessive) return m;
	    if (m != null) {
		if (count > 0) {
	            m.backtrackStack.push(new BacktrackStack.Backtrack(
		        this, input, mymatch,
			new int[] {index, count}));
	        }
		return m;
	    }
	    if (count <= 0) return null;
	}
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
