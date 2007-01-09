/* gnu/regexp/REToken.java
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
import java.io.Serializable;

abstract class REToken implements Serializable, Cloneable {

  protected REToken next = null;
  protected REToken uncle = null;
  protected int subIndex;
  protected boolean unicodeAware = true;

  public Object clone() {
    try {
      REToken copy = (REToken) super.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // doesn't happen
    }
  }

  protected REToken(int subIndex) {
      this.subIndex = subIndex;
  }

  int getMinimumLength() {
    return 0;
  }

  int getMaximumLength() {
    return Integer.MAX_VALUE;
  }

  void setUncle(REToken anUncle) {
    uncle = anUncle;
  }

    /** Returns true if the match succeeded, false if it failed. */
    boolean match(CharIndexed input, REMatch mymatch) {
	return match(input, mymatch, false);
    }
    boolean matchFake(CharIndexed input, REMatch mymatch) {
	return match(input, mymatch, true);
    }

    private boolean match(CharIndexed input, REMatch mymatch, boolean fake) {
	if (!fake) {
	    setHitEnd(input, mymatch);
	}
	REMatch m = matchThis(input, mymatch);
	if (m == null) return false;
	if (next(input, m)) {
	    mymatch.assignFrom(m);
	    return true;
	}
	return false;
    }

    /** Sets whether the matching occurs at the end of input */
    void setHitEnd(CharIndexed input, REMatch mymatch) {
        input.setHitEnd(mymatch);
    }

    /** Returns true if the match succeeded, false if it failed.
      * The matching is done against this REToken only. Chained
      * tokens are not checked.
      * This method is used to define the default match method.
      * Simple subclasses of REToken, for example, such that
      * matches only one character, should implement this method.
      * Then the default match method will work.  But complicated
      * subclasses of REToken, which needs a special match method,
      * do not have to implement this method.
      */
    REMatch matchThis(CharIndexed input, REMatch mymatch) {
	throw new UnsupportedOperationException(
	    "This REToken does not have a matchThis method");
    }
  
    /** Returns true if the rest of the tokens match, false if they fail. */
    protected boolean next(CharIndexed input, REMatch mymatch) {
	REToken nextToken = getNext();
	if (nextToken == null) return true;
	return nextToken.match(input, mymatch);
    }

    /** Returns the next REToken chained to this REToken. */
    REToken getNext() {
	return (next != null ? next : uncle);
    }

    /** Finds a match at the position specified by the given REMatch.
      * If necessary, adds a BacktrackStack.Backtrack object to backtrackStack
      * of the REmatch found this time so that another possible match
      * may be found when backtrack is called.
      * By default, nothing is added to the backtrackStack.
      * @param CharIndexed input Input character sequence.
      * @param mymatch Position at which a match should be found
      * @return REMatch object if a match was found, null otherwise.
      */
    REMatch findMatch(CharIndexed input, REMatch mymatch) {
        boolean b = match(input, mymatch);
	if (b) return mymatch;
	return null;
    }

    boolean returnsFixedLengthMatches() {
	return false;
    }

    int findFixedLengthMatches(CharIndexed input, REMatch mymatch, int max) {
	throw new UnsupportedOperationException(
	    "This token does not support findFixedLengthMatches");
    }

    /**
      * Backtrack to another possibility.
      * Ordinary REToken cannot do anything if this method is called.
      */
    REMatch backtrack(CharIndexed input, REMatch mymatch, Object param) {
	throw new IllegalStateException("This token cannot be backtracked to");
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

  public String toString() {
    StringBuffer os = new StringBuffer();
    dump(os);
    return os.toString();
  }

  /**
    * Converts the character argument to lowercase.
    * @param ch the character to be converted.
    * @param unicodeAware If true, use java.lang.Character#toLowerCase;
    * otherwise, only US-ASCII charactes can be converted.
    * @return the lowercase equivalent of the character, if any;
    * otherwise, the character itself.
    */
  public static char toLowerCase(char ch, boolean unicodeAware) {
    if (unicodeAware) return Character.toLowerCase(ch);
    if (ch >= 'A' && ch <= 'Z') return (char)(ch + 'a' - 'A');
    return ch;
  }

  /**
    * Converts the character argument to uppercase.
    * @param ch the character to be converted.
    * @param unicodeAware If true, use java.lang.Character#toUpperCase;
    * otherwise, only US-ASCII charactes can be converted.
    * @return the uppercase equivalent of the character, if any;
    * otherwise, the character itself.
    */
  public static char toUpperCase(char ch, boolean unicodeAware) {
    if (unicodeAware) return Character.toUpperCase(ch);
    if (ch >= 'a' && ch <= 'z') return (char)(ch + 'A' - 'a');
    return ch;
  }

}
