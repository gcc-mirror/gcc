/* gnu/regexp/RETokenPOSIX.java
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

final class RETokenPOSIX extends REToken {
  int type;
  boolean insens;
  boolean negated;

  static final int  ALNUM = 0;
  static final int  ALPHA = 1;
  static final int  BLANK = 2;
  static final int  CNTRL = 3;
  static final int  DIGIT = 4;
  static final int  GRAPH = 5;
  static final int  LOWER = 6;
  static final int  PRINT = 7;
  static final int  PUNCT = 8;
  static final int  SPACE = 9;
  static final int  UPPER = 10;
  static final int XDIGIT = 11;

  // Array indices correspond to constants defined above.
  static final String[] s_nameTable =  {
    "alnum", "alpha", "blank", "cntrl", "digit", "graph", "lower",
    "print", "punct", "space", "upper", "xdigit" 
  };

  // The RE constructor uses this to look up the constant for a string
  static int intValue(String key) {
    for (int i = 0; i < s_nameTable.length; i++) {
      if (s_nameTable[i].equals(key)) return i;
    }
    return -1;
  }

  RETokenPOSIX(int subIndex, int type, boolean insens, boolean negated) {
    super(subIndex);
    this.type = type;
    this.insens = insens;
    this.negated = negated;
  }

    int getMinimumLength() {
	return 1;
    }

    int getMaximumLength() {
	return 1;
    }

    REMatch matchThis(CharIndexed input, REMatch mymatch) {
      char ch = input.charAt(mymatch.index);
      boolean retval = matchOneChar(ch);
      if (retval) {
	++mymatch.index;
	return mymatch;
      }
      return null;
    }

    boolean matchOneChar(char ch) {
    if (ch == CharIndexed.OUT_OF_BOUNDS)
      return false;
    
    boolean retval = false;
    switch (type) {
    case ALNUM:
	// Note that there is some debate over whether '_' should be included
	retval = Character.isLetterOrDigit(ch) || (ch == '_');
	break;
    case ALPHA:
	retval = Character.isLetter(ch);
	break;
    case BLANK:
	retval = ((ch == ' ') || (ch == '\t'));
	break;
    case CNTRL:
	retval = Character.isISOControl(ch);
	break;
    case DIGIT:
	retval = Character.isDigit(ch);
	break;
    case GRAPH:
	retval = (!(Character.isWhitespace(ch) || Character.isISOControl(ch)));
	break;
    case LOWER:
	retval = ((insens && Character.isLetter(ch)) || Character.isLowerCase(ch));
	break;
    case PRINT:
	retval = (!(Character.isWhitespace(ch) || Character.isISOControl(ch)))
	    || (ch == ' ');
	break;
    case PUNCT:
	// This feels sloppy, especially for non-U.S. locales.
	retval = ("`~!@#$%^&*()-_=+[]{}\\|;:'\"/?,.<>".indexOf(ch)!=-1);
	break;
    case SPACE:
	retval = Character.isWhitespace(ch);
	break;
    case UPPER:
	retval = ((insens && Character.isLetter(ch)) || Character.isUpperCase(ch));
	break;
    case XDIGIT:
	retval = (Character.isDigit(ch) || ("abcdefABCDEF".indexOf(ch)!=-1));
	break;
    }

    if (negated) retval = !retval;
    return retval;
  }

  boolean returnsFixedLengthMatches() { return true; }

  int findFixedLengthMatches(CharIndexed input, REMatch mymatch, int max) {
      int index = mymatch.index;
      int numRepeats = 0;
      while (true) {
	if (numRepeats >= max) break;
	char ch = input.charAt(index++);
	if (! matchOneChar(ch)) break;
	numRepeats++;
      }
      return numRepeats;
  }

  void dump(StringBuffer os) {
    if (negated) os.append('^');
    os.append("[:" + s_nameTable[type] + ":]");
  }
}
