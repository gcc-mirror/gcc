/* gnu/regexp/REMatchEnumeration.java
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

package gnu.java.util.regex;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.NoSuchElementException;

/**
 * An REMatchEnumeration enumerates regular expression matches over a
 * given input text.  You obtain a reference to an enumeration using
 * the <code>getMatchEnumeration()</code> methods on an instance of
 * RE. 
 *
 * <P>
 *
 * REMatchEnumeration does lazy computation; that is, it will not
 * search for a match until it needs to.  If you'd rather just get all
 * the matches at once in a big array, use the
 * <code>getAllMatches()</code> methods on RE.  However, using an
 * enumeration can help speed performance when the entire text does
 * not need to be searched immediately.
 *
 * <P>
 * 
 * The enumerated type is especially useful when searching on a Reader
 * or InputStream, because the InputStream read position cannot be
 * guaranteed after calling <code>getMatch()</code> (see the
 * description of that method for an explanation of why).  Enumeration
 * also saves a lot of overhead required when calling
 * <code>getMatch()</code> multiple times.
 * 
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A> 
 */
public class REMatchEnumeration implements Enumeration, Serializable {
  private static final int YES = 1;
  private static final int MAYBE = 0;
  private static final int NO = -1;
  
  private int more;
  private REMatch match;
  private RE expr;
  private CharIndexed input;
  private int eflags;
    private int index;

  // Package scope constructor is used by RE.getMatchEnumeration()
  REMatchEnumeration(RE expr, CharIndexed input, int index, int eflags) {
    more = MAYBE;
    this.expr = expr;
    this.input = input;
    this.index = index;
    this.eflags = eflags;
  }

  /** Returns true if there are more matches in the input text. */
  public boolean hasMoreElements() {
    return hasMoreMatches(null);
  }

  /** Returns true if there are more matches in the input text. */
  public boolean hasMoreMatches() {
    return hasMoreMatches(null);
  }

  /** Returns true if there are more matches in the input text.
   * Saves the text leading up to the match (or to the end of the input)
   * in the specified buffer.
   */
  public boolean hasMoreMatches(StringBuffer buffer) {
    if (more == MAYBE) {
	match = expr.getMatchImpl(input,index,eflags,buffer);
	if (match != null) {
	    input.move((match.end[0] > 0) ? match.end[0] : 1);
	    
	    index = (match.end[0] > 0) ? match.end[0] + match.offset : index + 1;
	    more = YES;
	} else more = NO;
    }
    return (more == YES);
  }

  /** Returns the next match in the input text. */
  public Object nextElement() throws NoSuchElementException {
    return nextMatch();
  }

  /** 
   * Returns the next match in the input text. This method is provided
   * for convenience to avoid having to explicitly cast the return value
   * to class REMatch.
   */
  public REMatch nextMatch() throws NoSuchElementException {
    if (hasMoreElements()) {
	more = (input.isValid()) ? MAYBE : NO;
	return match;
    }
    throw new NoSuchElementException();
  }
}

