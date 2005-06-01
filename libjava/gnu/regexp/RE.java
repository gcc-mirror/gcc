/* gnu/regexp/RE.java
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
import java.io.InputStream;
import java.io.Serializable;
import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.Vector;

/**
 * RE provides the user interface for compiling and matching regular
 * expressions.
 * <P>
 * A regular expression object (class RE) is compiled by constructing it
 * from a String, StringBuffer or character array, with optional 
 * compilation flags (below)
 * and an optional syntax specification (see RESyntax; if not specified,
 * <code>RESyntax.RE_SYNTAX_PERL5</code> is used).
 * <P>
 * Once compiled, a regular expression object is reusable as well as
 * threadsafe: multiple threads can use the RE instance simultaneously
 * to match against different input text.
 * <P>
 * Various methods attempt to match input text against a compiled
 * regular expression.  These methods are:
 * <LI><code>isMatch</code>: returns true if the input text in its
 * entirety matches the regular expression pattern.
 * <LI><code>getMatch</code>: returns the first match found in the
 * input text, or null if no match is found.
 * <LI><code>getAllMatches</code>: returns an array of all
 * non-overlapping matches found in the input text.  If no matches are
 * found, the array is zero-length.
 * <LI><code>substitute</code>: substitute the first occurence of the
 * pattern in the input text with a replacement string (which may
 * include metacharacters $0-$9, see REMatch.substituteInto).
 * <LI><code>substituteAll</code>: same as above, but repeat for each
 * match before returning.
 * <LI><code>getMatchEnumeration</code>: returns an REMatchEnumeration
 * object that allows iteration over the matches (see
 * REMatchEnumeration for some reasons why you may want to do this
 * instead of using <code>getAllMatches</code>.
 * <P>
 *
 * These methods all have similar argument lists.  The input can be a
 * String, a character array, a StringBuffer, or an
 * InputStream of some sort.  Note that when using an
 * InputStream, the stream read position cannot be guaranteed after
 * attempting a match (this is not a bug, but a consequence of the way
 * regular expressions work).  Using an REMatchEnumeration can
 * eliminate most positioning problems.
 *
 * <P>
 *
 * The optional index argument specifies the offset from the beginning
 * of the text at which the search should start (see the descriptions
 * of some of the execution flags for how this can affect positional
 * pattern operators).  For an InputStream, this means an
 * offset from the current read position, so subsequent calls with the
 * same index argument on an InputStream will not
 * necessarily access the same position on the stream, whereas
 * repeated searches at a given index in a fixed string will return
 * consistent results.
 *
 * <P>
 * You can optionally affect the execution environment by using a
 * combination of execution flags (constants listed below).
 * 
 * <P>
 * All operations on a regular expression are performed in a
 * thread-safe manner.
 *
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A>
 * @version 1.1.5-dev, to be released
 */

public class RE extends REToken {

  private static final class IntPair implements Serializable {
    public int first, second;
  }

  private static final class CharUnit implements Serializable {
    public char ch;
    public boolean bk;
  }

  // This String will be returned by getVersion()
  private static final String VERSION = "1.1.5-dev";

  // The localized strings are kept in a separate file
  private static ResourceBundle messages = PropertyResourceBundle.getBundle("gnu/regexp/MessagesBundle", Locale.getDefault());

  // These are, respectively, the first and last tokens in our linked list
  // If there is only one token, firstToken == lastToken
  private REToken firstToken, lastToken;

  // This is the number of subexpressions in this regular expression,
  // with a minimum value of zero.  Returned by getNumSubs()
  private int numSubs;

    /** Minimum length, in characters, of any possible match. */
    private int minimumLength;

  /**
   * Compilation flag. Do  not  differentiate  case.   Subsequent
   * searches  using  this  RE will be case insensitive.
   */
  public static final int REG_ICASE = 2;

  /**
   * Compilation flag. The match-any-character operator (dot)
   * will match a newline character.  When set this overrides the syntax
   * bit RE_DOT_NEWLINE (see RESyntax for details).  This is equivalent to
   * the "/s" operator in Perl.
   */
  public static final int REG_DOT_NEWLINE = 4;

  /**
   * Compilation flag. Use multiline mode.  In this mode, the ^ and $
   * anchors will match based on newlines within the input. This is
   * equivalent to the "/m" operator in Perl.
   */
  public static final int REG_MULTILINE = 8;

  /**
   * Execution flag.
   * The match-beginning operator (^) will not match at the beginning
   * of the input string. Useful for matching on a substring when you
   * know the context of the input is such that position zero of the
   * input to the match test is not actually position zero of the text.
   * <P>
   * This example demonstrates the results of various ways of matching on
   * a substring.
   * <P>
   * <CODE>
   * String s = "food bar fool";<BR>
   * RE exp = new RE("^foo.");<BR>
   * REMatch m0 = exp.getMatch(s);<BR>
   * REMatch m1 = exp.getMatch(s.substring(8));<BR>
   * REMatch m2 = exp.getMatch(s.substring(8),0,RE.REG_NOTBOL); <BR>
   * REMatch m3 = exp.getMatch(s,8);                            <BR>
   * REMatch m4 = exp.getMatch(s,8,RE.REG_ANCHORINDEX);         <BR>
   * <P>
   * // Results:<BR>
   * //  m0.toString(): "food"<BR>
   * //  m1.toString(): "fool"<BR>
   * //  m2.toString(): null<BR>
   * //  m3.toString(): null<BR>
   * //  m4.toString(): "fool"<BR>
   * </CODE>
   */
  public static final int REG_NOTBOL = 16;

  /**
   * Execution flag.
   * The match-end operator ($) does not match at the end
   * of the input string. Useful for matching on substrings.
   */
  public static final int REG_NOTEOL = 32;

  /**
   * Execution flag.
   * When a match method is invoked that starts matching at a non-zero
   * index into the input, treat the input as if it begins at the index
   * given.  The effect of this flag is that the engine does not "see"
   * any text in the input before the given index.  This is useful so
   * that the match-beginning operator (^) matches not at position 0
   * in the input string, but at the position the search started at
   * (based on the index input given to the getMatch function).  See
   * the example under REG_NOTBOL.  It also affects the use of the \&lt;
   * and \b operators.
   */
  public static final int REG_ANCHORINDEX = 64;

  /**
   * Execution flag.
   * The substitute and substituteAll methods will not attempt to
   * interpolate occurrences of $1-$9 in the replacement text with
   * the corresponding subexpressions.  For example, you may want to
   * replace all matches of "one dollar" with "$1".
   */
  public static final int REG_NO_INTERPOLATE = 128;

  /** Returns a string representing the version of the gnu.regexp package. */
  public static final String version() {
    return VERSION;
  }

  // Retrieves a message from the ResourceBundle
  static final String getLocalizedMessage(String key) {
    return messages.getString(key);
  }

  /**
   * Constructs a regular expression pattern buffer without any compilation
   * flags set, and using the default syntax (RESyntax.RE_SYNTAX_PERL5).
   *
   * @param pattern A regular expression pattern, in the form of a String,
   *   StringBuffer or char[].  Other input types will be converted to
   *   strings using the toString() method.
   * @exception REException The input pattern could not be parsed.
   * @exception NullPointerException The pattern was null.
   */
  public RE(Object pattern) throws REException {
    this(pattern,0,RESyntax.RE_SYNTAX_PERL5,0,0);
  }

  /**
   * Constructs a regular expression pattern buffer using the specified
   * compilation flags and the default syntax (RESyntax.RE_SYNTAX_PERL5).
   *
   * @param pattern A regular expression pattern, in the form of a String,
   *   StringBuffer, or char[].  Other input types will be converted to
   *   strings using the toString() method.
   * @param cflags The logical OR of any combination of the compilation flags listed above.
   * @exception REException The input pattern could not be parsed.
   * @exception NullPointerException The pattern was null.
   */
  public RE(Object pattern, int cflags) throws REException {
    this(pattern,cflags,RESyntax.RE_SYNTAX_PERL5,0,0);
  }

  /**
   * Constructs a regular expression pattern buffer using the specified
   * compilation flags and regular expression syntax.
   *
   * @param pattern A regular expression pattern, in the form of a String,
   *   StringBuffer, or char[].  Other input types will be converted to
   *   strings using the toString() method.
   * @param cflags The logical OR of any combination of the compilation flags listed above.
   * @param syntax The type of regular expression syntax to use.
   * @exception REException The input pattern could not be parsed.
   * @exception NullPointerException The pattern was null.
   */
  public RE(Object pattern, int cflags, RESyntax syntax) throws REException {
    this(pattern,cflags,syntax,0,0);
  }

  // internal constructor used for alternation
  private RE(REToken first, REToken last,int subs, int subIndex, int minLength) {
    super(subIndex);
    firstToken = first;
    lastToken = last;
    numSubs = subs;
    minimumLength = minLength;
    addToken(new RETokenEndSub(subIndex));
  }

  private RE(Object patternObj, int cflags, RESyntax syntax, int myIndex, int nextSub) throws REException {
    super(myIndex); // Subexpression index of this token.
    initialize(patternObj, cflags, syntax, myIndex, nextSub);
  }

    // For use by subclasses
    protected RE() { super(0); }

    // The meat of construction
  protected void initialize(Object patternObj, int cflags, RESyntax syntax, int myIndex, int nextSub) throws REException {
      char[] pattern;
    if (patternObj instanceof String) {
      pattern = ((String) patternObj).toCharArray();
    } else if (patternObj instanceof char[]) {
      pattern = (char[]) patternObj;
    } else if (patternObj instanceof StringBuffer) {
      pattern = new char [((StringBuffer) patternObj).length()];
      ((StringBuffer) patternObj).getChars(0,pattern.length,pattern,0);
    } else {
	pattern = patternObj.toString().toCharArray();
    }

    int pLength = pattern.length;

    numSubs = 0; // Number of subexpressions in this token.
    Vector branches = null;

    // linked list of tokens (sort of -- some closed loops can exist)
    firstToken = lastToken = null;

    // Precalculate these so we don't pay for the math every time we
    // need to access them.
    boolean insens = ((cflags & REG_ICASE) > 0);

    // Parse pattern into tokens.  Does anyone know if it's more efficient
    // to use char[] than a String.charAt()?  I'm assuming so.

    // index tracks the position in the char array
    int index = 0;

    // this will be the current parse character (pattern[index])
    CharUnit unit = new CharUnit();

    // This is used for {x,y} calculations
    IntPair minMax = new IntPair();

    // Buffer a token so we can create a TokenRepeated, etc.
    REToken currentToken = null;
    char ch;
    boolean quot = false;

    while (index < pLength) {
      // read the next character unit (including backslash escapes)
      index = getCharUnit(pattern,index,unit,quot);

      if (unit.bk)
        if (unit.ch == 'Q') {
          quot = true;
          continue;
        } else if (unit.ch == 'E') {
          quot = false;
          continue;
        }
      if (quot)
      	unit.bk = false;

      // ALTERNATION OPERATOR
      //  \| or | (if RE_NO_BK_VBAR) or newline (if RE_NEWLINE_ALT)
      //  not available if RE_LIMITED_OPS is set

      // TODO: the '\n' literal here should be a test against REToken.newline,
      // which unfortunately may be more than a single character.
      if ( ( (unit.ch == '|' && (syntax.get(RESyntax.RE_NO_BK_VBAR) ^ (unit.bk || quot)))
	     || (syntax.get(RESyntax.RE_NEWLINE_ALT) && (unit.ch == '\n') && !(unit.bk || quot)) )
	   && !syntax.get(RESyntax.RE_LIMITED_OPS)) {
	// make everything up to here be a branch. create vector if nec.
	addToken(currentToken);
	RE theBranch = new RE(firstToken, lastToken, numSubs, subIndex, minimumLength);
	minimumLength = 0;
	if (branches == null) {
	    branches = new Vector();
	}
	branches.addElement(theBranch);
	firstToken = lastToken = currentToken = null;
      }
      
      // INTERVAL OPERATOR:
      //  {x} | {x,} | {x,y}  (RE_INTERVALS && RE_NO_BK_BRACES)
      //  \{x\} | \{x,\} | \{x,y\} (RE_INTERVALS && !RE_NO_BK_BRACES)
      //
      // OPEN QUESTION: 
      //  what is proper interpretation of '{' at start of string?

      else if ((unit.ch == '{') && syntax.get(RESyntax.RE_INTERVALS) && (syntax.get(RESyntax.RE_NO_BK_BRACES) ^ (unit.bk || quot))) {
	int newIndex = getMinMax(pattern,index,minMax,syntax);
        if (newIndex > index) {
          if (minMax.first > minMax.second)
            throw new REException(getLocalizedMessage("interval.order"),REException.REG_BADRPT,newIndex);
          if (currentToken == null)
            throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,newIndex);
          if (currentToken instanceof RETokenRepeated) 
            throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,newIndex);
          if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
            throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,newIndex);
          if ((currentToken.getMinimumLength() == 0) && (minMax.second == Integer.MAX_VALUE))
            throw new REException(getLocalizedMessage("repeat.empty.token"),REException.REG_BADRPT,newIndex);
          index = newIndex;
          currentToken = setRepeated(currentToken,minMax.first,minMax.second,index); 
        }
        else {
          addToken(currentToken);
          currentToken = new RETokenChar(subIndex,unit.ch,insens);
        } 
      }
      
      // LIST OPERATOR:
      //  [...] | [^...]

      else if ((unit.ch == '[') && !(unit.bk || quot)) {
	Vector options = new Vector();
	boolean negative = false;
	char lastChar = 0;
	if (index == pLength) throw new REException(getLocalizedMessage("unmatched.bracket"),REException.REG_EBRACK,index);
	
	// Check for initial caret, negation
	if ((ch = pattern[index]) == '^') {
	  negative = true;
	  if (++index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	  ch = pattern[index];
	}

	// Check for leading right bracket literal
	if (ch == ']') {
	  lastChar = ch;
	  if (++index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	}

	while ((ch = pattern[index++]) != ']') {
	  if ((ch == '-') && (lastChar != 0)) {
	    if (index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	    if ((ch = pattern[index]) == ']') {
	      options.addElement(new RETokenChar(subIndex,lastChar,insens));
	      lastChar = '-';
	    } else {
	      options.addElement(new RETokenRange(subIndex,lastChar,ch,insens));
	      lastChar = 0;
	      index++;
	    }
          } else if ((ch == '\\') && syntax.get(RESyntax.RE_BACKSLASH_ESCAPE_IN_LISTS)) {
            if (index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	    int posixID = -1;
	    boolean negate = false;
            char asciiEsc = 0;
	    if (("dswDSW".indexOf(pattern[index]) != -1) && syntax.get(RESyntax.RE_CHAR_CLASS_ESC_IN_LISTS)) {
	      switch (pattern[index]) {
	      case 'D':
		negate = true;
	      case 'd':
		posixID = RETokenPOSIX.DIGIT;
		break;
	      case 'S':
		negate = true;
	      case 's':
		posixID = RETokenPOSIX.SPACE;
		break;
	      case 'W':
		negate = true;
	      case 'w':
		posixID = RETokenPOSIX.ALNUM;
		break;
	      }
	    }
            else if ("nrt".indexOf(pattern[index]) != -1) {
              switch (pattern[index]) {
                case 'n':
                  asciiEsc = '\n';
                  break;
                case 't':
                  asciiEsc = '\t';
                  break;
                case 'r':
                  asciiEsc = '\r';
                  break;
              }
            }
	    if (lastChar != 0) options.addElement(new RETokenChar(subIndex,lastChar,insens));
	    
	    if (posixID != -1) {
	      options.addElement(new RETokenPOSIX(subIndex,posixID,insens,negate));
	    } else if (asciiEsc != 0) {
	      lastChar = asciiEsc;
	    } else {
	      lastChar = pattern[index];
	    }
	    ++index;
	  } else if ((ch == '[') && (syntax.get(RESyntax.RE_CHAR_CLASSES)) && (index < pLength) && (pattern[index] == ':')) {
	    StringBuffer posixSet = new StringBuffer();
	    index = getPosixSet(pattern,index+1,posixSet);
	    int posixId = RETokenPOSIX.intValue(posixSet.toString());
	    if (posixId != -1)
	      options.addElement(new RETokenPOSIX(subIndex,posixId,insens,false));
	  } else {
	    if (lastChar != 0) options.addElement(new RETokenChar(subIndex,lastChar,insens));
	    lastChar = ch;
	  }
	  if (index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	} // while in list
	// Out of list, index is one past ']'
	    
	if (lastChar != 0) options.addElement(new RETokenChar(subIndex,lastChar,insens));
	    
	// Create a new RETokenOneOf
	addToken(currentToken);
	options.trimToSize();
	currentToken = new RETokenOneOf(subIndex,options,negative);
      }

      // SUBEXPRESSIONS
      //  (...) | \(...\) depending on RE_NO_BK_PARENS

      else if ((unit.ch == '(') && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ (unit.bk || quot))) {
	boolean pure = false;
	boolean comment = false;
        boolean lookAhead = false;
        boolean negativelh = false;
	if ((index+1 < pLength) && (pattern[index] == '?')) {
	  switch (pattern[index+1]) {
          case '!':
            if (syntax.get(RESyntax.RE_LOOKAHEAD)) {
              pure = true;
              negativelh = true;
              lookAhead = true;
              index += 2;
            }
            break;
          case '=':
            if (syntax.get(RESyntax.RE_LOOKAHEAD)) {
              pure = true;
              lookAhead = true;
              index += 2;
            }
            break;
	  case ':':
	    if (syntax.get(RESyntax.RE_PURE_GROUPING)) {
	      pure = true;
	      index += 2;
	    }
	    break;
	  case '#':
	    if (syntax.get(RESyntax.RE_COMMENTS)) {
	      comment = true;
	    }
	    break;
          default:
            throw new REException(getLocalizedMessage("repeat.no.token"), REException.REG_BADRPT, index);
	  }
	}

	if (index >= pLength) {
	    throw new REException(getLocalizedMessage("unmatched.paren"), REException.REG_ESUBREG,index);
	}

	// find end of subexpression
	int endIndex = index;
	int nextIndex = index;
	int nested = 0;

	while ( ((nextIndex = getCharUnit(pattern,endIndex,unit,false)) > 0)
		&& !(nested == 0 && (unit.ch == ')') && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ (unit.bk || quot))) )
	  if ((endIndex = nextIndex) >= pLength)
	    throw new REException(getLocalizedMessage("subexpr.no.end"),REException.REG_ESUBREG,nextIndex);
	  else if (unit.ch == '(' && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ (unit.bk || quot)))
	    nested++;
	  else if (unit.ch == ')' && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ (unit.bk || quot)))
	    nested--;

	// endIndex is now position at a ')','\)' 
	// nextIndex is end of string or position after ')' or '\)'

	if (comment) index = nextIndex;
	else { // not a comment
	  // create RE subexpression as token.
	  addToken(currentToken);
	  if (!pure) {
	    numSubs++;
	  }

	  int useIndex = (pure || lookAhead) ? 0 : nextSub + numSubs;
	  currentToken = new RE(String.valueOf(pattern,index,endIndex-index).toCharArray(),cflags,syntax,useIndex,nextSub + numSubs);
	  numSubs += ((RE) currentToken).getNumSubs();

          if (lookAhead) {
	      currentToken = new RETokenLookAhead(currentToken,negativelh);
	  }

	  index = nextIndex;
	} // not a comment
      } // subexpression
    
      // UNMATCHED RIGHT PAREN
      // ) or \) throw exception if
      // !syntax.get(RESyntax.RE_UNMATCHED_RIGHT_PAREN_ORD)
      else if (!syntax.get(RESyntax.RE_UNMATCHED_RIGHT_PAREN_ORD) && ((unit.ch == ')') && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ (unit.bk || quot)))) {
	throw new REException(getLocalizedMessage("unmatched.paren"),REException.REG_EPAREN,index);
      }

      // START OF LINE OPERATOR
      //  ^

      else if ((unit.ch == '^') && !(unit.bk || quot)) {
	addToken(currentToken);
	currentToken = null;
	addToken(new RETokenStart(subIndex,((cflags & REG_MULTILINE) > 0) ? syntax.getLineSeparator() : null));
      }

      // END OF LINE OPERATOR
      //  $

      else if ((unit.ch == '$') && !(unit.bk || quot)) {
	addToken(currentToken);
	currentToken = null;
	addToken(new RETokenEnd(subIndex,((cflags & REG_MULTILINE) > 0) ? syntax.getLineSeparator() : null));
      }

      // MATCH-ANY-CHARACTER OPERATOR (except possibly newline and null)
      //  .

      else if ((unit.ch == '.') && !(unit.bk || quot)) {
	addToken(currentToken);
	currentToken = new RETokenAny(subIndex,syntax.get(RESyntax.RE_DOT_NEWLINE) || ((cflags & REG_DOT_NEWLINE) > 0),syntax.get(RESyntax.RE_DOT_NOT_NULL));
      }

      // ZERO-OR-MORE REPEAT OPERATOR
      //  *

      else if ((unit.ch == '*') && !(unit.bk || quot)) {
	if (currentToken == null)
          throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);
	if (currentToken instanceof RETokenRepeated)
          throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,index);
	if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
	  throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,index);
	if (currentToken.getMinimumLength() == 0)
	  throw new REException(getLocalizedMessage("repeat.empty.token"),REException.REG_BADRPT,index);
	currentToken = setRepeated(currentToken,0,Integer.MAX_VALUE,index);
      }

      // ONE-OR-MORE REPEAT OPERATOR / POSSESSIVE MATCHING OPERATOR
      //  + | \+ depending on RE_BK_PLUS_QM
      //  not available if RE_LIMITED_OPS is set

      else if ((unit.ch == '+') && !syntax.get(RESyntax.RE_LIMITED_OPS) && (!syntax.get(RESyntax.RE_BK_PLUS_QM) ^ (unit.bk || quot))) {
	if (currentToken == null)
          throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);
	
	// Check for possessive matching on RETokenRepeated
	if (currentToken instanceof RETokenRepeated) {
	  RETokenRepeated tokenRep = (RETokenRepeated)currentToken;
	  if (syntax.get(RESyntax.RE_POSSESSIVE_OPS) && !tokenRep.isPossessive() && !tokenRep.isStingy())
	    tokenRep.makePossessive();
	  else
	    throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,index);

	}
	else if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
	  throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,index);
	else if (currentToken.getMinimumLength() == 0)
	  throw new REException(getLocalizedMessage("repeat.empty.token"),REException.REG_BADRPT,index);
	else
	  currentToken = setRepeated(currentToken,1,Integer.MAX_VALUE,index);
      }

      // ZERO-OR-ONE REPEAT OPERATOR / STINGY MATCHING OPERATOR
      //  ? | \? depending on RE_BK_PLUS_QM
      //  not available if RE_LIMITED_OPS is set
      //  stingy matching if RE_STINGY_OPS is set and it follows a quantifier

      else if ((unit.ch == '?') && !syntax.get(RESyntax.RE_LIMITED_OPS) && (!syntax.get(RESyntax.RE_BK_PLUS_QM) ^ (unit.bk || quot))) {
	if (currentToken == null) throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);

	// Check for stingy matching on RETokenRepeated
	if (currentToken instanceof RETokenRepeated) {
	  RETokenRepeated tokenRep = (RETokenRepeated)currentToken;
	  if (syntax.get(RESyntax.RE_STINGY_OPS) && !tokenRep.isStingy() && !tokenRep.isPossessive())
	    tokenRep.makeStingy();
	  else
	    throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,index);
	}
	else if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
	  throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,index);
	else
	  currentToken = setRepeated(currentToken,0,1,index);
      }
	
      // BACKREFERENCE OPERATOR
      //  \1 \2 ... \9
      // not available if RE_NO_BK_REFS is set

      else if (unit.bk && Character.isDigit(unit.ch) && !syntax.get(RESyntax.RE_NO_BK_REFS)) {
	addToken(currentToken);
	currentToken = new RETokenBackRef(subIndex,Character.digit(unit.ch,10),insens);
      }

      // START OF STRING OPERATOR
      //  \A if RE_STRING_ANCHORS is set
      
      else if (unit.bk && (unit.ch == 'A') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	addToken(currentToken);
	currentToken = new RETokenStart(subIndex,null);
      }

      // WORD BREAK OPERATOR
      //  \b if ????

      else if (unit.bk && (unit.ch == 'b') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.BEGIN | RETokenWordBoundary.END, false);
      } 

      // WORD BEGIN OPERATOR 
      //  \< if ????
      else if (unit.bk && (unit.ch == '<')) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.BEGIN, false);
      } 

      // WORD END OPERATOR 
      //  \> if ????
      else if (unit.bk && (unit.ch == '>')) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.END, false);
      } 

      // NON-WORD BREAK OPERATOR
      // \B if ????

      else if (unit.bk && (unit.ch == 'B') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.BEGIN | RETokenWordBoundary.END, true);
      } 

      
      // DIGIT OPERATOR
      //  \d if RE_CHAR_CLASS_ESCAPES is set
      
      else if (unit.bk && (unit.ch == 'd') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	addToken(currentToken);
	currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.DIGIT,insens,false);
      }

      // NON-DIGIT OPERATOR
      //  \D

	else if (unit.bk && (unit.ch == 'D') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.DIGIT,insens,true);
	}

	// NEWLINE ESCAPE
        //  \n

	else if (unit.bk && (unit.ch == 'n')) {
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,'\n',false);
	}

	// RETURN ESCAPE
        //  \r

	else if (unit.bk && (unit.ch == 'r')) {
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,'\r',false);
	}

	// WHITESPACE OPERATOR
        //  \s if RE_CHAR_CLASS_ESCAPES is set

	else if (unit.bk && (unit.ch == 's') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.SPACE,insens,false);
	}

	// NON-WHITESPACE OPERATOR
        //  \S

	else if (unit.bk && (unit.ch == 'S') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.SPACE,insens,true);
	}

	// TAB ESCAPE
        //  \t

	else if (unit.bk && (unit.ch == 't')) {
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,'\t',false);
	}

	// ALPHANUMERIC OPERATOR
        //  \w

	else if (unit.bk && (unit.ch == 'w') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.ALNUM,insens,false);
	}

	// NON-ALPHANUMERIC OPERATOR
        //  \W

	else if (unit.bk && (unit.ch == 'W') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.ALNUM,insens,true);
	}

	// END OF STRING OPERATOR
        //  \Z

	else if (unit.bk && (unit.ch == 'Z') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	  addToken(currentToken);
	  currentToken = new RETokenEnd(subIndex,null);
	}

	// NON-SPECIAL CHARACTER (or escape to make literal)
        //  c | \* for example

	else {  // not a special character
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,unit.ch,insens);
	} 
      } // end while

    // Add final buffered token and an EndSub marker
    addToken(currentToken);
      
    if (branches != null) {
	branches.addElement(new RE(firstToken,lastToken,numSubs,subIndex,minimumLength));
	branches.trimToSize(); // compact the Vector
	minimumLength = 0;
	firstToken = lastToken = null;
	addToken(new RETokenOneOf(subIndex,branches,false));
    } 
    else addToken(new RETokenEndSub(subIndex));

  }

  private static int getCharUnit(char[] input, int index, CharUnit unit, boolean quot) throws REException {
    unit.ch = input[index++];
    if (unit.bk = (unit.ch == '\\' && (!quot || index >= input.length || input[index] == 'E')))
      if (index < input.length)
	unit.ch = input[index++];
      else throw new REException(getLocalizedMessage("ends.with.backslash"),REException.REG_ESCAPE,index);
    return index;
  }

  /**
   * Checks if the regular expression matches the input in its entirety.
   *
   * @param input The input text.
   */
  public boolean isMatch(Object input) {
    return isMatch(input,0,0);
  }
  
  /**
   * Checks if the input string, starting from index, is an exact match of
   * this regular expression.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   */
  public boolean isMatch(Object input,int index) {
    return isMatch(input,index,0);
  }
  

  /**
   * Checks if the input, starting from index and using the specified
   * execution flags, is an exact match of this regular expression.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   * @param eflags The logical OR of any execution flags above.
   */
  public boolean isMatch(Object input,int index,int eflags) {
    return isMatchImpl(makeCharIndexed(input,index),index,eflags);
  }

  private boolean isMatchImpl(CharIndexed input, int index, int eflags) {
    if (firstToken == null)  // Trivial case
      return (input.charAt(0) == CharIndexed.OUT_OF_BOUNDS);
    REMatch m = new REMatch(numSubs, index, eflags);
    if (firstToken.match(input, m)) {
	while (m != null) {
	    if (input.charAt(m.index) == CharIndexed.OUT_OF_BOUNDS) {
		return true;
	    }
	    m = m.next;
	}
    }
    return false;
  }
    
  /**
   * Returns the maximum number of subexpressions in this regular expression.
   * If the expression contains branches, the value returned will be the
   * maximum subexpressions in any of the branches.
   */
  public int getNumSubs() {
    return numSubs;
  }

  // Overrides REToken.setUncle
  void setUncle(REToken uncle) {
      if (lastToken != null) {
	  lastToken.setUncle(uncle);
      } else super.setUncle(uncle); // to deal with empty subexpressions
  }

  // Overrides REToken.chain

  boolean chain(REToken next) {
    super.chain(next);
    setUncle(next);
    return true;
  }

  /**
   * Returns the minimum number of characters that could possibly
   * constitute a match of this regular expression.
   */
  public int getMinimumLength() {
      return minimumLength;
  }

  /**
   * Returns an array of all matches found in the input.
   *
   * If the regular expression allows the empty string to match, it will
   * substitute matches at all positions except the end of the input.
   *
   * @param input The input text.
   * @return a non-null (but possibly zero-length) array of matches
   */
  public REMatch[] getAllMatches(Object input) {
    return getAllMatches(input,0,0);
  }

  /**
   * Returns an array of all matches found in the input,
   * beginning at the specified index position.
   *
   * If the regular expression allows the empty string to match, it will
   * substitute matches at all positions except the end of the input.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   * @return a non-null (but possibly zero-length) array of matches
   */
  public REMatch[] getAllMatches(Object input, int index) {
    return getAllMatches(input,index,0);
  }

  /**
   * Returns an array of all matches found in the input string,
   * beginning at the specified index position and using the specified
   * execution flags.
   *
   * If the regular expression allows the empty string to match, it will
   * substitute matches at all positions except the end of the input.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   * @param eflags The logical OR of any execution flags above.
   * @return a non-null (but possibly zero-length) array of matches
   */
  public REMatch[] getAllMatches(Object input, int index, int eflags) {
    return getAllMatchesImpl(makeCharIndexed(input,index),index,eflags);
  }

  // this has been changed since 1.03 to be non-overlapping matches
  private REMatch[] getAllMatchesImpl(CharIndexed input, int index, int eflags) {
    Vector all = new Vector();
    REMatch m = null;
    while ((m = getMatchImpl(input,index,eflags,null)) != null) {
      all.addElement(m);
      index = m.getEndIndex();
      if (m.end[0] == 0) {   // handle pathological case of zero-length match
	index++;
	input.move(1);
      } else {
	input.move(m.end[0]);
      }
      if (!input.isValid()) break;
    }
    REMatch[] mset = new REMatch[all.size()];
    all.copyInto(mset);
    return mset;
  }
  
    /* Implements abstract method REToken.match() */
    boolean match(CharIndexed input, REMatch mymatch) { 
	if (firstToken == null) return next(input, mymatch);

	// Note the start of this subexpression
	mymatch.start[subIndex] = mymatch.index;

	return firstToken.match(input, mymatch);
    }
  
  /**
   * Returns the first match found in the input.  If no match is found,
   * null is returned.
   *
   * @param input The input text.
   * @return An REMatch instance referencing the match, or null if none.
   */
  public REMatch getMatch(Object input) {
    return getMatch(input,0,0);
  }
  
  /**
   * Returns the first match found in the input, beginning
   * the search at the specified index.  If no match is found,
   * returns null.
   *
   * @param input The input text.
   * @param index The offset within the text to begin looking for a match.
   * @return An REMatch instance referencing the match, or null if none.
   */
  public REMatch getMatch(Object input, int index) {
    return getMatch(input,index,0);
  }
  
  /**
   * Returns the first match found in the input, beginning
   * the search at the specified index, and using the specified
   * execution flags.  If no match is found, returns null.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   * @param eflags The logical OR of any execution flags above.
   * @return An REMatch instance referencing the match, or null if none.
   */
  public REMatch getMatch(Object input, int index, int eflags) {
    return getMatch(input,index,eflags,null);
  }

  /**
   * Returns the first match found in the input, beginning the search
   * at the specified index, and using the specified execution flags.
   * If no match is found, returns null.  If a StringBuffer is
   * provided and is non-null, the contents of the input text from the
   * index to the beginning of the match (or to the end of the input,
   * if there is no match) are appended to the StringBuffer.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   * @param eflags The logical OR of any execution flags above.
   * @param buffer The StringBuffer to save pre-match text in.
   * @return An REMatch instance referencing the match, or null if none.  */
  public REMatch getMatch(Object input, int index, int eflags, StringBuffer buffer) {
    return getMatchImpl(makeCharIndexed(input,index),index,eflags,buffer);
  }

  REMatch getMatchImpl(CharIndexed input, int anchor, int eflags, StringBuffer buffer) {
      // Create a new REMatch to hold results
      REMatch mymatch = new REMatch(numSubs, anchor, eflags);
      do {
	  // Optimization: check if anchor + minimumLength > length
	  if (minimumLength == 0 || input.charAt(minimumLength-1) != CharIndexed.OUT_OF_BOUNDS) {
	      if (match(input, mymatch)) {
		  // Find longest match of them all to observe leftmost longest
		  REMatch longest = mymatch;
		  while ((mymatch = mymatch.next) != null) {
		      if (mymatch.index > longest.index) {
			  longest = mymatch;
		      }
		  }
		  
		  longest.end[0] = longest.index;
		  longest.finish(input);
		  return longest;
	      }
	  }
	  mymatch.clear(++anchor);
	  // Append character to buffer if needed
	  if (buffer != null && input.charAt(0) != CharIndexed.OUT_OF_BOUNDS) {
	      buffer.append(input.charAt(0));
	  }
      } while (input.move(1));
      
      // Special handling at end of input for e.g. "$"
      if (minimumLength == 0) {
	  if (match(input, mymatch)) {
	      mymatch.finish(input);
	      return mymatch;
	  }
      }

      return null;
  }

  /**
   * Returns an REMatchEnumeration that can be used to iterate over the
   * matches found in the input text.
   *
   * @param input The input text.
   * @return A non-null REMatchEnumeration instance.
   */
  public REMatchEnumeration getMatchEnumeration(Object input) {
    return getMatchEnumeration(input,0,0);
  }


  /**
   * Returns an REMatchEnumeration that can be used to iterate over the
   * matches found in the input text.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   * @return A non-null REMatchEnumeration instance, with its input cursor
   *  set to the index position specified.
   */
  public REMatchEnumeration getMatchEnumeration(Object input, int index) {
    return getMatchEnumeration(input,index,0);
  }

  /**
   * Returns an REMatchEnumeration that can be used to iterate over the
   * matches found in the input text.
   *
   * @param input The input text.
   * @param index The offset index at which the search should be begin.
   * @param eflags The logical OR of any execution flags above.
   * @return A non-null REMatchEnumeration instance, with its input cursor
   *  set to the index position specified.
   */
  public REMatchEnumeration getMatchEnumeration(Object input, int index, int eflags) {
    return new REMatchEnumeration(this,makeCharIndexed(input,index),index,eflags);
  }


  /**
   * Substitutes the replacement text for the first match found in the input.
   *
   * @param input The input text.
   * @param replace The replacement text, which may contain $x metacharacters (see REMatch.substituteInto).
   * @return A String interpolating the substituted text.
   * @see REMatch#substituteInto
   */
  public String substitute(Object input,String replace) {
    return substitute(input,replace,0,0);
  }

  /**
   * Substitutes the replacement text for the first match found in the input
   * beginning at the specified index position.  Specifying an index
   * effectively causes the regular expression engine to throw away the
   * specified number of characters. 
   *
   * @param input The input text.
   * @param replace The replacement text, which may contain $x metacharacters (see REMatch.substituteInto).
   * @param index The offset index at which the search should be begin.
   * @return A String containing the substring of the input, starting
   *   at the index position, and interpolating the substituted text.
   * @see REMatch#substituteInto
   */
  public String substitute(Object input,String replace,int index) {
    return substitute(input,replace,index,0);
  }

  /**
   * Substitutes the replacement text for the first match found in the input
   * string, beginning at the specified index position and using the
   * specified execution flags.
   *
   * @param input The input text.
   * @param replace The replacement text, which may contain $x metacharacters (see REMatch.substituteInto).
   * @param index The offset index at which the search should be begin.
   * @param eflags The logical OR of any execution flags above.
   * @return A String containing the substring of the input, starting
   *   at the index position, and interpolating the substituted text.
   * @see REMatch#substituteInto
   */
  public String substitute(Object input,String replace,int index,int eflags) {
    return substituteImpl(makeCharIndexed(input,index),replace,index,eflags);
  }

  private String substituteImpl(CharIndexed input,String replace,int index,int eflags) {
    StringBuffer buffer = new StringBuffer();
    REMatch m = getMatchImpl(input,index,eflags,buffer);
    if (m==null) return buffer.toString();
    buffer.append( ((eflags & REG_NO_INTERPOLATE) > 0) ?
		   replace : m.substituteInto(replace) );
    if (input.move(m.end[0])) {
      do {
	buffer.append(input.charAt(0));
      } while (input.move(1));
    }
    return buffer.toString();
  }
  
  /**
   * Substitutes the replacement text for each non-overlapping match found 
   * in the input text.
   *
   * @param input The input text.
   * @param replace The replacement text, which may contain $x metacharacters (see REMatch.substituteInto).
   * @return A String interpolating the substituted text.
   * @see REMatch#substituteInto
   */
  public String substituteAll(Object input,String replace) {
    return substituteAll(input,replace,0,0);
  }

  /**
   * Substitutes the replacement text for each non-overlapping match found 
   * in the input text, starting at the specified index.
   *
   * If the regular expression allows the empty string to match, it will
   * substitute matches at all positions except the end of the input.
   *
   * @param input The input text.
   * @param replace The replacement text, which may contain $x metacharacters (see REMatch.substituteInto).
   * @param index The offset index at which the search should be begin.
   * @return A String containing the substring of the input, starting
   *   at the index position, and interpolating the substituted text.
   * @see REMatch#substituteInto
   */
  public String substituteAll(Object input,String replace,int index) {
    return substituteAll(input,replace,index,0);
  }
 
  /**
   * Substitutes the replacement text for each non-overlapping match found 
   * in the input text, starting at the specified index and using the
   * specified execution flags.
   *
   * @param input The input text.
   * @param replace The replacement text, which may contain $x metacharacters (see REMatch.substituteInto).
   * @param index The offset index at which the search should be begin.
   * @param eflags The logical OR of any execution flags above.
   * @return A String containing the substring of the input, starting
   *   at the index position, and interpolating the substituted text.
   * @see REMatch#substituteInto
   */
  public String substituteAll(Object input,String replace,int index,int eflags) {
    return substituteAllImpl(makeCharIndexed(input,index),replace,index,eflags);
  }

  private String substituteAllImpl(CharIndexed input,String replace,int index,int eflags) {
    StringBuffer buffer = new StringBuffer();
    REMatch m;
    while ((m = getMatchImpl(input,index,eflags,buffer)) != null) {
	buffer.append( ((eflags & REG_NO_INTERPOLATE) > 0) ?
		       replace : m.substituteInto(replace) );
      index = m.getEndIndex();
      if (m.end[0] == 0) {
	char ch = input.charAt(0);
	if (ch != CharIndexed.OUT_OF_BOUNDS) 
	    buffer.append(ch);
	input.move(1);
      } else {
	  input.move(m.end[0]);
      }

      if (!input.isValid()) break;
    }
    return buffer.toString();
  }
  
  /* Helper function for constructor */
  private void addToken(REToken next) {
    if (next == null) return;
    minimumLength += next.getMinimumLength();
    if (firstToken == null) {
	lastToken = firstToken = next;
    } else {
      // if chain returns false, it "rejected" the token due to
      // an optimization, and next was combined with lastToken
      if (lastToken.chain(next)) {
	  lastToken = next;
      }
    }
  }

  private static REToken setRepeated(REToken current, int min, int max, int index) throws REException {
    if (current == null) throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);
    return new RETokenRepeated(current.subIndex,current,min,max);
  }

  private static int getPosixSet(char[] pattern,int index,StringBuffer buf) {
    // Precondition: pattern[index-1] == ':'
    // we will return pos of closing ']'.
    int i;
    for (i=index; i<(pattern.length-1); i++) {
      if ((pattern[i] == ':') && (pattern[i+1] == ']'))
	return i+2;
      buf.append(pattern[i]);
    }
    return index; // didn't match up
  }

  private int getMinMax(char[] input,int index,IntPair minMax,RESyntax syntax) throws REException {
    // Precondition: input[index-1] == '{', minMax != null

    boolean mustMatch = !syntax.get(RESyntax.RE_NO_BK_BRACES);
    int startIndex = index;
    if (index == input.length) {
      if (mustMatch)
        throw new REException(getLocalizedMessage("unmatched.brace"),REException.REG_EBRACE,index);
      else
        return startIndex;
    }
    
    int min,max=0;
    CharUnit unit = new CharUnit();
    StringBuffer buf = new StringBuffer();
    
    // Read string of digits
    do {
      index = getCharUnit(input,index,unit,false);
      if (Character.isDigit(unit.ch))
        buf.append(unit.ch);
    } while ((index != input.length) && Character.isDigit(unit.ch));

    // Check for {} tomfoolery
    if (buf.length() == 0) {
      if (mustMatch)
        throw new REException(getLocalizedMessage("interval.error"),REException.REG_EBRACE,index);
      else
        return startIndex;
    }

    min = Integer.parseInt(buf.toString());
	
    if ((unit.ch == '}') && (syntax.get(RESyntax.RE_NO_BK_BRACES) ^ unit.bk))
      max = min;
    else if (index == input.length)
      if (mustMatch)
        throw new REException(getLocalizedMessage("interval.no.end"),REException.REG_EBRACE,index);
      else
        return startIndex;
    else if ((unit.ch == ',') && !unit.bk) {
      buf = new StringBuffer();
      // Read string of digits
      while (((index = getCharUnit(input,index,unit,false)) != input.length) && Character.isDigit(unit.ch))
	buf.append(unit.ch);

      if (!((unit.ch == '}') && (syntax.get(RESyntax.RE_NO_BK_BRACES) ^ unit.bk)))
        if (mustMatch)
          throw new REException(getLocalizedMessage("interval.error"),REException.REG_EBRACE,index);
        else
          return startIndex;

      // This is the case of {x,}
      if (buf.length() == 0) max = Integer.MAX_VALUE;
      else max = Integer.parseInt(buf.toString());
    } else
      if (mustMatch)
        throw new REException(getLocalizedMessage("interval.error"),REException.REG_EBRACE,index);
      else
        return startIndex;

    // We know min and max now, and they are valid.

    minMax.first = min;
    minMax.second = max;

    // return the index following the '}'
    return index;
  }

   /**
    * Return a human readable form of the compiled regular expression,
    * useful for debugging.
    */
   public String toString() {
     StringBuffer sb = new StringBuffer();
     dump(sb);
     return sb.toString();
   }

  void dump(StringBuffer os) {
    os.append('(');
    if (subIndex == 0)
      os.append("?:");
    if (firstToken != null)
      firstToken.dumpAll(os);
    os.append(')');
  }

  // Cast input appropriately or throw exception
  private static CharIndexed makeCharIndexed(Object input, int index) {
      // We could let a String fall through to final input, but since
      // it's the most likely input type, we check it first.
    if (input instanceof String)
      return new CharIndexedString((String) input,index);
    else if (input instanceof char[])
      return new CharIndexedCharArray((char[]) input,index);
    else if (input instanceof StringBuffer)
      return new CharIndexedStringBuffer((StringBuffer) input,index);
    else if (input instanceof InputStream)
      return new CharIndexedInputStream((InputStream) input,index);
    else if (input instanceof CharIndexed)
	return (CharIndexed) input; // do we lose index info?
    else 
	return new CharIndexedString(input.toString(), index);
  }
}
