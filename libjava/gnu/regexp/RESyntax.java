/* gnu/regexp/RESyntax.java
   Copyright (C) 1998-2002, 2004 Free Software Foundation, Inc.

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
import java.io.Serializable;
import java.util.BitSet;

/**
 * An RESyntax specifies the way a regular expression will be compiled.
 * This class provides a number of predefined useful constants for
 * emulating popular regular expression syntaxes.  Additionally the
 * user may construct his or her own syntax, using any combination of the
 * syntax bit constants.  The syntax is an optional argument to any of the
 * matching methods on class RE.
 *
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A>
 */

public final class RESyntax implements Serializable {
    static final String DEFAULT_LINE_SEPARATOR = System.getProperty("line.separator");

    private static final String SYNTAX_IS_FINAL = RE.getLocalizedMessage("syntax.final");

    private BitSet bits;

    // true for the constant defined syntaxes
    private boolean isFinal = false;

    private String lineSeparator = DEFAULT_LINE_SEPARATOR;

  // Values for constants are bit indexes

  /**
   * Syntax bit. Backslash is an escape character in lists.
   */
  public static final int RE_BACKSLASH_ESCAPE_IN_LISTS =  0;

  /**
   * Syntax bit. Use \? instead of ? and \+ instead of +.
   */
  public static final int RE_BK_PLUS_QM                =  1;

  /**
   * Syntax bit. POSIX character classes ([:...:]) in lists are allowed.
   */
  public static final int RE_CHAR_CLASSES              =  2;

  /**
   * Syntax bit. ^ and $ are special everywhere.
   * <B>Not implemented.</B>
   */
  public static final int RE_CONTEXT_INDEP_ANCHORS     =  3; 

  /**
   * Syntax bit. Repetition operators are only special in valid positions.
   * <B>Not implemented.</B>
   */
  public static final int RE_CONTEXT_INDEP_OPS         =  4; 

  /**
   * Syntax bit. Repetition and alternation operators are invalid
   * at start and end of pattern and other places. 
   * <B>Not implemented</B>.
   */
  public static final int RE_CONTEXT_INVALID_OPS       =  5; 

  /**
   * Syntax bit. Match-any-character operator (.) matches a newline.
   */
  public static final int RE_DOT_NEWLINE               =  6;

  /**
   * Syntax bit. Match-any-character operator (.) does not match a null.
   */
  public static final int RE_DOT_NOT_NULL              =  7;

  /**
   * Syntax bit. Intervals ({x}, {x,}, {x,y}) are allowed.
   */
  public static final int RE_INTERVALS                 =  8;

  /**
   * Syntax bit. No alternation (|), match one-or-more (+), or 
   * match zero-or-one (?) operators.
   */
  public static final int RE_LIMITED_OPS               =  9;

  /**
   * Syntax bit. Newline is an alternation operator.
   */
  public static final int RE_NEWLINE_ALT               = 10; // impl.

  /**
   * Syntax bit. Intervals use { } instead of \{ \}
   */
  public static final int RE_NO_BK_BRACES              = 11; 

  /**
   * Syntax bit. Grouping uses ( ) instead of \( \).
   */
  public static final int RE_NO_BK_PARENS              = 12;

  /**
   * Syntax bit. Backreferences not allowed.
   */
  public static final int RE_NO_BK_REFS                = 13;

  /**
   * Syntax bit. Alternation uses | instead of \|
   */
  public static final int RE_NO_BK_VBAR                = 14;

  /**
   * Syntax bit. <B>Not implemented</B>.
   */
  public static final int RE_NO_EMPTY_RANGES           = 15;

  /**
   * Syntax bit. An unmatched right parenthesis (')' or '\)', depending
   * on RE_NO_BK_PARENS) will throw an exception when compiling.
   */
  public static final int RE_UNMATCHED_RIGHT_PAREN_ORD = 16;

  /**
   * Syntax bit. <B>Not implemented.</B>
   */
  public static final int RE_HAT_LISTS_NOT_NEWLINE     = 17;

  /**
   * Syntax bit.  Stingy matching is allowed (+?, *?, ??, {x,y}?).
   */
  public static final int RE_STINGY_OPS                = 18;

  /**
   * Syntax bit. Allow character class escapes (\d, \D, \s, \S, \w, \W).
   */
  public static final int RE_CHAR_CLASS_ESCAPES        = 19;

  /**
   * Syntax bit. Allow use of (?:xxx) grouping (subexpression is not saved).
   */
  public static final int RE_PURE_GROUPING             = 20;

  /**
   * Syntax bit. Allow use of (?=xxx) and (?!xxx) apply the subexpression
   * to the text following the current position without consuming that text.
   */
  public static final int RE_LOOKAHEAD                 = 21;

  /**
   * Syntax bit. Allow beginning- and end-of-string anchors (\A, \Z).
   */
  public static final int RE_STRING_ANCHORS            = 22;

  /**
   * Syntax bit. Allow embedded comments, (?#comment), as in Perl5.
   */
  public static final int RE_COMMENTS                  = 23;

  /**
   * Syntax bit. Allow character class escapes within lists, as in Perl5.
   */
  public static final int RE_CHAR_CLASS_ESC_IN_LISTS   = 24;

  private static final int BIT_TOTAL                   = 25;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the awk utility.
   */
  public static final RESyntax RE_SYNTAX_AWK;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the ed utility.
   */
  public static final RESyntax RE_SYNTAX_ED;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the egrep utility.
   */
  public static final RESyntax RE_SYNTAX_EGREP;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the GNU Emacs editor.
   */
  public static final RESyntax RE_SYNTAX_EMACS;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the grep utility.
   */
  public static final RESyntax RE_SYNTAX_GREP;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the POSIX awk specification.
   */
  public static final RESyntax RE_SYNTAX_POSIX_AWK;

  /**
   * Predefined syntax.
   * Emulates POSIX basic regular expression support.
   */
  public static final RESyntax RE_SYNTAX_POSIX_BASIC;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the POSIX egrep specification.
   */
  public static final RESyntax RE_SYNTAX_POSIX_EGREP;

  /**
   * Predefined syntax.
   * Emulates POSIX extended regular expression support.
   */
  public static final RESyntax RE_SYNTAX_POSIX_EXTENDED;

  /**
   * Predefined syntax.
   * Emulates POSIX basic minimal regular expressions.
   */
  public static final RESyntax RE_SYNTAX_POSIX_MINIMAL_BASIC;

  /**
   * Predefined syntax.
   * Emulates POSIX extended minimal regular expressions.
   */
  public static final RESyntax RE_SYNTAX_POSIX_MINIMAL_EXTENDED;

  /**
   * Predefined syntax.
   * Emulates regular expression support in the sed utility.
   */
  public static final RESyntax RE_SYNTAX_SED;

  /**
   * Predefined syntax.
   * Emulates regular expression support in Larry Wall's perl, version 4,
   */
  public static final RESyntax RE_SYNTAX_PERL4;

  /**
   * Predefined syntax.
   * Emulates regular expression support in Larry Wall's perl, version 4,
   * using single line mode (/s modifier).
   */
  public static final RESyntax RE_SYNTAX_PERL4_S; // single line mode (/s)

  /**
   * Predefined syntax.
   * Emulates regular expression support in Larry Wall's perl, version 5.
   */
  public static final RESyntax RE_SYNTAX_PERL5;  

  /**
   * Predefined syntax.
   * Emulates regular expression support in Larry Wall's perl, version 5,
   * using single line mode (/s modifier).
   */
  public static final RESyntax RE_SYNTAX_PERL5_S;

    /**
     * Predefined syntax.
     * Emulates regular expression support in Java 1.4's java.util.regex
     * package.
     */
    public static final RESyntax RE_SYNTAX_JAVA_1_4;

  static {
      // Define syntaxes
      
      RE_SYNTAX_EMACS = new RESyntax().makeFinal();
      
      RESyntax RE_SYNTAX_POSIX_COMMON = new RESyntax()
	  .set(RE_CHAR_CLASSES)
	  .set(RE_DOT_NEWLINE)
	  .set(RE_DOT_NOT_NULL)
	  .set(RE_INTERVALS)
	  .set(RE_NO_EMPTY_RANGES)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_BASIC = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_BK_PLUS_QM)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_EXTENDED = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INDEP_OPS)
	  .set(RE_NO_BK_BRACES)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_UNMATCHED_RIGHT_PAREN_ORD)
	  .makeFinal();

      RE_SYNTAX_AWK = new RESyntax()
	  .set(RE_BACKSLASH_ESCAPE_IN_LISTS)
	  .set(RE_DOT_NOT_NULL)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_REFS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_NO_EMPTY_RANGES)
	  .set(RE_UNMATCHED_RIGHT_PAREN_ORD)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_AWK = new RESyntax(RE_SYNTAX_POSIX_EXTENDED)
	  .set(RE_BACKSLASH_ESCAPE_IN_LISTS)
	  .makeFinal();
      
      RE_SYNTAX_GREP = new RESyntax()
	  .set(RE_BK_PLUS_QM)
	  .set(RE_CHAR_CLASSES)
	  .set(RE_HAT_LISTS_NOT_NEWLINE)
	  .set(RE_INTERVALS)
	  .set(RE_NEWLINE_ALT)
	  .makeFinal();
      
      RE_SYNTAX_EGREP = new RESyntax()
	  .set(RE_CHAR_CLASSES)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INDEP_OPS)
	  .set(RE_HAT_LISTS_NOT_NEWLINE)
	  .set(RE_NEWLINE_ALT)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_VBAR)
	  .makeFinal();
    
      RE_SYNTAX_POSIX_EGREP = new RESyntax(RE_SYNTAX_EGREP)
	  .set(RE_INTERVALS)
	  .set(RE_NO_BK_BRACES)
	  .makeFinal();
    
      /* P1003.2/D11.2, section 4.20.7.1, lines 5078ff.  */
    
      RE_SYNTAX_ED = new RESyntax(RE_SYNTAX_POSIX_BASIC)
	  .makeFinal();
    
      RE_SYNTAX_SED = new RESyntax(RE_SYNTAX_POSIX_BASIC)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_MINIMAL_BASIC = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_LIMITED_OPS)
	  .makeFinal();
      
      /* Differs from RE_SYNTAX_POSIX_EXTENDED in that RE_CONTEXT_INVALID_OPS
	 replaces RE_CONTEXT_INDEP_OPS and RE_NO_BK_REFS is added. */
      
      RE_SYNTAX_POSIX_MINIMAL_EXTENDED = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INVALID_OPS)
	  .set(RE_NO_BK_BRACES)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_REFS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_UNMATCHED_RIGHT_PAREN_ORD)
	  .makeFinal();
      
      /* There is no official Perl spec, but here's a "best guess" */
      
      RE_SYNTAX_PERL4 = new RESyntax()
	  .set(RE_BACKSLASH_ESCAPE_IN_LISTS)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INDEP_OPS)          // except for '{', apparently
	  .set(RE_INTERVALS)
	  .set(RE_NO_BK_BRACES)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_NO_EMPTY_RANGES)
	  .set(RE_CHAR_CLASS_ESCAPES)    // \d,\D,\w,\W,\s,\S
	  .makeFinal();
      
      RE_SYNTAX_PERL4_S = new RESyntax(RE_SYNTAX_PERL4)
	  .set(RE_DOT_NEWLINE)
	  .makeFinal();
      
      RE_SYNTAX_PERL5 = new RESyntax(RE_SYNTAX_PERL4)
	  .set(RE_PURE_GROUPING)          // (?:)
	  .set(RE_STINGY_OPS)             // *?,??,+?,{}?
	  .set(RE_LOOKAHEAD)              // (?=)(?!)
	  .set(RE_STRING_ANCHORS)         // \A,\Z
	  .set(RE_CHAR_CLASS_ESC_IN_LISTS)// \d,\D,\w,\W,\s,\S within []
	  .set(RE_COMMENTS)              // (?#)
	  .makeFinal();
      
      RE_SYNTAX_PERL5_S = new RESyntax(RE_SYNTAX_PERL5)
	  .set(RE_DOT_NEWLINE)
	  .makeFinal();

      RE_SYNTAX_JAVA_1_4 = new RESyntax(RE_SYNTAX_PERL5)
	  // XXX
	  .makeFinal();
  }

  /**
   * Construct a new syntax object with all bits turned off.
   * This is equivalent to RE_SYNTAX_EMACS.
   */
  public RESyntax() {
    bits = new BitSet(BIT_TOTAL);
  }

    /**
     * Called internally when constructing predefined syntaxes
     * so their interpretation cannot vary.  Conceivably useful
     * for your syntaxes as well.  Causes IllegalAccessError to
     * be thrown if any attempt to modify the syntax is made.
     *
     * @return this object for convenient chaining
     */
    public RESyntax makeFinal() {
	isFinal = true;
	return this;
    }

  /**
   * Construct a new syntax object with all bits set the same 
   * as the other syntax.
   */
  public RESyntax(RESyntax other) {
    bits = (BitSet) other.bits.clone();
  }

  /**
   * Check if a given bit is set in this syntax.
   */
  public boolean get(int index) {
    return bits.get(index);
  }

  /**
   * Set a given bit in this syntax. 
   *
   * @param index the constant (RESyntax.RE_xxx) bit to set.
   * @return a reference to this object for easy chaining.
   */
  public RESyntax set(int index) {
      if (isFinal) throw new IllegalAccessError(SYNTAX_IS_FINAL);
    bits.set(index);
    return this;
  }

  /**
   * Clear a given bit in this syntax. 
   *
   * @param index the constant (RESyntax.RE_xxx) bit to clear.
   * @return a reference to this object for easy chaining.
   */
  public RESyntax clear(int index) {
      if (isFinal) throw new IllegalAccessError(SYNTAX_IS_FINAL);
      bits.clear(index);
      return this;
  }

    /**
     * Changes the line separator string for regular expressions
     * created using this RESyntax.  The default separator is the
     * value returned by the system property "line.separator", which
     * should be correct when reading platform-specific files from a
     * filesystem.  However, many programs may collect input from
     * sources where the line separator is differently specified (for
     * example, in the applet environment, the text box widget
     * interprets line breaks as single-character newlines,
     * regardless of the host platform.
     *
     * Note that setting the line separator to a character or
     * characters that have specific meaning within the current syntax
     * can cause unexpected chronosynclastic infundibula.
     *
     * @return this object for convenient chaining 
     */
    public RESyntax setLineSeparator(String aSeparator) {
	if (isFinal) throw new IllegalAccessError(SYNTAX_IS_FINAL);
	lineSeparator = aSeparator;
	return this;
    }

    /**
     * Returns the currently active line separator string.  The default
     * is the platform-dependent system property "line.separator".
     */
    public String getLineSeparator() {
	return lineSeparator;
    }
}
