/* gnu/regexp/UncheckedRE.java
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

package gnu.java.util.regex;

/**
 * UncheckedRE is a subclass of RE that allows programmers an easier means
 * of programmatically precompiling regular expressions.  It is constructed
 * and used in exactly the same manner as an instance of the RE class; the
 * only difference is that its constructors do not throw REException.
 * Instead, if a syntax error is encountered during construction, a
 * RuntimeException will be thrown.
 * <P>
 * Note that this makes UncheckedRE dangerous if constructed with
 * dynamic data.  Do not use UncheckedRE unless you are completely sure
 * that all input being passed to it contains valid, well-formed 
 * regular expressions for the syntax specified.
 *
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A>
 * @see gnu.java.util.regex.RE 
 * @since gnu.regexp 1.1.4
 */

public final class UncheckedRE extends RE {
  /**
   * Constructs a regular expression pattern buffer without any compilation
   * flags set, and using the default syntax (RESyntax.RE_SYNTAX_PERL5).
   *
   * @param pattern A regular expression pattern, in the form of a String,
   *   StringBuffer or char[].  Other input types will be converted to
   *   strings using the toString() method.
   * @exception RuntimeException The input pattern could not be parsed.
   * @exception NullPointerException The pattern was null.
   */
  public UncheckedRE(Object pattern) {
      this(pattern,0,RESyntax.RE_SYNTAX_PERL5);
  }

  /**
   * Constructs a regular expression pattern buffer using the specified
   * compilation flags and the default syntax (RESyntax.RE_SYNTAX_PERL5).
   *
   * @param pattern A regular expression pattern, in the form of a String,
   *   StringBuffer, or char[].  Other input types will be converted to
   *   strings using the toString() method.
   * @param cflags The logical OR of any combination of the compilation flags in the RE class.
   * @exception RuntimeException The input pattern could not be parsed.
   * @exception NullPointerException The pattern was null.
   */
  public UncheckedRE(Object pattern, int cflags) {
      this(pattern,cflags,RESyntax.RE_SYNTAX_PERL5);
  }

  /**
   * Constructs a regular expression pattern buffer using the specified
   * compilation flags and regular expression syntax.
   *
   * @param pattern A regular expression pattern, in the form of a String,
   *   StringBuffer, or char[].  Other input types will be converted to
   *   strings using the toString() method.
   * @param cflags The logical OR of any combination of the compilation flags in the RE class.
   * @param syntax The type of regular expression syntax to use.
   * @exception RuntimeException The input pattern could not be parsed.
   * @exception NullPointerException The pattern was null.
   */
  public UncheckedRE(Object pattern, int cflags, RESyntax syntax) {
      try {
	  initialize(pattern,cflags,syntax,0,0);
      } catch (REException e) { 
	  throw new RuntimeException(e.getMessage());
      }
  }
}


