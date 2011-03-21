/* Pattern.java -- Compiled regular expression ready to be applied.
   Copyright (C) 2002, 2004, 2005, 2007 Free Software Foundation, Inc.

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

package java.util.regex;

import gnu.java.util.regex.RE;
import gnu.java.util.regex.REException;
import gnu.java.util.regex.RESyntax;

import java.io.Serializable;
import java.util.ArrayList;


/**
 * Compiled regular expression ready to be applied.
 *
 * @since 1.4
 */
public final class Pattern implements Serializable
{
  private static final long serialVersionUID = 5073258162644648461L;

  public static final int CANON_EQ = 128;
  public static final int CASE_INSENSITIVE = 2;
  public static final int COMMENTS = 4;
  public static final int DOTALL = 32;
  public static final int MULTILINE = 8;
  public static final int UNICODE_CASE = 64;
  public static final int UNIX_LINES = 1;

  private final String regex;
  private final int flags;

  private final RE re;

  private Pattern (String regex, int flags)
    throws PatternSyntaxException
  {
    this.regex = regex;
    this.flags = flags;

    RESyntax syntax = RESyntax.RE_SYNTAX_JAVA_1_4;
    int gnuFlags = 0;
    gnuFlags |= RE.REG_ICASE_USASCII;
    if ((flags & CASE_INSENSITIVE) != 0)
      gnuFlags |= RE.REG_ICASE;
    if ((flags & MULTILINE) != 0)
      {
        gnuFlags |= RE.REG_MULTILINE;
        syntax = new RESyntax(syntax);
        syntax.setLineSeparator(null);
      }
    if ((flags & DOTALL) != 0)
      gnuFlags |= RE.REG_DOT_NEWLINE;
    if ((flags & UNICODE_CASE) != 0)
      gnuFlags &= ~RE.REG_ICASE_USASCII;
    // not yet supported:
    // if ((flags & CANON_EQ) != 0) gnuFlags =

    if ((flags & UNIX_LINES) != 0)
      {
        // Use a syntax set with \n for linefeeds?
        syntax = new RESyntax(syntax);
        syntax.setLineSeparator("\n");
      }

    if ((flags & COMMENTS) != 0)
      {
        gnuFlags |= RE.REG_X_COMMENTS;
      }

    try
      {
        this.re = new RE(regex, gnuFlags, syntax);
      }
    catch (REException e)
      {
        PatternSyntaxException pse;
        pse = new PatternSyntaxException(e.getMessage(),
                                         regex, e.getPosition());
        pse.initCause(e);
        throw pse;
      }
  }

  // package private accessor method
  RE getRE()
  {
    return re;
  }

  /**
   * @param regex The regular expression
   *
   * @exception PatternSyntaxException If the expression's syntax is invalid
   */
  public static Pattern compile (String regex)
    throws PatternSyntaxException
  {
    return compile(regex, 0);
  }

  /**
   * @param regex The regular expression
   * @param flags The match flags, a bit mask
   *
   * @exception PatternSyntaxException If the expression's syntax is invalid
   * @exception IllegalArgumentException If bit values other than those
   * corresponding to the defined match flags are set in flags
   */
  public static Pattern compile (String regex, int flags)
    throws PatternSyntaxException
  {
    // FIXME: check which flags are really accepted
    if ((flags & ~0xEF) != 0)
      throw new IllegalArgumentException ();

    return new Pattern (regex, flags);
  }

  public int flags ()
  {
    return this.flags;
  }

  /**
   * @param regex The regular expression
   * @param input The character sequence to be matched
   *
   * @exception PatternSyntaxException If the expression's syntax is invalid
   */
  public static boolean matches (String regex, CharSequence input)
  {
    return compile(regex).matcher(input).matches();
  }

  /**
   * @param input The character sequence to be matched
   */
  public Matcher matcher (CharSequence input)
  {
    return new Matcher(this, input);
  }

  /**
   * @param input The character sequence to be matched
   */
  public String[] split (CharSequence input)
  {
    return split(input, 0);
  }

  /**
   * @param input The character sequence to be matched
   * @param limit The result threshold
   */
  public String[] split (CharSequence input, int limit)
  {
    Matcher matcher = new Matcher(this, input);
    ArrayList<String> list = new ArrayList<String>();
    int empties = 0;
    int count = 0;
    int start = 0;
    int end;
    boolean matched = matcher.find();

    while (matched && (limit <= 0 || count < limit - 1))
      {
        ++count;
        end = matcher.start();
        if (start == end)
          empties++;
        else
          {
            while (empties > 0)
              {
                list.add("");
                empties--;
              }

            String text = input.subSequence(start, end).toString();
            list.add(text);
          }
        start = matcher.end();
        matched = matcher.find();
      }

    // We matched nothing.
    if (!matched && count == 0)
      return new String[] { input.toString() };

    // Is the last token empty?
    boolean emptyLast = (start == input.length());

    // Can/Must we add empties or an extra last token at the end?
    if (list.size() < limit || limit < 0 || (limit == 0 && !emptyLast))
      {
        if (limit > list.size())
          {
            int max = limit - list.size();
            empties = (empties > max) ? max : empties;
          }
        while (empties > 0)
          {
            list.add("");
            empties--;
          }
      }

    // last token at end
    if (limit != 0 || (limit == 0 && !emptyLast))
      {
        String t = input.subSequence(start, input.length()).toString();
        if ("".equals(t) && limit == 0)
          { /* Don't add. */ }
        else
          list.add(t);
      }

    return list.toArray(new String[list.size()]);
  }

  public String pattern ()
  {
    return regex;
  }

  /**
   * Return the regular expression used to construct this object.
   * @specnote Prior to JDK 1.5 this method had a different behavior
   * @since 1.5
   */
  public String toString()
  {
    return regex;
  }
}
