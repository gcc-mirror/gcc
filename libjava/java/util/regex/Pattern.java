/* Pattern.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

// Stub class until java.util.regex is implemented.
package java.util.regex;

import java.io.Serializable;

/**
 * @author Michael Koch
 * @since 1.4
 */
public class Pattern implements Serializable
{
  private static final long serialVersionUID = 5073258162644648461L;
  
  public static final int CANON_EQ = 128;
  public static final int CASE_INSENSITIVE = 2;
  public static final int COMMENTS = 4;
  public static final int DOTALL = 32;
  public static final int MULTILINE = 8;
  public static final int UNICODE_CASE = 64;
  public static final int UNIX_LINES = 1;
  
  private String regex;
  private int flags;

  private Pattern (String regex)
    throws PatternSyntaxException
  {
    this (regex, 0);
  }

  private Pattern (String regex, int flags)
    throws PatternSyntaxException
  {
    this.regex = regex;
    this.flags = flags;

    throw new Error ("Not implemented");
  }
 
  /**
   * @param regex The regular expression
   *
   * @exception PatternSyntaxException If the expression's syntax is invalid
   */
  public static Pattern compile (String regex)
    throws PatternSyntaxException
  {
    throw new Error ("Not implemented");
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
    throw new Error ("Not implemented");
  }
  
  /**
   * @param input The character sequence to be matched
   */
  public Matcher matcher (CharSequence input)
  {
    throw new Error ("Not implemented");
  }
  
  /**
   * @param input The character sequence to be matched
   */
  public String[] split (CharSequence input)
  {
    throw new Error ("Not implemented");
  }
  
  /**
   * @param input The character sequence to be matched
   * @param limit The result threshold
   */
  public String[] split (CharSequence input, int limit)
  {
    throw new Error ("Not implemented");
  }
  
  public String pattern ()
  {
    throw new Error ("Not implemented");
  }
}
