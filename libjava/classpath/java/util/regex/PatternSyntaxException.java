/* PatternSyntaxException - Indicates illegal pattern for regular expression.
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

import gnu.java.lang.CPStringBuilder;

/**
 * Indicates illegal pattern for regular expression.
 * Includes state to inspect the pattern and what and where the expression
 * was not valid regular expression.
 * @since 1.4
 */
public class PatternSyntaxException extends IllegalArgumentException
{
  private static final long serialVersionUID = -3864639126226059218L;

  /**
   * Human readable escription of the syntax error.
   */
  private final String desc;

  /**
   * The original pattern that contained the syntax error.
   */
  private final String pattern;
  
  /**
   * Index of the first character in the String that was probably invalid,
   * or -1 when unknown.
   */
  private final int index;

  /**
   * Creates a new PatternSyntaxException.
   *
   * @param description Human readable escription of the syntax error.
   * @param pattern The original pattern that contained the syntax error.
   * @param index Index of the first character in the String that was
   *        probably invalid, or -1 when unknown.
   */
  public PatternSyntaxException(String description,
		                String pattern,
				int index)
  {
    super(description);
    this.desc = description;
    this.pattern = pattern;
    this.index = index;
  }

  /**
   * Returns a human readable escription of the syntax error.
   */
  public String getDescription()
  {
    return desc;
  }

  /**
   * Returns the original pattern that contained the syntax error.
   */
  public String getPattern()
  {
    return pattern;
  }

  /**
   * Returns the index of the first character in the String that was probably
   * invalid, or -1 when unknown.
   */
  public int getIndex()
  {
    return index;
  }

  /**
   * Returns a string containing a line with the description, a line with
   * the original pattern and a line indicating with a ^ which character is
   * probably the first invalid character in the pattern if the index is not
   * negative.
   */
  public String getMessage()
  {
    String lineSep = System.getProperty("line.separator");
    CPStringBuilder sb = new CPStringBuilder(desc);
    sb.append(lineSep);
    sb.append('\t');
    sb.append(pattern);
    if (index != -1)
      {
	sb.append(lineSep);
	sb.append('\t');
	for (int i=0; i<index; i++)
	  sb.append(' ');
	sb.append('^');
      }
    return sb.toString();
  }

}
