/* FormattableFlags.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package java.util;

/** 
 * This class contains a set of flags used
 * by the {@link Formattable#formatTo()} method.
 * They are used to modify the output of the
 * {@link Formattable}.  The interpretation and
 * validation of the flags is left to the
 * particular {@link Formattable}.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5 
 */
public class FormattableFlags
{

  /**
   * Requires the output to be left-justified.  Any spaces
   * required to meet the specified width will be added to
   * the right of the output.  The default output is
   * right-justified, where spaces are added to the left.
   * The output is as for the format specifier
   * '-' ('\u002d').
   */
  public static final int LEFT_JUSTIFY = 1;

  /**
   * Requires the output to be in uppercase.  The output
   * should be the same as the result from calling
   * {@link String#toUpperCase(java.util.Locale)} with
   * the formatting locale. The output is as for the
   * format specifier '^' ('\u005e').
   */
  public static final int UPPERCASE = 2;

  /**
   * Requires the use of an alternate form, as specified
   * in the documentation of {@link Formattable}.
   * The output is as for the format specifier
   * '#' ('\u0023').
   */
  public static final int ALTERNATE = 4;

  // Used internally by Formatter.
  // Changes here must be reflected in the FLAGS string there.

  /**
   * Requires the output to always include a '+' sign.
   * The output is as for the format specifier '+'.
   */
  static final int PLUS = 8;

  /**
   * Requires the output to include a leading space on
   * positive value.  The output is as for the format
   * specifier ' '.
   */
  static final int SPACE = 16;

  /**
   * Requires the output to be zero-padded.  The output
   * is as for the format specifier '0'.
   */
  static final int ZERO = 32;

  /**
   * Requires the output to include locale-specific
   * grouping operators.  The output is as for the
   * format specifier ','.
   */
  static final int COMMA = 64;

  /**
   * Requires the output to include negative numbers
   * enclosed in parentheses.  The output is as for
   * the format specifier '('.
   */
  static final int PAREN = 128;

  // Not instantiable.
  private FormattableFlags()
  {
  }
}
