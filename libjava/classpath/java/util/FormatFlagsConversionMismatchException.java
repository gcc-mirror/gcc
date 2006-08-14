/* FormatFlagsConversionMismatchException.java
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
 * Thrown when the flags supplied to the {@link Formatter#format()}
 * method of a {@link Formatter} contains a flag that does not match
 * the conversion character specified for it.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5 
 */
public class FormatFlagsConversionMismatchException
  extends IllegalFormatException
{
  private static final long serialVersionUID = 19120414L;

  /**
   * The mismatching flag.
   *
   * @serial the mismatching flag.
   */
  // Note: name fixed by serialization.
  private String f;

  /**
   * The conversion character which doesn't match the
   * appropriate flag.
   *
   * @serial the conversion character which doesn't match its flag.
   */
  // Note: name fixed by serialization.
  private char c;

  /**
   * Constructs a new <code>FormatFlagsConversionMismatchException</code>
   * which specifies that the flag, <code>f</code>, does
   * not match its appropriate conversion character, <code>c</code>.
   *
   * @param f the mismatching flag.
   * @param c the conversion character which doesn't match its flag.
   * @throws NullPointerException if <code>f</code> is null.
   */
  public FormatFlagsConversionMismatchException(String f, char c)
  {
    super("Invalid flag " + f + " for conversion " + c);
    if (f == null)
      throw new
	NullPointerException("Null flag value passed to constructor.");
    this.f = f;
    this.c = c;
  }

  /**
   * Returns the conversion character which doesn't
   * match the flag.
   *
   * @return the conversion character.
   */
  public char getConversion()
  {
    return c;
  }

  /**
   * Returns the mismatching flag.
   *
   * @return the mismatching flag.
   */
  public String getFlags()
  {
    return f;
  }
}
