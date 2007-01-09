/* IllegalFormatConversionException.java
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
 * Thrown when the type of an argument supplied to the
 * {@link Formatter#format()} method of a {@link Formatter}
 * does not match the conversion character specified for it.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5 
 */
public class IllegalFormatConversionException 
  extends IllegalFormatException
{
  private static final long serialVersionUID = 17000126L;

  /**
   * The conversion character which doesn't match
   * the type of the argument.
   *
   * @serial the conversion character.
   */
  // Note: name fixed by serialization.
  char c;

  /**
   * The type of the mismatching argument.
   *
   * @serial the mismatching argument type.
   */
  // Note: name fixed by serialization.
  Class<?> arg;

  /**
   * Constructs a new <code>IllegalFormatConversionException</code>
   * which specifies that the argument of type <code>arg</code> does
   * not match the conversion character, <code>c</code>.
   *
   * @param c the conversion character.
   * @param arg the type which doesn't match the conversion character.
   * @throws NullPointerException if <code>arg</code> is null.
   */
  public IllegalFormatConversionException(char c, Class<?> arg)
  {
    super("The type, " + arg + ", is invalid for the conversion character, " +
	  c + ".");
    if (arg == null)
      throw new NullPointerException("The supplied type was null.");
    this.c = c;
    this.arg = arg;
  }

  /**
   * Returns the conversion character.
   *
   * @return the conversion character.
   */
  public char getConversion()
  {
    return c;
  }

  /**
   * Returns the type of the mismatched argument.
   *
   * @return the type of the mismatched argument.
   */
  public Class<?> getArgumentClass()
  {
    return arg;
  }
}
