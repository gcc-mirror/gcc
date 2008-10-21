/* VMCPStringBuilder.java -- Growable strings without locking or copying
   Copyright (C) 2008 Free Software Foundation, Inc.

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

package gnu.java.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
 * This class provides VM support for CPStringBuilder
 * by allowing the package-private constructor
 * of java.lang.String to be invoked.  The default
 * implementation uses reflection.  VMs may replace
 * this class with a more efficient version.
 */
final class VMCPStringBuilder
{

  /**
   * The package-private constructor for String objects without copying.
   */
  private static final Constructor cons;

  static
  {
    try
      {
	cons = String.class.getDeclaredConstructor(new Class[] { char[].class,
								 Integer.TYPE,
								 Integer.TYPE,
								 Boolean.TYPE });
	cons.setAccessible(true);
      }
    catch (NoSuchMethodException e)
      {
	throw (Error) 
	  new InternalError("Could not get no-copy String constructor").initCause(e);
      }
  }

  /**
   * Convert this <code>StringBuilder</code> to a <code>String</code>. The
   * String is composed of the characters currently in this StringBuilder. Note
   * that the result is not a copy, so the builder will allocate a new array
   * if a further write operation is attempted.
   *
   * @param value the buffered characters.
   * @param startIndex the index at which to start taking characters from the buffer.
   * @param count the number of characters used in the buffer.
   * @return the characters in this StringBuilder
   */
  public static String toString(char[] value, int startIndex, int count)
  {
    try
      {
	return (String)
	  cons.newInstance(new Object[] { value, Integer.valueOf(startIndex),
					  Integer.valueOf(count),
					  Boolean.valueOf(true) });
      }
    catch (InstantiationException e)
      {
	throw (Error) 
	  new InternalError("Could not instantiate no-copy String constructor").initCause(e);
      }
    catch (IllegalAccessException e)
      {
	throw (Error) 
	  new InternalError("Could not access no-copy String constructor").initCause(e);
      }
    catch (InvocationTargetException e)
      {
	throw (Error) 
	  new InternalError("Error calling no-copy String constructor").initCause(e);
      }
  }

}
