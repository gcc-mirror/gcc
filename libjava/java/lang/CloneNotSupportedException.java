/* CloneNotSupportedException.java -- thrown when an object cannot be cloned
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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


package java.lang;

/**
 * Thrown to indicate an object should not or could not be cloned. This
 * includes the case when {@link Object#clone()} is called on an object
 * which does not implement the {@link Cloneable} interface. For example:<br>
 * <pre>
 * void m() throws CloneNotSupportedException
 * {
 *   clone();
 * }
 * </pre>
 *
 * <p>Notice that calling <code>clone()</code> on an array will never produce
 * this exception, as the VM will always succeed in copying the array, or
 * cause an OutOfMemoryError first. For example:<br>
 * <pre>
 * void m(int[] array)
 * {
 *   int[] copy = (int[]) array.clone();
 * }
 * </pre>
 *
 * @author Brian Jones
 * @author Warren Levy <warrenl@cygnus.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Cloneable
 * @see Object#clone()
 * @status updated to 1.4
 */
public class CloneNotSupportedException extends Exception
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 5195511250079656443L;

  /**
   * Create an exception without a message.
   */
  public CloneNotSupportedException()
  {
  }

  /**
   * Create an exception with a message.
   *
   * @param s the error message
   */
  public CloneNotSupportedException(String s)
  {
    super(s);
  }
}
