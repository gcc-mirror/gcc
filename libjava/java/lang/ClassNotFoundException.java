/* ClassNotFoundException.java -- thrown when class definition cannot be found
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

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
 * Thrown when a class is requested by reflection, but the class definition
 * cannot be found. This exception is often chained from another Throwable.
 *
 * @author Brian Jones
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Class#forName(String)
 * @see ClassLoader#findSystemClass(String)
 * @see ClassLoader#loadClass(String, boolean)
 * @status updated to 1.4
 */
public class ClassNotFoundException extends Exception
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 9176873029745254542L;

  /**
   * The cause of this exception (duplicates the one stored in Throwable).
   *
   * @serial the exception cause
   * @since 1.2
   */
  private final Throwable ex;

  /**
   * Create an exception without a message. Note that this initializes the
   * cause to null.
   */
  public ClassNotFoundException()
  {
    this(null, null);
  }

  /**
   * Create an exception with a message. Note that this initializes the
   * cause to null.
   *
   * @param s the message
   */
  public ClassNotFoundException(String s)
  {
    this(s, null);
  }

  /**
   * Create an exception with a message and chain it to the exception
   * which occurred while loading the class.
   *
   * @param s the message
   * @param ex the chained exception
   * @since 1.2
   */
  public ClassNotFoundException(String s, Throwable ex)
  {
    super(s, ex);
    this.ex = ex;
  }

  /**
   * Returns the exception which occurred while loading the class,
   * otherwise returns null. This is a legacy method; the preferred choice
   * now is {@link Throwable#getCause()}.
   *
   * @return the cause of this exception
   * @since 1.2
   */
  public Throwable getException()
  {
    return ex;
  }

  /**
   * Returns the exception which occurred while loading the class,
   * otherwise returns null.
   *
   * @return the cause of this exception
   * @since 1.4
   */
  public Throwable getCause()
  {
    return ex;
  }
}
