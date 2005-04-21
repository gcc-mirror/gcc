/* AssertionError.java -- indication of a failed assertion
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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
 * An assertion error normally occurs as a result of the <code>assert</code>
 * statement added in JDK 1.4, to indicate that an assertion failed. There
 * are enough constructors to ensure that
 * <code>new AssertionError(<em>expression</em>)</code> will work for all
 * expressions, regardless of type, as if the error message were given by
 * the string <code>"" + <em>expression</em></code>. This extends Error,
 * because you usually do not want to inadvertently trap an assertion failure.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.4
 * @status updated to 1.4
 */
public class AssertionError extends Error
{
  /**
   * Compatible with JDK 1.4+.
   */
  private static final long serialVersionUID = -5013299493970297370L;

  /**
   * Construct an AssertionError with no detail message.
   */
  public AssertionError()
  {
  }

  /**
   * Construct an AssertionError with the string conversion of the given
   * object as its error message. If the object is a Throwable, it is also
   * set as the cause of this error.
   *
   * @param msg the source of the error message
   * @see Throwable#getCause()
   */
  public AssertionError(Object msg)
  {
    super("" + msg);
    if (msg instanceof Throwable)
      initCause((Throwable) msg);
  }

  /**
   * Construct an AssertionError with the string conversion of the given
   * boolean as its error message.
   *
   * @param msg the source of the error message
   */
  public AssertionError(boolean msg)
  {
    super(msg ? "true" : "false");
  }

  /**
   * Construct an AssertionError with the string conversion of the given
   * char as its error message.
   *
   * @param msg the source of the error message
   */
  public AssertionError(char msg)
  {
    super(String.valueOf(msg));
  }

  /**
   * Construct an AssertionError with the string conversion of the given
   * int as its error message.
   *
   * @param msg the source of the error message
   */
  public AssertionError(int msg)
  {
    super(Integer.toString(msg, 10));
  }

  /**
   * Construct an AssertionError with the string conversion of the given
   * long as its error message.
   *
   * @param msg the source of the error message
   */
  public AssertionError(long msg)
  {
    super(Long.toString(msg));
  }

  /**
   * Construct an AssertionError with the string conversion of the given
   * float as its error message.
   *
   * @param msg the source of the error message
   */
  public AssertionError(float msg)
  {
    super(Float.toString(msg));
  }

  /**
   * Construct an AssertionError with the string conversion of the given
   * double as its error message.
   *
   * @param msg the source of the error message
   */
  public AssertionError(double msg)
  {
    super(Double.toString(msg));
  }
}
