/* ExceptionInInitializerError.java -- thrown when class initialization fails
   with an uncaught exception
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2005  Free Software Foundation, Inc.

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
 * An <code>ExceptionInInitializerError</code> is thrown when an uncaught
 * exception has occurred in a static initializer or the initializer for a
 * static variable. In general, this wraps only RuntimeExceptions, since the
 * compiler does not allow a checked exception to be uncaught in an
 * initializer. This exception only occurs during reflection, when a class
 * is initialized as part of another action.
 *
 * @author Brian Jones
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.1
 * @status updated to 1.4
 */
public class ExceptionInInitializerError extends LinkageError
{
  /**
   * Compatible with JDK 1.1+.
   */
  static final long serialVersionUID = 1521711792217232256L;

  /**
   * The cause of this exception (duplicates the one stored in Throwable).
   *
   * @serial the exception cause
   */
  private final Throwable exception;

  /**
   * Create an error without a message. The cause is initialized as null.
   */
  public ExceptionInInitializerError()
  {
    this((String) null);
  }

  /**
   * Create an error with a message. The cause is initialized as null.
   *
   * @param s the message
   */
  public ExceptionInInitializerError(String s)
  {
    super(s);
    exception = null;
  }

  /**
   * Creates an error an saves a reference to the <code>Throwable</code>
   * object. The message string is null.
   *
   * @param t the exception thrown
   */
  public ExceptionInInitializerError(Throwable t)
  {
    super(null);
    initCause(t);
    exception = t;
  }

  /**
   * Return the exception that caused this error to be created. This is a
   * legacy method; the preferred choice now is {@link Throwable#getCause()}.
   *
   * @return the cause, or null if unknown
   */
  public Throwable getException()
  {
    return exception;
  }

  /**
   * Return the exception that cause this error to be created.
   *
   * @return the cause, or null if unknown
   * @since 1.4
   */
  public Throwable getCause()
  {
    return exception;
  }
}
