/* PrivilegedActionException.java -- wrap an exception in a privileged action
   Copyright (C) 1998, 2002, 2005  Free Software Foundation, Inc.

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

package java.security;

/**
 * This exception is thrown when an exception is thrown during a
 * privileged action being performed with the
 * <code>AccessController.doPrivileged()</code> method.  It wraps the
 * actual exception thrown in the privileged code.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see PrivilegedExceptionAction
 * @see AccessController#doPrivileged(PrivilegedExceptionAction)
 * @see AccessController#doPrivileged(PrivilegedExceptionAction, AccessControlContext)
 * @status updated to 1.4
 */
public class PrivilegedActionException extends Exception
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 4724086851538908602L;

  /**
   * This is the actual exception that occurred.
   *
   * @serial the wrapped exception
   */
  private Exception exception;

  /**
   * Create a new instance that wraps the specified <code>Exception</code>.
   *
   * @param e the <code>Exception</code> to wrap
   */
  public PrivilegedActionException(Exception e)
  {
    super(e);
    exception = e;
  }

  /**
   * Get the underlying <code>Exception</code> that caused this one. This
   * is a legacy method, the preferred way is {@link #getCause()}.
   *
   * @return the cause
   */
  public Exception getException()
  {
    return exception;
  }

  /**
   * Gets the cause of this exception.
   *
   * @return the cause
   * @since 1.4
   */
  public Throwable getCause()
  {
    return exception;
  }

  /**
   * Convert this to a String.
   *
   * @return the string representation
   */
  public String toString()
  {
    return super.toString();
  }
}
