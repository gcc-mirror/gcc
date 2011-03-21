/* RuntimeErrorException.java -- A user-defined management error.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

/**
 * Represents an arbitrary error thrown by a management
 * bean.  When a management bean executes code that causes
 * an error to be thrown, the resulting error is
 * wrapped inside an {@link RuntimeErrorException}.  Calling
 * {@link getTargetError()} will return the wrapped
 * exception.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class RuntimeErrorException
  extends JMRuntimeException
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 704338937753949796L;

  /**
   * The target error.
   *
   * @serial the target error.
   */
  private Error error;

  /**
   * Constructs a new <code>RuntimeErrorException</code> wrapping
   * the specified error.
   *
   * @param e the error to be wrapped.
   */
  public RuntimeErrorException(Error e)
  {
    super();
    error = e;
  }

  /**
   * Constructs a new <code>RuntimeErrorException</code> wrapping
   * the specified error and using the supplied message.
   *
   * @param e the error to be wrapped.
   * @param message the error message to give to the user.
   */
  public RuntimeErrorException(Error e, String message)
  {
    super(message);
    error = e;
  }

  /**
   * Returns the true cause of this error, the wrapped
   * error.
   *
   * @return the wrapped error.
   */
  public Throwable getCause()
  {
    return error;
  }

  /**
   * Returns the true cause of this error, the wrapped
   * error.
   *
   * @return the wrapped error.
   */
  public Error getTargetError()
  {
    return error;
  }

}
