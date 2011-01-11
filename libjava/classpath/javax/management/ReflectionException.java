/* ReflectionException.java -- A reflection-based management exception.
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
 * Represents one of the reflection exceptions thrown by a
 * management bean.  When a management bean tries to perform
 * a lookup using the reflection API and encounters an exception,
 * it gets wrapped inside an {@link ReflectionException}.
 * Calling {@link getTargetException()} will return the wrapped
 * exception.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ReflectionException
  extends JMException
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 9170809325636915553L;

  /* Sun re-implemented causality -- don't know why, but
     serialization demands we do too... */

  /**
   * The target exception.
   *
   * @serial the target exception.
   */
  private Exception exception;

  /**
   * Constructs a new <code>ReflectionException</code> wrapping
   * the specified exception.
   *
   * @param e the exception to be wrapped.
   */
  public ReflectionException(Exception e)
  {
    super();
    exception = e;
  }

  /**
   * Constructs a new <code>ReflectionException</code> wrapping
   * the specified exception and using the supplied message.
   *
   * @param e the exception to be wrapped.
   * @param message the error message to give to the user.
   */
  public ReflectionException(Exception e, String message)
  {
    super(message);
    exception = e;
  }

  /**
   * Returns the true cause of this exception, the wrapped
   * exception.
   *
   * @return the wrapped exception.
   */
  public Throwable getCause()
  {
    return exception;
  }

  /**
   * Returns the true cause of this exception, the wrapped
   * exception.
   *
   * @return the wrapped exception.
   */
  public Exception getTargetException()
  {
    return exception;
  }

}
