/* UndeclaredThrowableException.java -- wraps an undeclared checked exception
   thrown by a Proxy invocation handler
   Copyright (C) 2001, 2002, 2005  Free Software Foundation, Inc.

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


package java.lang.reflect;

/**
 * This exception class is thrown by a {@link Proxy} instance if
 * the {@link InvocationHandler#invoke(Object, Method, Object[]) invoke}
 * method of that instance's InvocationHandler attempts to throw an
 * exception that not declared by the throws clauses of all of the
 * interface methods that the proxy instance is implementing.
 *
 * <p>When thrown by Proxy, this class will always wrap a checked
 * exception, never {@link Error} or {@link RuntimeException},
 * which are unchecked.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Proxy
 * @see InvocationHandler
 * @since 1.3
 * @status updated to 1.4
 */
public class UndeclaredThrowableException extends RuntimeException
{
  /**
   * Compatible with JDK 1.3+.
   */
  private static final long serialVersionUID = 330127114055056639L;

  /**
   * The immutable exception that this wraps. This field is redundant
   * with {@link Throwable#cause}, but is necessary for serial compatibility.
   *
   * @serial the chained exception
   */
  private final Throwable undeclaredThrowable;

  /**
   * Wraps the given checked exception into a RuntimeException, with no
   * detail message.  {@link Throwable#initCause(Throwable)} will fail
   * on this instance.
   *
   * @param cause the undeclared throwable that caused this exception,
   *        may be null
   */
  public UndeclaredThrowableException(Throwable cause)
  {
    this(cause, null);
  }

  /**
   * Wraps the given checked exception into a RuntimeException, with the
   * specified detail message.  {@link Throwable#initCause(Throwable)} will
   * fail on this instance.
   *
   * @param cause the undeclared throwable that caused this exception,
   *        may be null
   * @param message the message, may be null
   */
  public UndeclaredThrowableException(Throwable cause, String message)
  {
    super(message, cause);
    undeclaredThrowable = cause;
  }

  /**
   * Returns the cause of this exception.  If this exception was created
   * by a {@link Proxy} instance, it will be a non-null checked
   * exception.  This method pre-dates exception chaining, and is now
   * simply a longer way to call <code>getCause()</code>.
   *
   * @return the cause of this exception, may be null
   * @see #getCause()
   */
  public Throwable getUndeclaredThrowable()
  {
    return undeclaredThrowable;
  }

  /**
   * Returns the cause of this exception.  If this exception was created
   * by a {@link Proxy} instance, it will be a non-null checked
   * exception.
   *
   * @return the cause of this exception, may be null
   * @since 1.4
   */
  public Throwable getCause()
  {
    return undeclaredThrowable;
  }
}
