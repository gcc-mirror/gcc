/* InvocationTargetException.java -- Wrapper exception for reflection
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


package java.lang.reflect;

/**
 * InvocationTargetException is sort of a way to "wrap" whatever exception
 * comes up when a method or constructor is called via Reflection. As of
 * JDK 1.4, it was retrofitted to match the exception chaining of all other
 * exceptions, but <code>getTargetException()</code> still works.
 *
 * @author John Keiser
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Method#invoke(Object,Object[])
 * @see Constructor#newInstance(Object[])
 * @since 1.1
 * @status updated to 1.4
 */
public class InvocationTargetException extends Exception
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 4085088731926701167L;

  /**
   * The chained exception. This field is only around for serial compatibility.
   *
   * @serial the chained exception
   */
  private final Throwable target;

  /**
   * Construct an exception with null as the cause. The cause is initialized
   * to null.
   */
  protected InvocationTargetException()
  {
    this(null, null);
  }

  /**
   * Create an <code>InvocationTargetException</code> using another
   * exception.
   *
   * @param targetException the exception to wrap
   */
  public InvocationTargetException(Throwable targetException)
  {
    this(targetException, null);
  }

  /**
   * Create an <code>InvocationTargetException</code> using another
   * exception and an error message.
   *
   * @param targetException the exception to wrap
   * @param err an extra reason for the exception-throwing
   */
  public InvocationTargetException(Throwable targetException, String err)
  {
    super(err, targetException);
    target = targetException;
  }

  /**
   * Get the wrapped (targeted) exception.
   *
   * @return the targeted exception
   * @see #getCause()
   */
  public Throwable getTargetException()
  {
    return target;
  }

  /**
   * Returns the cause of this exception (which may be null).
   *
   * @return the cause
   * @since 1.4
   */
  public Throwable getCause()
  {
    return target;
  }
}
