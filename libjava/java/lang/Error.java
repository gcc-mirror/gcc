/* Error.java -- Indication of fatal abnormal conditions
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
 * Applications should not try to catch errors since they indicate
 * abnormal conditions.  An abnormal condition is something which should not
 * occur, or which should not be recovered from.  This latter category
 * includes <code>ThreadDeath</code> and <code>AssertionError</code>.
 *
 * <p>A method is not required to declare any subclass of <code>Error</code> in
 * its <code>throws</code> clause which might be thrown but not caught while
 * executing the method.
 *
 * @author Brian Jones
 * @author Tom Tromey <tromey@cygnus.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.0
 * @status updated to 1.4
 */
public class Error extends Throwable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 4980196508277280342L;

  /**
   * Create an error without a message. The cause remains uninitialized.
   *
   * @see #initCause(Throwable)
   */
  public Error()
  {
  }

  /**
   * Create an error with a message. The cause remains uninitialized.
   *
   * @param s the message string
   * @see #initCause(Throwable)
   */
  public Error(String s)
  {
    super(s);
  }

  /**
   * Create an error with a message and a cause.
   *
   * @param s the message string
   * @param cause the cause of this error
   * @since 1.4
   */
  public Error(String s, Throwable cause)
  {
    super(s, cause);
  }

  /**
   * Create an error with a given cause, and a message of
   * <code>cause == null ? null : cause.toString()</code>.
   *
   * @param cause the cause of this error
   * @since 1.4
   */
  public Error(Throwable cause)
  {
    super(cause);
  }
}
