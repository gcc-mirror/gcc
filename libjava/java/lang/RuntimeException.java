/* RuntimeException.java -- root of all unchecked exceptions
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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
 * All exceptions which are subclasses of <code>RuntimeException</code>
 * can be thrown at any time during the execution of a Java virtual machine.
 * Methods which throw these exceptions are not required to declare them
 * in their throws clause.
 *
 * @author Brian Jones
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @status updated to 1.4
 */
public class RuntimeException extends Exception
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -7034897190745766939L;

  /**
   * Create an exception without a message. The cause remains uninitialized.
   *
   * @see #initCause(Throwable)
   */
  public RuntimeException()
  {
  }

  /**
   * Create an exception with a message. The cause remains uninitialized.
   *
   * @param s the message string
   * @see #initCause(Throwable)
   */
  public RuntimeException(String s)
  {
    super(s);
  }

  /**
   * Create an exception with a message and a cause.
   *
   * @param s the message string
   * @param cause the cause of this exception
   * @since 1.4
   */
  public RuntimeException(String s, Throwable cause)
  {
    super(s, cause);
  }

  /**
   * Create an exception with the given cause, and a message of
   * <code>cause == null ? null : cause.toString()</code>.
   *
   * @param cause the cause of this exception
   * @since 1.4
   */
  public RuntimeException(Throwable cause)
  {
    super(cause);
  }
}
