/* SaslException.java
   Copyright (C) 2003, Free Software Foundation, Inc.

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


package javax.security.sasl;

import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;

/**
 * This class represents an error that has occurred when using SASL.
 */
public class SaslException extends IOException implements Serializable
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /**
   * @serial The possibly null root cause exception.
   */
  private Throwable _exception = null;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Constructs a new instance of <code>SaslException</code>. The root
   * exception and the detailed message are null.
   */
  public SaslException()
  {
    super();
  }

  /**
   * Constructs a new instance of <code>SaslException</code> with a detailed
   * message. The <code>root</code> exception is <code>null</code>.
   *
   * @param detail a possibly null string containing details of the exception.
   * @see Throwable#getMessage()
   */
  public SaslException(String detail)
  {
    super(detail);
  }

  /**
   * Constructs a new instance of <code>SaslException</code> with a detailed
   * message and a root exception. For example, a <code>SaslException</code>
   * might result from a problem with the callback handler, which might throw a
   * {@link javax.security.auth.callback.UnsupportedCallbackException} if it
   * does not support the requested callback, or throw an {@link IOException}
   * if it had problems obtaining data for the callback. The
   * <code>SaslException</code>'s root exception would be then be the exception
   * thrown by the callback handler.
   *
   * @param detail a possibly <code>null</code> string containing details of
   * the exception.
   * @param ex a possibly <code>null</code> root exception that caused this
   * exception.
   * @see Throwable#getMessage()
   * @see #getCause()
   */
  public SaslException(String detail, Throwable ex)
  {
    super(detail);
    _exception = ex;
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------

  /**
   * Returns the cause of this throwable or <code>null</code> if the cause is
   * nonexistent or unknown. The cause is the throwable that caused this
   * exception to be thrown.
   *
   * @return the possibly <code>null</code> exception that caused this exception.
   */
  public Throwable getCause()
  {
    return _exception;
  }

  /**
   * Prints this exception's stack trace to <code>System.err</code>. If this
   * exception has a root exception; the stack trace of the root exception is
   * also printed to <code>System.err</code>.
   */
  public void printStackTrace()
  {
    super.printStackTrace();
    if (_exception != null)
      _exception.printStackTrace();
  }

  /**
   * Prints this exception's stack trace to a print stream. If this exception
   * has a root exception; the stack trace of the root exception is also
   * printed to the print stream.
   *
   * @param ps the non-null print stream to which to print.
   */
  public void printStackTrace(PrintStream ps)
  {
    super.printStackTrace(ps);
    if (_exception != null)
      _exception.printStackTrace(ps);
  }

  /**
   * Prints this exception's stack trace to a print writer. If this exception
   * has a root exception; the stack trace of the root exception is also
   * printed to the print writer.
   *
   * @param pw the non-null print writer to use for output.
   */
  public void printStackTrace(PrintWriter pw)
  {
    super.printStackTrace(pw);
    if (_exception != null)
      _exception.printStackTrace(pw);
  }

  /**
   * Returns the string representation of this exception. The string
   * representation contains this exception's class name, its detailed
   * messsage, and if it has a root exception, the string representation of the
   * root exception. This string representation is meant for debugging and not
   * meant to be interpreted programmatically.
   *
   * @return the non-null string representation of this exception.
   * @see Throwable#getMessage()
   */
  public String toString()
  {
    StringBuffer sb = new StringBuffer(this.getClass().getName())
      .append(": ").append(super.toString());
    if (_exception != null)
      sb.append("; caused by: ").append(_exception.toString());
    return sb.toString();
  }
}
