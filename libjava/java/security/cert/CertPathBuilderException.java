/* CertPathBuilderException.java -- wraps an exception during certificate
   path building
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


package java.security.cert;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.security.GeneralSecurityException;

/**
 * Indicates a problem while using a <code>CertPathBuilder</code>, wrapping
 * the lower exception. This class is not thread-safe.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see CertPathBuilder
 * @since 1.4
 * @status updated to 1.4
*/
public class CertPathBuilderException extends GeneralSecurityException
{
  /**
   * Compatible with JDK 1.4+.
   */
  private static final long serialVersionUID = 5316471420178794402L;

  /**
   * Create an exception without a message. The cause may be initialized.
   */
  public CertPathBuilderException()
  {
  }

  /**
   * Create an exception with a message. The cause may be initialized.
   *
   * @param msg a message to display with exception
   */
  public CertPathBuilderException(String msg)
  {
    super(msg);
  }

  /**
   * Create an exception with a cause. The message will be
   * <code>cause == null ? null : cause.toString()</code>.
   *
   * @param cause the cause
   */
  public CertPathBuilderException(Throwable cause)
  {
    this(cause == null ? null : cause.toString(), cause);
  }

  /**
   * Create an exception with a cause and a message.
   *
   * @param msg the message
   * @param cause the cause
   */
  public CertPathBuilderException(String msg, Throwable cause)
  {
    super(msg);
    initCause(cause);
  }

  /**
   * Get the detail message.
   *
   * @return the detail message
   */
  public String getMessage()
  {
    return super.getMessage();
  }

  /**
   * Get the cause, null if unknown.
   *
   * @return the cause
   */
  public Throwable getCause()
  {
    return super.getCause();
  }

  /**
   * Convert this to a string, including its cause.
   *
   * @return the string conversion
   */
  public String toString()
  {
    return super.toString();
  }

  /**
   * Print the stack trace to <code>System.err</code>.
   */
  public void printStackTrace()
  {
    super.printStackTrace();
  }

  /**
   * Print the stack trace to a stream.
   *
   * @param stream the stream
   */
  public void printStackTrace(PrintStream stream)
  {
    super.printStackTrace(stream);
  }

  /**
   * Print the stack trace to a stream.
   *
   * @param stream the stream
   */
  public void printStackTrace(PrintWriter stream)
  {
    super.printStackTrace(stream);
  }
}
