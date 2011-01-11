/* KeyAgreementException.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.key;

import gnu.java.lang.CPStringBuilder;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.security.KeyManagementException;

/**
 * A generic exception indicating that an unexpected condition has been detected
 * during the setup and/or processing of a key agreement protocol exchange.
 */
public class KeyAgreementException
    extends KeyManagementException
    implements Serializable
{
  /** @serial The possibly <code>null</code> <i>root</i> cause exception. */
  private Throwable cause = null;

  /**
   * Constructs a new instance of <code>KeyAgreementException</code>. The
   * root exception and the detailed message are <code>null</code>.
   */
  public KeyAgreementException()
  {
    super();
  }

  /**
   * Constructs a new instance of <code>KeyAgreementException</code> with a
   * detailed message. The <i>root</i> exception is <code>null</code>.
   *
   * @param detail a possibly <code>null</code> string containing details of
   *          the exception.
   * @see Throwable#getMessage()
   */
  public KeyAgreementException(String detail)
  {
    super(detail);
  }

  /**
   * Constructs a new instance of <code>KeyAgreementException</code> with a
   * detailed message and a <i>root</i> exception.
   *
   * @param detail a possibly <code>null</code> string containing details of
   *          the exception.
   * @param cause a possibly <code>null</code> root exception that caused this
   *          exception.
   * @see Throwable#getMessage()
   * @see #getCause()
   */
  public KeyAgreementException(String detail, Throwable cause)
  {
    super(detail);
    this.cause = cause;
  }

  /**
   * Returns the cause of this throwable or <code>null</code> if the cause is
   * nonexistent or unknown. The <i>cause</i> is the throwable that caused this
   * exception to be thrown.
   *
   * @return the possibly <code>null</code> exception that caused this one.
   */
  public Throwable getCause()
  {
    return cause;
  }

  /**
   * Prints this exception's stack trace to <code>System.err</code>. If this
   * exception has a <i>root</i> exception; the stack trace of the <i>root</i>
   * exception is also printed to <code>System.err</code>.
   */
  public void printStackTrace()
  {
    super.printStackTrace();
    if (cause != null)
      cause.printStackTrace();
  }

  /**
   * Prints this exception's stack trace to a print stream. If this exception
   * has a <i>root</i> exception; the stack trace of the <i>root</i> exception
   * is also printed to the print stream.
   *
   * @param ps the non-null print stream to which to print.
   */
  public void printStackTrace(PrintStream ps)
  {
    super.printStackTrace(ps);
    if (cause != null)
      cause.printStackTrace(ps);
  }

  /**
   * Prints this exception's stack trace to a print writer. If this exception
   * has a <i>root</i> exception; the stack trace of the <i>root</i> exception
   * is also printed to the print writer.
   *
   * @param pw the non-null print writer to use for output.
   */
  public void printStackTrace(PrintWriter pw)
  {
    super.printStackTrace(pw);
    if (cause != null)
      cause.printStackTrace(pw);
  }

  /**
   * Returns the string representation of this exception. The string
   * representation contains this exception's class name, its detailed messsage,
   * and if it has a <i>root</i> exception, the string representation of the
   * root exception. This string representation is meant for debugging and is
   * not meant to be interpreted programmatically.
   *
   * @return the non-null string representation of this exception.
   * @see Throwable#getMessage()
   */
  public String toString()
  {
    CPStringBuilder sb = new CPStringBuilder(this.getClass().getName()).append(": ")
        .append(super.toString());
    if (cause != null)
      sb.append("; caused by: ").append(cause.toString());
    return sb.toString();
  }
}
