/* WriteAbortedException.java -- wraps an exception thrown while writing
   Copyright (C) 1998, 2000, 2002 Free Software Foundation, Inc.

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


package java.io;

/**
  * This exception is thrown when another ObjectStreamException occurs during
  * a serialization read or write. The stream is reset, and deserialized
  * objects are discarded.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Eric Blake <ebb9@email.byu.edu>
  * @since 1.1
  * @status updated to 1.4
  */
public class WriteAbortedException extends ObjectStreamException
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -3326426625597282442L;

  /**
   * The cause of this exception. This pre-dates the exception chaining
   * of Throwable; and although you can change this field, you are wiser
   * to leave it alone.
   *
   * @serial the exception cause
   */
  public Exception detail;

  /**
   * Create a new WriteAbortedException with a specified message and
   * cause.
   *
   * @param msg the message
   * @param detail the cause
   */
  public WriteAbortedException(String msg, Exception detail)
  {
    super(msg);
    initCause(detail);
    this.detail = detail;
  }

  /**
   * This method returns a message indicating what went wrong, in this
   * format:
   * <code>super.getMessage() + (detail == null ? "" : "; " + detail)<code>.
   *
   * @return the chained message
   */
  public String getMessage()
  {
    if (detail == this || detail == null)
      return super.getMessage();
    return super.getMessage() + "; " + detail;
  }

  /**
   * Returns the cause of this exception. Note that this may not be the
   * original cause, thanks to the <code>detail</code> field being public
   * and non-final (yuck). However, to avoid violating the contract of
   * Throwable.getCause(), this returns null if <code>detail == this</code>,
   * as no exception can be its own cause.
   *
   * @return the cause
   * @since 1.4
   */
  public Throwable getCause()
  {
    return detail == this ? null : detail;
  }
} // class WriteAbortedException
