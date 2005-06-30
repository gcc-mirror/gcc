/* PrinterIOException.java -- The print job encountered an I/O error
   Copyright (C) 1999, 2002, 2005  Free Software Foundation, Inc.

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


package java.awt.print;

import java.io.IOException;

/**
 * This exception is thrown when the print job encounters an I/O problem
 * of some kind.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @status updated to 1.4
 */
public class PrinterIOException extends PrinterException
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 5850870712125932846L;

  /**
   * The exception that caused this (duplicates Throwable).
   *
   * @serial the I/O exception that terminated the job
  */
  private final IOException mException;

  /**
   * Initializes a new instance with the given cause.
   *
   * @param mException the cause
   */
  public PrinterIOException(IOException mException)
  {
    super(mException == null ? null : mException.toString());
    initCause(mException);
    this.mException = mException;  
  }

  /**
   * Gets the underlying <code>IOException</code> that caused this exception.
   * This legacy method has been replaced by {@link #getCause()}.
   *
   * @return the cause
  */
  public IOException getIOException()
  {
    return mException;
  }

  /**
   * Gets the cause.
   *
   * @return the cause
   */
  public Throwable getCause()
  {
    return mException;
  }
} // class PrinterIOException

