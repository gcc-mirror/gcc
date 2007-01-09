/* RMICException.java --
  Copyright (c) 2003, 2006 Free Software Foundation, Inc.

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
02111-1307 USA. */


package gnu.classpath.tools.rmic;

/**
 * Thrown by the underlying compiler used by RMIC when it fails to compile a
 * file.
 * 
 * @author Dalibor Topic <robilad@kaffe.org>
 */
public class RMICException
    extends Exception
{
  /**
   * Create an exception with a message. The cause remains uninitialized.
   * 
   * @param message the message string
   * @see #initCause(Throwable)
   */
  public RMICException(String message)
  {
    super(message);
  }

  /**
   * Create an exception with a message and a cause.
   * 
   * @param message the message string
   * @param cause the cause of this exception
   */
  public RMICException(String message, Throwable cause)
  {
    super(message, cause);
  }
}
