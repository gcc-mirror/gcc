/* PrivilegedActionException.java -- An exception occurred in a 
   privileged action.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

package java.security;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * This exception is thrown when an exception is thrown during a
 * privileged action being performed with the 
 * <code>AccessController.doPrivileged()</code> method.  It wrappers the
 * actual exception thrown in the privileged code.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class PrivilegedActionException extends Exception
{

  /**
   * This is the actual exception that occurred
   */
  private Exception e;

  /**
   * This method initializes a new instance of <code>PrivilegedActionException</code>
   * that wrappers the specified <code>Exception</code>.
   *
   * @param e The <code>Exception</code> to wrapper
   */
  public PrivilegedActionException(Exception e)
  {
    this.e = e;
  }

  /**
   * This method returns the underlying <code>Exception</code> that caused
   * this exception to be raised.
   *
   * @return The wrappered <code>Exception</code>.
   */
  public Exception getException()
  {
    return (e);
  }

  /**
   * This method prints the stack trace of the wrappered exception.
   */
  public void printStackTrace()
  {
    e.printStackTrace();
  }

  /**
   * This method prints the stack trace of the wrappered exception to the
   * specified <code>PrintStream</code>.
   *
   * @param ps The <code>PrintStream</code> to print the stack trace to.
   */
  public void printStackTrace(PrintStream ps)
  {
    e.printStackTrace(ps);
  }

  /**
   * This method prints the stack trace of the wrappered exception to the
   * specified <code>PrintWriter</code>.
   *
   * @param pw The <code>PrintWriter</code> to print the stack trace to.
   */
  public void printStackTrace(PrintWriter pw)
  {
    e.printStackTrace(pw);
  }
}
