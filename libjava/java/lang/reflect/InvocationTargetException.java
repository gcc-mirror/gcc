/* InvocationTargetException.java - Wrapper exception for reflection
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.lang.reflect;

import java.io.PrintStream;
import java.io.PrintWriter;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Believed complete and correct.
 */

/**
 * InvocationTargetException is sort of a way to "wrap" whatever exception 
 * comes up when a method or constructor is called via Reflection.
 *
 * @author John Keiser
 * @version 1.1.0, 31 May 1998
 * @author Tom Tromey <tromey@cygnus.com>
 * @date December 12, 1998
 *
 * @see Method#invoke(Object,Object[])
 * @see Constructor#newInstance(Object[])
 */

public class InvocationTargetException extends Exception 
{
  static final long serialVersionUID = 4085088731926701167L;

  private Throwable target = null;
  
  protected InvocationTargetException() 
    {
      super();
    }
  
  /**
   * Create an <code>InvocationTargetException</code> using another 
   * exception.
   * @param targetException the exception to wrap
   */
  public InvocationTargetException(Throwable targetException) 
    {
      super(targetException.toString());
      target = targetException;
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
      super(err);
      target = targetException;
    }
  
  /**
   * Get the wrapped (targeted) exception.
   * 
   * @return the targeted exception.
   */
  public Throwable getTargetException() 
    {
      return target;
    }

  public void printStackTrace()
    {
      if (target == null)
	super.printStackTrace();
      else
      {
	System.err.print(this.getClass() + ": ");
	target.printStackTrace();
      }
    }

  public void printStackTrace(PrintStream ps)
    {
      if (target == null)
	super.printStackTrace(ps);
      else
      {
	ps.print(this.getClass() + ": ");
	target.printStackTrace(ps);
      }
    }

  public void printStackTrace(PrintWriter pw)
    {
      if (target == null)
	super.printStackTrace(pw);
      else
      {
	pw.print(this.getClass() + ": ");
	target.printStackTrace(pw);
      }
    }
}
