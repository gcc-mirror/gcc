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
