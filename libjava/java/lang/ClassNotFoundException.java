/* ClassNotFoundException.java -- exception thrown when attempting to load
   a class when no definition for the class can be found.
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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.lang;

import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * Exceptions may be thrown by one part of a Java program and caught
 * by another in order to deal with exceptional conditions.  This 
 * exception can by thrown by specific methods of <code>ClassLoader</code>
 * and <code>Class</code> when attempting to load a class when no definition
 * for the specified class can be found.
 *
 * @since JDK 1.0
 * 
 * @author Brian Jones
 */
public class ClassNotFoundException extends Exception
{
  static final long serialVersionUID = 9176873029745254542L;

  private Throwable ex = null;
  
  /**
   * Create an exception without a message.
   */
  public ClassNotFoundException()
    {
      super();
    }

  /**
   * Create an exception with a message.
   */
  public ClassNotFoundException(String s)
    {
      super(s);
    }

  /**
   * Create an exception with a message and include the exception 
   * which occurred while loading the class.
   *
   * @param ex the exception which occurred while loading the class
   *
   * @since JDK 1.2
   */
  public ClassNotFoundException(String s, Throwable ex)
    {
      super(s);
      this.ex = ex;
    }

  /**
   * Returns the exception which occurred while loading the class, 
   * otherwise returns null.
   * 
   * @since JDK 1.2
   */
  public Throwable getException()
    {
      return ex;
    }

  /**
   * Print a stack trace of the exception that occurred.
   */
  public void printStackTrace()
    {
      if (ex == null)
        {
          super.printStackTrace();
        }
      else
        {
          ex.printStackTrace();
        }
    }

  /**
   * Print a stack trace of the exception that occurred to 
   * the specified <code>PrintStream</code>.
   */
  public void printStackTrace(PrintStream ps)
    {
      if (ex == null)
        {
          super.printStackTrace(ps);
        }
      else
        {
          ex.printStackTrace(ps);
        }
    }

  /**
   * Print a stack trace of the exception that occurred to 
   * the specified <code>PrintWriter</code>.
   */
  public void printStackTrace(PrintWriter pw)
    {
      if (ex == null)
        {
          super.printStackTrace(pw);
        }
      else
        {
          ex.printStackTrace(pw);
        }
    }

  /**
   * Serialize the object in a manner binary compatible with the JDK 1.2
   */
  private void writeObject(java.io.ObjectOutputStream s) 
    throws IOException
    {
      ObjectOutputStream.PutField oFields;
      oFields = s.putFields();
      oFields.put("ex", this.ex);
      s.writeFields(); 
    }

  /**
   * Deserialize the object in a manner binary compatible with the JDK 1.2
   */    
  private void readObject(java.io.ObjectInputStream s)
    throws IOException, ClassNotFoundException
    {
      ObjectInputStream.GetField oFields;
      oFields = s.readFields();
      ex = (Throwable)oFields.get("ex", (Throwable)null);
    }
}
