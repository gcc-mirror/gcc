// InvocationTargetException.java - Wrapper exception for reflection.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang.reflect;
import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date December 12, 1998
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Believed complete and correct.
 */

public class InvocationTargetException extends Exception
{
  public Throwable getTargetException ()
    {
      return target;
    }

  protected InvocationTargetException ()
    {
      super ();
      target = null;
    }

  public InvocationTargetException (Throwable exception)
    {
      super ();
      target = exception;
    }

  public InvocationTargetException (Throwable exception, String msg)
    {
      super (msg);
      target = exception;
    }

  // This is from JDK 1.2.
  public void printStackTrace ()
    {
      if (target != null)
	target.printStackTrace();
    }

  // This is from JDK 1.2.
  public void printStackTrace (PrintStream s)
    {
      if (target != null)
	target.printStackTrace(s);
    }

  // This is from JDK 1.2.
  public void printStackTrace (PrintWriter wr)
    {
      if (target != null)
	target.printStackTrace(wr);
    }

  // The wrapped exception.  The name is specified by the
  // serialization spec.
  private Throwable target;
}
