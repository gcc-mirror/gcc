// ExceptionInInitializerError.java

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 1, 1998 
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

public class ExceptionInInitializerError extends LinkageError
{
  public ExceptionInInitializerError ()
  {
    super ();
    exception = null;
  }

  public ExceptionInInitializerError (String msg)
  {
    super (msg);
    exception = null;
  }

  public ExceptionInInitializerError (Throwable e)
  {
    super ();
    exception = e;
  }

  public Throwable getException ()
  {
    return exception;
  }

  public void printStackTrace ()
  {
    exception.printStackTrace ();
  }

  public void printStackTrace (PrintStream ps)
  {
    exception.printStackTrace (ps);
  }

  public void printStackTrace (PrintWriter pw)
  {
    exception.printStackTrace (pw);
  }

  // The exception that caused this error.
  private Throwable exception;
}
