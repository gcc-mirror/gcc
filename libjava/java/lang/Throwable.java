// Throwable.java - Superclass for all exceptions.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.OutputStreamWriter;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 30, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status: Sufficient for compiled code, but methods applicable to
 * bytecode not implemented.  JDK 1.1.
 */

public class Throwable implements Serializable
{
  public native Throwable fillInStackTrace ();

  public String getLocalizedMessage ()
  {
    return getMessage ();
  }

  public String getMessage ()
  {
    return detailMessage;
  }

  public void printStackTrace ()
  {
    printStackTrace (System.err);
  }

  public void printStackTrace (PrintStream ps)
  {
    printStackTrace (new PrintWriter(new OutputStreamWriter(ps)));
  }
  
  public native void printStackTrace (PrintWriter wr);
  
  public Throwable ()
  {
    detailMessage = null;
    fillInStackTrace ();
  }

  public Throwable (String message)
  {
    detailMessage = message;
    fillInStackTrace ();
  }

  public String toString ()
  {
    return ((detailMessage == null)
	    ? getClass().getName()
	    : getClass().getName() + ": " + getMessage ());
  }

  // Name of this field comes from serialization spec.
  private String detailMessage;

  private byte stackTrace[];
}
