// FirstThread.java - Implementation of very first thread.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date August 24, 1998 
 */

// This is entirely internal to our implementation.

final class FirstThread extends Thread
{
  public native void run ();

  public FirstThread (ThreadGroup g, Class k, Object o)
  {
    super (g, null, "main");
    klass = k;
    args = o;
  }

  private static void die (String s)
  {
    System.err.println(s);
    System.exit(1);
  }

  // Private data.
  private Class klass;
  private Object args;
}
