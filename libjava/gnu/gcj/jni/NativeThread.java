// NativeThread.java - Wrapper for attached user threads.

/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.jni;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date February 9, 2000
 */

public class NativeThread extends Thread
{
  public NativeThread (ThreadGroup g, String name)
  {
    super (g, null, name);
    alive_flag = true;
  }

  // Call this to mark the thread as finished.
  public native void finish ();
}
