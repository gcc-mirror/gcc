// FinalizerThread.java -- Thread in which finalizers are run.

/* Copyright (C) 2001, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date October 3, 2001
 */
public final class FinalizerThread extends Thread
{
  private static boolean finalizer_ready;

  public FinalizerThread ()
  {
    super ("LibgcjInternalFinalizerThread");
    setDaemon (true);
    finalizer_ready = false;
    init();
  }

  private native void init();
  static native void finalizerReady();
  public native void run();
}
