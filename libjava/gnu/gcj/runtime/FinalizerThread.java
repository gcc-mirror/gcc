// FinalizerThread.java -- Thread in which finalizers are run.

/* Copyright (C) 2001  Free Software Foundation

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
  // Finalizers must be run in a thread with no Java-visible locks
  // held.  This qualifies because we don't make the lock visible.
  private static final Object lock = new Object ();

  // This is true if the finalizer thread started successfully.  It
  // might be false if, for instance, there are no threads on the
  // current platform.  In this situation we run finalizers in the
  // caller's thread.
  private static boolean thread_started = false;

  public FinalizerThread ()
  {
    super ("LibgcjInternalFinalizerThread");
    setDaemon (true);
  }

  // This is called by the runtime when a finalizer is ready to be
  // run.  It simply wakes up the finalizer thread.
  public static void finalizerReady ()
  {
    synchronized (lock)
      {
	if (! thread_started)
	  runFinalizers ();
	else
	  lock.notify ();
      }
  }

  // Actually run the finalizers.
  private static native void runFinalizers ();

  public void run ()
  {
    // Wait on a lock.  Whenever we wake up, try to invoke the
    // finalizers.
    synchronized (lock)
      {
	thread_started = true;
	while (true)
	  {
	    try
	      {
		lock.wait ();
	      }
	    catch (InterruptedException _)
	      {
		// Just ignore it.  It doesn't hurt to run finalizers
		// when none are pending.
	      }
	    runFinalizers ();
	  }
      }
  }
}
