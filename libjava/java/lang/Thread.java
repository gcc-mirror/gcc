// Thread.java - Thread class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import gnu.gcj.RawData;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date August 24, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Complete to version 1.1, with caveats
 * Known problems:
 *   No attempt was made to implement suspend/resume
 *     (this could be done in some cases)
 *   Various methods which assume a VM are likewise unimplemented
 *   We do implement stop() even though it is deprecated.
 */

public class Thread implements Runnable
{
  public final static int MAX_PRIORITY = 10;
  public final static int MIN_PRIORITY = 1;
  public final static int NORM_PRIORITY = 5;

  public static int activeCount ()
  {
    return currentThread().getThreadGroup().activeCount();
  }

  public void checkAccess ()
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkAccess(this);
  }

  public native int countStackFrames ();
  public static native Thread currentThread ();
  public native void destroy ();
  
  public static void dumpStack ()
  {
    (new Exception ("Stack trace")).printStackTrace ();
  }

  public static int enumerate (Thread[] threads)
  {
    return currentThread().group.enumerate(threads);
  }

  public final String getName ()
  {
    return name;
  }

  public final int getPriority ()
  {
    return priority;
  }

  public final ThreadGroup getThreadGroup ()
  {
    return group;
  }

  public native void interrupt ();

  public static boolean interrupted ()
  {
    return currentThread().isInterrupted (true);
  }

  // Check the threads interrupted status. Note that this does not clear the
  // thread's interrupted status (per JDK 1.2 online API documentation).
  public boolean isInterrupted ()
  {
    return interrupt_flag;
  }

  public final boolean isAlive ()
  {
    return alive_flag;
  }

  public final boolean isDaemon ()
  {
    return daemon_flag;
  }

  public final void join () throws InterruptedException
  {
    join (0, 0);
  }

  public final void join (long timeout) throws InterruptedException
  {
    join (timeout, 0);
  }

  public final native void join (long timeout, int nanos)
    throws InterruptedException;

  public final native void resume ();

  // This method exists only to avoid a warning from the C++ compiler.
  private static final native void run_ (Object obj);
  private final native void finish_ ();

  // Check the thread's interrupted status. If clear_flag is true, the 
  // thread's interrupted status is also cleared.
  private boolean isInterrupted (boolean clear_flag)
  {
    boolean r = interrupt_flag;
    if (clear_flag && r)
      {
	// Only clear the flag if we saw it as set. Otherwise this could 
	// potentially cause us to miss an interrupt in a race condition, 
	// because this method is not synchronized.
	interrupt_flag = false;
      }
    return r;
  }
  
  public void run ()
  {
    if (runnable != null)
      runnable.run();
  }

  public final void setDaemon (boolean status)
  {
    checkAccess ();
    if (isAlive ())
      throw new IllegalThreadStateException ();
    daemon_flag = status;
  }

  // TODO12:
  // public ClassLoader getContextClassLoader()
  // {
  // }

  // TODO12:
  // public void setContextClassLoader(ClassLoader cl)
  // {
  // }

  public final void setName (String n)
  {
    checkAccess ();
    // The Class Libraries book says ``threadName cannot be null''.  I
    // take this to mean NullPointerException.
    if (n == null)
      throw new NullPointerException ();
    name = n;
  }

  public final native void setPriority (int newPriority);

  public static void sleep (long timeout) throws InterruptedException
  {
    sleep (timeout, 0);
  }

  public static native void sleep (long timeout, int nanos)
    throws InterruptedException;
  public synchronized native void start ();

  public final void stop ()
  {
    // Argument doesn't matter, because this is no longer
    // supported.
    stop (null);
  }

  public final synchronized native void stop (Throwable e);
  public final native void suspend ();

  private final native void initialize_native ();

  private final synchronized static String gen_name ()
  {
    String n;
    n = "Thread-" + nextThreadNumber;
    ++nextThreadNumber;
    return n;
  }

  public Thread (ThreadGroup g, Runnable r, String n)
  {
    // Note that CURRENT can be null when we are creating the very
    // first thread.  That's why we check it below.
    Thread current = currentThread ();

    if (g != null)
      {
	// If CURRENT is null, then we are creating the first thread.
	// In this case we don't do the security check.
	if (current != null)
	  g.checkAccess();
      }
    else
      g = current.getThreadGroup();

    // The Class Libraries book says ``threadName cannot be null''.  I
    // take this to mean NullPointerException.
    if (n == null)
      throw new NullPointerException ();

    name = n;
    group = g;
    g.add(this);
    runnable = r;

    data = null;
    interrupt_flag = false;
    alive_flag = false;
    startable_flag = true;
    
    if (current != null)
      {
	daemon_flag = current.isDaemon();
	priority = current.getPriority();
      }
    else
      {
	daemon_flag = false;
	priority = NORM_PRIORITY;
      }

    initialize_native ();
  }

  public Thread ()
  {
    this (null, null, gen_name ());
  }

  public Thread (Runnable r)
  {
    this (null, r, gen_name ());
  }

  public Thread (String n)
  {
    this (null, null, n);
  }

  public Thread (ThreadGroup g, Runnable r)
  {
    this (g, r, gen_name ());
  }

  public Thread (ThreadGroup g, String n)
  {
    this (g, null, n);
  }

  public Thread (Runnable r, String n)
  {
    this (null, r, n);
  }

  public String toString ()
  {
    return "Thread[" + name + "," + priority + "," + 
	   (group == null ? "" : group.getName()) + "]";
  }

  public static native void yield ();

  // Private data.
  private ThreadGroup group;
  private String name;
  private Runnable runnable;
  private int priority;
  private boolean daemon_flag;
  private boolean interrupt_flag;
  private boolean alive_flag;
  private boolean startable_flag;

  // Our native data.
  private RawData data;

  // Next thread number to assign.
  private static int nextThreadNumber = 0;
}
