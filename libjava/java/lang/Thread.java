/* Thread -- an independent thread of executable code
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package java.lang;

import gnu.gcj.RawData;
import gnu.gcj.RawDataManaged;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete to version 1.4, with caveats. We do not
 *          implement the deprecated (and dangerous) stop, suspend, and resume
 *          methods. Security implementation is not complete.
 */

/**
 * Thread represents a single thread of execution in the VM. When an
 * application VM starts up, it creates a non-daemon Thread which calls the
 * main() method of a particular class.  There may be other Threads running,
 * such as the garbage collection thread.
 *
 * <p>Threads have names to identify them.  These names are not necessarily
 * unique. Every Thread has a priority, as well, which tells the VM which
 * Threads should get more running time. New threads inherit the priority
 * and daemon status of the parent thread, by default.
 *
 * <p>There are two methods of creating a Thread: you may subclass Thread and
 * implement the <code>run()</code> method, at which point you may start the
 * Thread by calling its <code>start()</code> method, or you may implement
 * <code>Runnable</code> in the class you want to use and then call new
 * <code>Thread(your_obj).start()</code>.
 *
 * <p>The virtual machine runs until all non-daemon threads have died (either
 * by returning from the run() method as invoked by start(), or by throwing
 * an uncaught exception); or until <code>System.exit</code> is called with
 * adequate permissions.
 *
 * <p>It is unclear at what point a Thread should be added to a ThreadGroup,
 * and at what point it should be removed. Should it be inserted when it
 * starts, or when it is created?  Should it be removed when it is suspended
 * or interrupted?  The only thing that is clear is that the Thread should be
 * removed when it is stopped.
 *
 * @author Tom Tromey
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Runnable
 * @see Runtime#exit(int)
 * @see #run()
 * @see #start()
 * @see ThreadLocal
 * @since 1.0
 * @status updated to 1.4
 */
public class Thread implements Runnable
{
  /** The minimum priority for a Thread. */
  public static final int MIN_PRIORITY = 1;

  /** The priority a Thread gets by default. */
  public static final int NORM_PRIORITY = 5;

  /** The maximum priority for a Thread. */
  public static final int MAX_PRIORITY = 10;

  /**
   * The group this thread belongs to. This is set to null by
   * ThreadGroup.removeThread when the thread dies.
   */
  ThreadGroup group;

  /** The object to run(), null if this is the target. */
  private Runnable runnable;

  /** The thread name, non-null. */
  String name;

  /** Whether the thread is a daemon. */
  private boolean daemon;

  /** The thread priority, 1 to 10. */
  private int priority;

  boolean interrupt_flag;
  private boolean alive_flag;
  private boolean startable_flag;

  /** The context classloader for this Thread. */
  private ClassLoader contextClassLoader;

  // This describes the top-most interpreter frame for this thread.
  RawData interp_frame;

  // Our native data - points to an instance of struct natThread.
  private RawDataManaged data;

  /**
   * Allocates a new <code>Thread</code> object. This constructor has
   * the same effect as <code>Thread(null, null,</code>
   * <i>gname</i><code>)</code>, where <b><i>gname</i></b> is
   * a newly generated name. Automatically generated names are of the
   * form <code>"Thread-"+</code><i>n</i>, where <i>n</i> is an integer.
   * <p>
   * Threads created this way must have overridden their
   * <code>run()</code> method to actually do anything.  An example
   * illustrating this method being used follows:
   * <p><blockquote><pre>
   *     import java.lang.*;
   *
   *     class plain01 implements Runnable {
   *         String name;
   *         plain01() {
   *             name = null;
   *         }
   *         plain01(String s) {
   *             name = s;
   *         }
   *         public void run() {
   *             if (name == null)
   *                 System.out.println("A new thread created");
   *             else
   *                 System.out.println("A new thread with name " + name +
   *                                    " created");
   *         }
   *     }
   *     class threadtest01 {
   *         public static void main(String args[] ) {
   *             int failed = 0 ;
   *
   *             <b>Thread t1 = new Thread();</b>
   *             if (t1 != null)
   *                 System.out.println("new Thread() succeed");
   *             else {
   *                 System.out.println("new Thread() failed");
   *                 failed++;
   *             }
   *         }
   *     }
   * </pre></blockquote>
   *
   * @see     java.lang.Thread#Thread(java.lang.ThreadGroup,
   *          java.lang.Runnable, java.lang.String)
   */
  public Thread()
  {
    this(null, null, gen_name());
  }

  /**
   * Allocates a new <code>Thread</code> object. This constructor has
   * the same effect as <code>Thread(null, target,</code>
   * <i>gname</i><code>)</code>, where <i>gname</i> is
   * a newly generated name. Automatically generated names are of the
   * form <code>"Thread-"+</code><i>n</i>, where <i>n</i> is an integer.
   *
   * @param target the object whose <code>run</code> method is called.
   * @see java.lang.Thread#Thread(java.lang.ThreadGroup,
   *                              java.lang.Runnable, java.lang.String)
   */
  public Thread(Runnable target)
  {
    this(null, target, gen_name());
  }

  /**
   * Allocates a new <code>Thread</code> object. This constructor has
   * the same effect as <code>Thread(null, null, name)</code>.
   *
   * @param   name   the name of the new thread.
   * @see     java.lang.Thread#Thread(java.lang.ThreadGroup,
   *          java.lang.Runnable, java.lang.String)
   */
  public Thread(String name)
  {
    this(null, null, name);
  }

  /**
   * Allocates a new <code>Thread</code> object. This constructor has
   * the same effect as <code>Thread(group, target,</code>
   * <i>gname</i><code>)</code>, where <i>gname</i> is
   * a newly generated name. Automatically generated names are of the
   * form <code>"Thread-"+</code><i>n</i>, where <i>n</i> is an integer.
   *
   * @param group the group to put the Thread into
   * @param target the Runnable object to execute
   * @throws SecurityException if this thread cannot access <code>group</code>
   * @throws IllegalThreadStateException if group is destroyed
   * @see #Thread(ThreadGroup, Runnable, String)
   */
  public Thread(ThreadGroup group, Runnable target)
  {
    this(group, target, gen_name());
  }

  /**
   * Allocates a new <code>Thread</code> object. This constructor has
   * the same effect as <code>Thread(group, null, name)</code>
   *
   * @param group the group to put the Thread into
   * @param name the name for the Thread
   * @throws NullPointerException if name is null
   * @throws SecurityException if this thread cannot access <code>group</code>
   * @throws IllegalThreadStateException if group is destroyed
   * @see #Thread(ThreadGroup, Runnable, String)
   */
  public Thread(ThreadGroup group, String name)
  {
    this(group, null, name);
  }

  /**
   * Allocates a new <code>Thread</code> object. This constructor has
   * the same effect as <code>Thread(null, target, name)</code>.
   *
   * @param target the Runnable object to execute
   * @param name the name for the Thread
   * @throws NullPointerException if name is null
   * @see #Thread(ThreadGroup, Runnable, String)
   */
  public Thread(Runnable target, String name)
  {
    this(null, target, name);
  }

  /**
   * Allocate a new Thread object, with the specified ThreadGroup and name, and
   * using the specified Runnable object's <code>run()</code> method to
   * execute.  If the Runnable object is null, <code>this</code> (which is
   * a Runnable) is used instead.
   *
   * <p>If the ThreadGroup is null, the security manager is checked. If a
   * manager exists and returns a non-null object for
   * <code>getThreadGroup</code>, that group is used; otherwise the group
   * of the creating thread is used. Note that the security manager calls
   * <code>checkAccess</code> if the ThreadGroup is not null.
   *
   * <p>The new Thread will inherit its creator's priority and daemon status.
   * These can be changed with <code>setPriority</code> and
   * <code>setDaemon</code>.
   *
   * @param group the group to put the Thread into
   * @param target the Runnable object to execute
   * @param name the name for the Thread
   * @throws NullPointerException if name is null
   * @throws SecurityException if this thread cannot access <code>group</code>
   * @throws IllegalThreadStateException if group is destroyed
   * @see Runnable#run()
   * @see #run()
   * @see #setDaemon(boolean)
   * @see #setPriority(int)
   * @see SecurityManager#checkAccess(ThreadGroup)
   * @see ThreadGroup#checkAccess()
   */
  public Thread(ThreadGroup group, Runnable target, String name)
  {
    this(currentThread(), group, target, name);
  }

  /**
   * Allocate a new Thread object, as if by
   * <code>Thread(group, null, name)</code>, and give it the specified stack
   * size, in bytes. The stack size is <b>highly platform independent</b>,
   * and the virtual machine is free to round up or down, or ignore it
   * completely.  A higher value might let you go longer before a
   * <code>StackOverflowError</code>, while a lower value might let you go
   * longer before an <code>OutOfMemoryError</code>.  Or, it may do absolutely
   * nothing! So be careful, and expect to need to tune this value if your
   * virtual machine even supports it.
   *
   * @param group the group to put the Thread into
   * @param target the Runnable object to execute
   * @param name the name for the Thread
   * @param size the stack size, in bytes; 0 to be ignored
   * @throws NullPointerException if name is null
   * @throws SecurityException if this thread cannot access <code>group</code>
   * @throws IllegalThreadStateException if group is destroyed
   * @since 1.4
   */
  public Thread(ThreadGroup group, Runnable target, String name, long size)
  {
    // Just ignore stackSize for now.
    this(currentThread(), group, target, name);
  }

  private Thread (Thread current, ThreadGroup g, Runnable r, String n)
  {
    // Make sure the current thread may create a new thread.
    checkAccess();
    
    // The Class Libraries book says ``threadName cannot be null''.  I
    // take this to mean NullPointerException.
    if (n == null)
      throw new NullPointerException ();
      
    if (g == null)
      {
	// If CURRENT is null, then we are bootstrapping the first thread. 
	// Use ThreadGroup.root, the main threadgroup.
	if (current == null)
	  group = ThreadGroup.root;
	else
	  group = current.getThreadGroup();
      }
    else
      group = g;
      
    data = null;
    interrupt_flag = false;
    alive_flag = false;
    startable_flag = true;

    if (current != null)
      {
	group.checkAccess();

	daemon = current.isDaemon();
        int gmax = group.getMaxPriority();
	int pri = current.getPriority();
	priority = (gmax < pri ? gmax : pri);
	contextClassLoader = current.contextClassLoader;
	InheritableThreadLocal.newChildThread(this);
      }
    else
      {
	daemon = false;
	priority = NORM_PRIORITY;
      }

    name = n;
    group.addThread(this);
    runnable = r;

    initialize_native ();
  }

  /**
   * Get the number of active threads in the current Thread's ThreadGroup.
   * This implementation calls
   * <code>currentThread().getThreadGroup().activeCount()</code>.
   *
   * @return the number of active threads in the current ThreadGroup
   * @see ThreadGroup#activeCount()
   */
  public static int activeCount()
  {
    return currentThread().group.activeCount();
  }

  /**
   * Check whether the current Thread is allowed to modify this Thread. This
   * passes the check on to <code>SecurityManager.checkAccess(this)</code>.
   *
   * @throws SecurityException if the current Thread cannot modify this Thread
   * @see SecurityManager#checkAccess(Thread)
   */
  public final void checkAccess()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkAccess(this);
  }

  /**
   * Count the number of stack frames in this Thread.  The Thread in question
   * must be suspended when this occurs.
   *
   * @return the number of stack frames in this Thread
   * @throws IllegalThreadStateException if this Thread is not suspended
   * @deprecated pointless, since suspend is deprecated
   */
  public native int countStackFrames();

  /**
   * Get the currently executing Thread.
   *
   * @return the currently executing Thread
   */
  public static native Thread currentThread();

  /**
   * Originally intended to destroy this thread, this method was never
   * implemented by Sun, and is hence a no-op.
   */
  public void destroy()
  {
    throw new NoSuchMethodError();
  }
  
  /**
   * Print a stack trace of the current thread to stderr using the same
   * format as Throwable's printStackTrace() method.
   *
   * @see Throwable#printStackTrace()
   */
  public static void dumpStack()
  {
    (new Exception("Stack trace")).printStackTrace();
  }

  /**
   * Copy every active thread in the current Thread's ThreadGroup into the
   * array. Extra threads are silently ignored. This implementation calls
   * <code>getThreadGroup().enumerate(array)</code>, which may have a
   * security check, <code>checkAccess(group)</code>.
   *
   * @param array the array to place the Threads into
   * @return the number of Threads placed into the array
   * @throws NullPointerException if array is null
   * @throws SecurityException if you cannot access the ThreadGroup
   * @see ThreadGroup#enumerate(Thread[])
   * @see #activeCount()
   * @see SecurityManager#checkAccess(ThreadGroup)
   */
  public static int enumerate(Thread[] array)
  {
    return currentThread().group.enumerate(array);
  }
  
  /**
   * Get this Thread's name.
   *
   * @return this Thread's name
   */
  public final String getName()
  {
    return name;
  }

  /**
   * Get this Thread's priority.
   *
   * @return the Thread's priority
   */
  public final int getPriority()
  {
    return priority;
  }

  /**
   * Get the ThreadGroup this Thread belongs to. If the thread has died, this
   * returns null.
   *
   * @return this Thread's ThreadGroup
   */
  public final ThreadGroup getThreadGroup()
  {
    return group;
  }

  /**
   * Checks whether the current thread holds the monitor on a given object.
   * This allows you to do <code>assert Thread.holdsLock(obj)</code>.
   *
   * @param obj the object to test lock ownership on.
   * @return true if the current thread is currently synchronized on obj
   * @throws NullPointerException if obj is null
   * @since 1.4
   */
  public static native boolean holdsLock(Object obj);

  /**
   * Interrupt this Thread. First, there is a security check,
   * <code>checkAccess</code>. Then, depending on the current state of the
   * thread, various actions take place:
   *
   * <p>If the thread is waiting because of {@link #wait()},
   * {@link #sleep(long)}, or {@link #join()}, its <i>interrupt status</i>
   * will be cleared, and an InterruptedException will be thrown. Notice that
   * this case is only possible if an external thread called interrupt().
   *
   * <p>If the thread is blocked in an interruptible I/O operation, in
   * {@link java.nio.channels.InterruptibleChannel}, the <i>interrupt
   * status</i> will be set, and ClosedByInterruptException will be thrown.
   *
   * <p>If the thread is blocked on a {@link java.nio.channels.Selector}, the
   * <i>interrupt status</i> will be set, and the selection will return, with
   * a possible non-zero value, as though by the wakeup() method.
   *
   * <p>Otherwise, the interrupt status will be set.
   *
   * @throws SecurityException if you cannot modify this Thread
   */
  public native void interrupt();

  /**
   * Determine whether the current Thread has been interrupted, and clear
   * the <i>interrupted status</i> in the process.
   *
   * @return whether the current Thread has been interrupted
   * @see #isInterrupted()
   */
  public static boolean interrupted()
  {
    return currentThread().isInterrupted(true);
  }

  /**
   * Determine whether the given Thread has been interrupted, but leave
   * the <i>interrupted status</i> alone in the process.
   *
   * @return whether the Thread has been interrupted
   * @see #interrupted()
   */
  public boolean isInterrupted()
  {
    return interrupt_flag;
  }

  /**
   * Determine whether this Thread is alive. A thread which is alive has
   * started and not yet died.
   *
   * @return whether this Thread is alive
   */
  public final synchronized boolean isAlive()
  {
    return alive_flag;
  }

  /**
   * Tell whether this is a daemon Thread or not.
   *
   * @return whether this is a daemon Thread or not
   * @see #setDaemon(boolean)
   */
  public final boolean isDaemon()
  {
    return daemon;
  }

  /**
   * Wait forever for the Thread in question to die.
   *
   * @throws InterruptedException if the Thread is interrupted; it's
   *         <i>interrupted status</i> will be cleared
   */
  public final void join() throws InterruptedException
  {
    join(0, 0);
  }

  /**
   * Wait the specified amount of time for the Thread in question to die.
   *
   * @param ms the number of milliseconds to wait, or 0 for forever
   * @throws InterruptedException if the Thread is interrupted; it's
   *         <i>interrupted status</i> will be cleared
   */
  public final void join(long ms) throws InterruptedException
  {
    join(ms, 0);
  }

  /**
   * Wait the specified amount of time for the Thread in question to die.
   *
   * <p>Note that 1,000,000 nanoseconds == 1 millisecond, but most VMs do
   * not offer that fine a grain of timing resolution. Besides, there is
   * no guarantee that this thread can start up immediately when time expires,
   * because some other thread may be active.  So don't expect real-time
   * performance.
   *
   * @param ms the number of milliseconds to wait, or 0 for forever
   * @param ns the number of extra nanoseconds to sleep (0-999999)
   * @throws InterruptedException if the Thread is interrupted; it's
   *         <i>interrupted status</i> will be cleared
   * @throws IllegalArgumentException if ns is invalid
   * @XXX A ThreadListener would be nice, to make this efficient.
   */
  public final native void join(long ms, int ns)
    throws InterruptedException;

  /**
   * Resume a suspended thread.
   *
   * @throws SecurityException if you cannot resume the Thread
   * @see #checkAccess()
   * @see #suspend()
   * @deprecated pointless, since suspend is deprecated
   */
  public final native void resume();

  private final native void finish_();

  /**
   * Determine whether the given Thread has been interrupted, but leave
   * the <i>interrupted status</i> alone in the process.
   *
   * @return whether the current Thread has been interrupted
   * @see #interrupted()
   */
  private boolean isInterrupted(boolean clear_flag)
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
  
  /**
   * The method of Thread that will be run if there is no Runnable object
   * associated with the Thread. Thread's implementation does nothing at all.
   *
   * @see #start()
   * @see #Thread(ThreadGroup, Runnable, String)
   */
  public void run()
  {
    if (runnable != null)
      runnable.run();
  }

  /**
   * Set the daemon status of this Thread.  If this is a daemon Thread, then
   * the VM may exit even if it is still running.  This may only be called
   * before the Thread starts running. There may be a security check,
   * <code>checkAccess</code>.
   *
   * @param daemon whether this should be a daemon thread or not
   * @throws SecurityException if you cannot modify this Thread
   * @throws IllegalThreadStateException if the Thread is active
   * @see #isDaemon()
   * @see #checkAccess()
   */
  public final void setDaemon(boolean daemon)
  {
    if (!startable_flag)
      throw new IllegalThreadStateException();
    checkAccess();
    this.daemon = daemon;
  }

  /**
   * Returns the context classloader of this Thread. The context
   * classloader can be used by code that want to load classes depending
   * on the current thread. Normally classes are loaded depending on
   * the classloader of the current class. There may be a security check
   * for <code>RuntimePermission("getClassLoader")</code> if the caller's
   * class loader is not null or an ancestor of this thread's context class
   * loader.
   *
   * @return the context class loader
   * @throws SecurityException when permission is denied
   * @see setContextClassLoader(ClassLoader)
   * @since 1.2
   */
  public synchronized ClassLoader getContextClassLoader()
  {
    if (contextClassLoader == null)
      contextClassLoader = ClassLoader.getSystemClassLoader();

    SecurityManager sm = System.getSecurityManager();
    // FIXME: we can't currently find the caller's class loader.
    ClassLoader callers = null;
    if (sm != null && callers != null)
      {
	// See if the caller's class loader is the same as or an
	// ancestor of this thread's class loader.
	while (callers != null && callers != contextClassLoader)
	  {
	    // FIXME: should use some internal version of getParent
	    // that avoids security checks.
	    callers = callers.getParent();
	  }

	if (callers != contextClassLoader)
	  sm.checkPermission(new RuntimePermission("getClassLoader"));
      }

    return contextClassLoader;
  }

  /**
   * Sets the context classloader for this Thread. When not explicitly set,
   * the context classloader for a thread is the same as the context
   * classloader of the thread that created this thread. The first thread has
   * as context classloader the system classloader. There may be a security
   * check for <code>RuntimePermission("setContextClassLoader")</code>.
   *
   * @param classloader the new context class loader
   * @throws SecurityException when permission is denied
   * @see getContextClassLoader()
   * @since 1.2
   */
  public synchronized void setContextClassLoader(ClassLoader classloader)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new RuntimePermission("setContextClassLoader"));
    this.contextClassLoader = classloader;
  }

  /**
   * Set this Thread's name.  There may be a security check,
   * <code>checkAccess</code>.
   *
   * @param name the new name for this Thread
   * @throws NullPointerException if name is null
   * @throws SecurityException if you cannot modify this Thread
   */
  public final void setName(String name)
  {
    checkAccess();
    // The Class Libraries book says ``threadName cannot be null''.  I
    // take this to mean NullPointerException.
    if (name == null)
      throw new NullPointerException();
    this.name = name;
  }

  /**
   * Causes the currently executing thread object to temporarily pause
   * and allow other threads to execute.
   */
  public static native void yield();

  /**
   * Suspend the current Thread's execution for the specified amount of
   * time. The Thread will not lose any locks it has during this time. There
   * are no guarantees which thread will be next to run, but most VMs will
   * choose the highest priority thread that has been waiting longest.
   *
   * @param ms the number of milliseconds to sleep, or 0 for forever
   * @throws InterruptedException if the Thread is interrupted; it's
   *         <i>interrupted status</i> will be cleared
   * @see #notify()
   * @see #wait(long)
   */
  public static void sleep(long ms) throws InterruptedException
  {
    sleep(ms, 0);
  }

  /**
   * Suspend the current Thread's execution for the specified amount of
   * time. The Thread will not lose any locks it has during this time. There
   * are no guarantees which thread will be next to run, but most VMs will
   * choose the highest priority thread that has been waiting longest.
   *
   * <p>Note that 1,000,000 nanoseconds == 1 millisecond, but most VMs do
   * not offer that fine a grain of timing resolution. Besides, there is
   * no guarantee that this thread can start up immediately when time expires,
   * because some other thread may be active.  So don't expect real-time
   * performance.
   *
   * @param ms the number of milliseconds to sleep, or 0 for forever
   * @param ns the number of extra nanoseconds to sleep (0-999999)
   * @throws InterruptedException if the Thread is interrupted; it's
   *         <i>interrupted status</i> will be cleared
   * @throws IllegalArgumentException if ns is invalid
   * @see #notify()
   * @see #wait(long, int)
   */
  public static native void sleep(long timeout, int nanos)
    throws InterruptedException;

  /**
   * Start this Thread, calling the run() method of the Runnable this Thread
   * was created with, or else the run() method of the Thread itself. This
   * is the only way to start a new thread; calling run by yourself will just
   * stay in the same thread. The virtual machine will remove the thread from
   * its thread group when the run() method completes.
   *
   * @throws IllegalThreadStateException if the thread has already started
   * @see #run()
   */
  public native void start();

  /**
   * Cause this Thread to stop abnormally because of the throw of a ThreadDeath
   * error. If you stop a Thread that has not yet started, it will stop
   * immediately when it is actually started.
   *
   * <p>This is inherently unsafe, as it can interrupt synchronized blocks and
   * leave data in bad states.  Hence, there is a security check:
   * <code>checkAccess(this)</code>, plus another one if the current thread
   * is not this: <code>RuntimePermission("stopThread")</code>. If you must
   * catch a ThreadDeath, be sure to rethrow it after you have cleaned up.
   * ThreadDeath is the only exception which does not print a stack trace when
   * the thread dies.
   *
   * @throws SecurityException if you cannot stop the Thread
   * @see #interrupt()
   * @see #checkAccess()
   * @see #start()
   * @see ThreadDeath
   * @see ThreadGroup#uncaughtException(Thread, Throwable)
   * @see SecurityManager#checkAccess(Thread)
   * @see SecurityManager#checkPermission(Permission)
   * @deprecated unsafe operation, try not to use
   */
  public final void stop()
  {
    // Argument doesn't matter, because this is no longer
    // supported.
    stop(null);
  }

  /**
   * Cause this Thread to stop abnormally and throw the specified exception.
   * If you stop a Thread that has not yet started, it will stop immediately
   * when it is actually started. <b>WARNING</b>This bypasses Java security,
   * and can throw a checked exception which the call stack is unprepared to
   * handle. Do not abuse this power.
   *
   * <p>This is inherently unsafe, as it can interrupt synchronized blocks and
   * leave data in bad states.  Hence, there is a security check:
   * <code>checkAccess(this)</code>, plus another one if the current thread
   * is not this: <code>RuntimePermission("stopThread")</code>. If you must
   * catch a ThreadDeath, be sure to rethrow it after you have cleaned up.
   * ThreadDeath is the only exception which does not print a stack trace when
   * the thread dies.
   *
   * @param t the Throwable to throw when the Thread dies
   * @throws SecurityException if you cannot stop the Thread
   * @throws NullPointerException in the calling thread, if t is null
   * @see #interrupt()
   * @see #checkAccess()
   * @see #start()
   * @see ThreadDeath
   * @see ThreadGroup#uncaughtException(Thread, Throwable)
   * @see SecurityManager#checkAccess(Thread)
   * @see SecurityManager#checkPermission(Permission)
   * @deprecated unsafe operation, try not to use
   */
  public final native void stop(Throwable t);

  /**
   * Suspend this Thread.  It will not come back, ever, unless it is resumed.
   *
   * <p>This is inherently unsafe, as the suspended thread still holds locks,
   * and can potentially deadlock your program.  Hence, there is a security
   * check: <code>checkAccess</code>.
   *
   * @throws SecurityException if you cannot suspend the Thread
   * @see #checkAccess()
   * @see #resume()
   * @deprecated unsafe operation, try not to use
   */
  public final native void suspend();

  /**
   * Set this Thread's priority. There may be a security check,
   * <code>checkAccess</code>, then the priority is set to the smaller of
   * priority and the ThreadGroup maximum priority.
   *
   * @param priority the new priority for this Thread
   * @throws IllegalArgumentException if priority exceeds MIN_PRIORITY or
   *         MAX_PRIORITY
   * @throws SecurityException if you cannot modify this Thread
   * @see #getPriority()
   * @see #checkAccess()
   * @see ThreadGroup#getMaxPriority()
   * @see #MIN_PRIORITY
   * @see #MAX_PRIORITY
   */
  public final native void setPriority(int newPriority);

  /**
   * Returns a string representation of this thread, including the
   * thread's name, priority, and thread group.
   *
   * @return a human-readable String representing this Thread
   */
  public String toString()
  {
    return ("Thread[" + name + "," + priority + ","
	    + (group == null ? "" : group.getName()) + "]");
  }

  private final native void initialize_native();

  private final native static String gen_name();
}
