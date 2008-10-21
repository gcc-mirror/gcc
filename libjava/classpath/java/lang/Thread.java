/* Thread -- an independent thread of executable code
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
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

import gnu.classpath.VMStackWalker;
import gnu.java.util.WeakIdentityHashMap;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;

import java.security.Permission;

import java.util.HashMap;
import java.util.Map;

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
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
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

  /** The underlying VM thread, only set when the thread is actually running.
   */
  volatile VMThread vmThread;

  /**
   * The group this thread belongs to. This is set to null by
   * ThreadGroup.removeThread when the thread dies.
   */
  volatile ThreadGroup group;

  /** The object to run(), null if this is the target. */
  final Runnable runnable;

  /** The thread name, non-null. */
  volatile String name;

  /** Whether the thread is a daemon. */
  volatile boolean daemon;

  /** The thread priority, 1 to 10. */
  volatile int priority;

  /** Native thread stack size. 0 = use default */
  private long stacksize;

  /** Was the thread stopped before it was started? */
  Throwable stillborn;

  /** The context classloader for this Thread. */
  private ClassLoader contextClassLoader;
  private boolean contextClassLoaderIsSystemClassLoader;

  /** This thread's ID.  */
  private final long threadId;
  
  /** The park blocker.  See LockSupport.  */
  Object parkBlocker;

  /** The next thread number to use. */
  private static int numAnonymousThreadsCreated;
  
  /** Used to generate the next thread ID to use.  */
  private static long totalThreadsCreated;

  /** The default exception handler.  */
  private static UncaughtExceptionHandler defaultHandler;

  /** Thread local storage. Package accessible for use by
    * InheritableThreadLocal.
    */
  final ThreadLocalMap locals;

  /** The uncaught exception handler.  */
  UncaughtExceptionHandler exceptionHandler;

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
    this(null, (Runnable) null);
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
    this(null, target);
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
    this(null, null, name, 0);
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
    this(group, target, createAnonymousThreadName(), 0);
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
    this(group, null, name, 0);
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
    this(null, target, name, 0);
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
    this(group, target, name, 0);
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
    // Bypass System.getSecurityManager, for bootstrap efficiency.
    SecurityManager sm = SecurityManager.current;
    Thread current = currentThread();
    if (group == null)
      {
	if (sm != null)
	  group = sm.getThreadGroup();
	if (group == null)
	  group = current.group;
      }
    if (sm != null)
      sm.checkAccess(group);

    this.group = group;
    // Use toString hack to detect null.
    this.name = name.toString();
    this.runnable = target;
    this.stacksize = size;
    this.locals = new ThreadLocalMap();
    
    synchronized (Thread.class)
      {
        this.threadId = ++totalThreadsCreated;
      }

    priority = current.priority;
    daemon = current.daemon;
    contextClassLoader = current.contextClassLoader;
    contextClassLoaderIsSystemClassLoader =
        current.contextClassLoaderIsSystemClassLoader;

    group.addThread(this);
    InheritableThreadLocal.newChildThread(this);
  }

  /**
   * Used by the VM to create thread objects for threads started outside
   * of Java. Note: caller is responsible for adding the thread to
   * a group and InheritableThreadLocal.
   * Note: This constructor should not call any methods that could result
   * in a call to Thread.currentThread(), because that makes life harder
   * for the VM.
   *
   * @param vmThread the native thread
   * @param name the thread name or null to use the default naming scheme
   * @param priority current priority
   * @param daemon is the thread a background thread?
   */
  Thread(VMThread vmThread, String name, int priority, boolean daemon)
  {
    this.locals = new ThreadLocalMap();
    this.vmThread = vmThread;
    this.runnable = null;
    if (name == null)
      name = createAnonymousThreadName();
    this.name = name;
    this.priority = priority;
    this.daemon = daemon;
    // By default the context class loader is the system class loader,
    // we set a flag to signal this because we don't want to call
    // ClassLoader.getSystemClassLoader() at this point, because on
    // VMs that lazily create the system class loader that might result
    // in running user code (when a custom system class loader is specified)
    // and that user code could call Thread.currentThread().
    // ClassLoader.getSystemClassLoader() can also return null, if the system
    // is currently in the process of constructing the system class loader
    // (and, as above, the constructiong sequence calls Thread.currenThread()).
    contextClassLoaderIsSystemClassLoader = true;
    synchronized (Thread.class)
    {
      this.threadId = ++totalThreadsCreated;
    }
  }
  
  /**
   * Generate a name for an anonymous thread.
   */
  private static synchronized String createAnonymousThreadName()
  {
    return "Thread-" + ++numAnonymousThreadsCreated;
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
    // Bypass System.getSecurityManager, for bootstrap efficiency.
    SecurityManager sm = SecurityManager.current;
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
  public int countStackFrames()
  {
    VMThread t = vmThread;
    if (t == null || group == null)
      throw new IllegalThreadStateException();

    return t.countStackFrames();
  }

  /**
   * Get the currently executing Thread. In the situation that the
   * currently running thread was created by native code and doesn't
   * have an associated Thread object yet, a new Thread object is
   * constructed and associated with the native thread.
   *
   * @return the currently executing Thread
   */
  public static Thread currentThread()
  {
    return VMThread.currentThread();
  }

  /**
   * Originally intended to destroy this thread, this method was never
   * implemented by Sun, and is hence a no-op.
   *
   * @deprecated This method was originally intended to simply destroy
   *             the thread without performing any form of cleanup operation.
   *             However, it was never implemented.  It is now deprecated
   *             for the same reason as <code>suspend()</code>,
   *             <code>stop()</code> and <code>resume()</code>; namely,
   *             it is prone to deadlocks.  If a thread is destroyed while
   *             it still maintains a lock on a resource, then this resource
   *             will remain locked and any attempts by other threads to
   *             access the resource will result in a deadlock.  Thus, even
   *             an implemented version of this method would be still be
   *             deprecated, due to its unsafe nature.
   * @throws NoSuchMethodError as this method was never implemented.
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
    new Throwable().printStackTrace();
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
    VMThread t = vmThread;
    return t == null ? name : t.getName();
  }

  /**
   * Get this Thread's priority.
   *
   * @return the Thread's priority
   */
  public final synchronized int getPriority()
  {
    VMThread t = vmThread;
    return t == null ? priority : t.getPriority();
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
  public static boolean holdsLock(Object obj)
  {
    return VMThread.holdsLock(obj);
  }

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
  public synchronized void interrupt()
  {
    checkAccess();
    VMThread t = vmThread;
    if (t != null)
      t.interrupt();
  }

  /**
   * Determine whether the current Thread has been interrupted, and clear
   * the <i>interrupted status</i> in the process.
   *
   * @return whether the current Thread has been interrupted
   * @see #isInterrupted()
   */
  public static boolean interrupted()
  {
    return VMThread.interrupted();
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
    VMThread t = vmThread;
    return t != null && t.isInterrupted();
  }

  /**
   * Determine whether this Thread is alive. A thread which is alive has
   * started and not yet died.
   *
   * @return whether this Thread is alive
   */
  public final boolean isAlive()
  {
    return vmThread != null && group != null;
  }

  /**
   * Tell whether this is a daemon Thread or not.
   *
   * @return whether this is a daemon Thread or not
   * @see #setDaemon(boolean)
   */
  public final boolean isDaemon()
  {
    VMThread t = vmThread;
    return t == null ? daemon : t.isDaemon();
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
   */
  public final void join(long ms, int ns) throws InterruptedException
  {
    if (ms < 0 || ns < 0 || ns > 999999)
      throw new IllegalArgumentException();

    VMThread t = vmThread;
    if (t != null)
      t.join(ms, ns);
  }

  /**
   * Resume this Thread.  If the thread is not suspended, this method does
   * nothing. To mirror suspend(), there may be a security check:
   * <code>checkAccess</code>.
   *
   * @throws SecurityException if you cannot resume the Thread
   * @see #checkAccess()
   * @see #suspend()
   * @deprecated pointless, since suspend is deprecated
   */
  public final synchronized void resume()
  {
    checkAccess();
    VMThread t = vmThread;
    if (t != null)
      t.resume();
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
  public final synchronized void setDaemon(boolean daemon)
  {
    if (vmThread != null)
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
   * @see #setContextClassLoader(ClassLoader)
   * @since 1.2
   */
  public synchronized ClassLoader getContextClassLoader()
  {
    ClassLoader loader = contextClassLoaderIsSystemClassLoader ?
        ClassLoader.getSystemClassLoader() : contextClassLoader;
    // Check if we may get the classloader
    SecurityManager sm = SecurityManager.current;
    if (loader != null && sm != null)
      {
        // Get the calling classloader
	ClassLoader cl = VMStackWalker.getCallingClassLoader();
        if (cl != null && !cl.isAncestorOf(loader))
          sm.checkPermission(new RuntimePermission("getClassLoader"));
      }
    return loader;
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
   * @see #getContextClassLoader()
   * @since 1.2
   */
  public synchronized void setContextClassLoader(ClassLoader classloader)
  {
    SecurityManager sm = SecurityManager.current;
    if (sm != null)
      sm.checkPermission(new RuntimePermission("setContextClassLoader"));
    this.contextClassLoader = classloader;
    contextClassLoaderIsSystemClassLoader = false;
  }

  /**
   * Set this Thread's name.  There may be a security check,
   * <code>checkAccess</code>.
   *
   * @param name the new name for this Thread
   * @throws NullPointerException if name is null
   * @throws SecurityException if you cannot modify this Thread
   */
  public final synchronized void setName(String name)
  {
    checkAccess();
    // The Class Libraries book says ``threadName cannot be null''.  I
    // take this to mean NullPointerException.
    if (name == null)
      throw new NullPointerException();
    VMThread t = vmThread;
    if (t != null)
      t.setName(name);
    else
      this.name = name;
  }

  /**
   * Yield to another thread. The Thread will not lose any locks it holds
   * during this time. There are no guarantees which thread will be
   * next to run, and it could even be this one, but most VMs will choose
   * the highest priority thread that has been waiting longest.
   */
  public static void yield()
  {
    VMThread.yield();
  }

  /**
   * Suspend the current Thread's execution for the specified amount of
   * time. The Thread will not lose any locks it has during this time. There
   * are no guarantees which thread will be next to run, but most VMs will
   * choose the highest priority thread that has been waiting longest.
   *
   * @param ms the number of milliseconds to sleep, or 0 for forever
   * @throws InterruptedException if the Thread is (or was) interrupted;
   *         it's <i>interrupted status</i> will be cleared
   * @throws IllegalArgumentException if ms is negative
   * @see #interrupt()
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
   * <p>
   * Note that 1,000,000 nanoseconds == 1 millisecond, but most VMs
   * do not offer that fine a grain of timing resolution. When ms is
   * zero and ns is non-zero the Thread will sleep for at least one
   * milli second. There is no guarantee that this thread can start up
   * immediately when time expires, because some other thread may be
   * active.  So don't expect real-time performance.
   *
   * @param ms the number of milliseconds to sleep, or 0 for forever
   * @param ns the number of extra nanoseconds to sleep (0-999999)
   * @throws InterruptedException if the Thread is (or was) interrupted;
   *         it's <i>interrupted status</i> will be cleared
   * @throws IllegalArgumentException if ms or ns is negative
   *         or ns is larger than 999999.
   * @see #interrupt()
   * @see #notify()
   * @see #wait(long, int)
   */
  public static void sleep(long ms, int ns) throws InterruptedException
  {
    // Check parameters
    if (ms < 0 )
      throw new IllegalArgumentException("Negative milliseconds: " + ms);

    if (ns < 0 || ns > 999999)
      throw new IllegalArgumentException("Nanoseconds ouf of range: " + ns);

    // Really sleep
    VMThread.sleep(ms, ns);
  }

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
  public synchronized void start()
  {
    if (vmThread != null || group == null)
      throw new IllegalThreadStateException();

    VMThread.create(this, stacksize);
  }
  
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
    stop(new ThreadDeath());
  }

  /**
   * Cause this Thread to stop abnormally and throw the specified exception.
   * If you stop a Thread that has not yet started, the stop is ignored
   * (contrary to what the JDK documentation says).
   * <b>WARNING</b>This bypasses Java security, and can throw a checked
   * exception which the call stack is unprepared to handle. Do not abuse
   * this power.
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
  public final synchronized void stop(Throwable t)
  {
    if (t == null)
      throw new NullPointerException();
    // Bypass System.getSecurityManager, for bootstrap efficiency.
    SecurityManager sm = SecurityManager.current;
    if (sm != null)
      {
        sm.checkAccess(this);
        if (this != currentThread() || !(t instanceof ThreadDeath))
          sm.checkPermission(new RuntimePermission("stopThread"));
      }
    VMThread vt = vmThread;
    if (vt != null)
	vt.stop(t);
    else
	stillborn = t;
  }

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
  public final synchronized void suspend()
  {
    checkAccess();
    VMThread t = vmThread;
    if (t != null)
      t.suspend();
  }

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
  public final synchronized void setPriority(int priority)
  {
    checkAccess();
    if (priority < MIN_PRIORITY || priority > MAX_PRIORITY)
      throw new IllegalArgumentException("Invalid thread priority value "
                                         + priority + ".");
    priority = Math.min(priority, group.getMaxPriority());
    VMThread t = vmThread;
    if (t != null)
      t.setPriority(priority);
    else
      this.priority = priority;
  }

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

  /**
   * Clean up code, called by VMThread when thread dies.
   */
  synchronized void die()
  {
    group.removeThread(this);
    vmThread = null;
    locals.clear();
  }

  /**
   * Returns the map used by ThreadLocal to store the thread local values.
   */
  static ThreadLocalMap getThreadLocals()
  {
    return currentThread().locals;
  }

  /** 
   * Assigns the given <code>UncaughtExceptionHandler</code> to this
   * thread.  This will then be called if the thread terminates due
   * to an uncaught exception, pre-empting that of the
   * <code>ThreadGroup</code>.
   *
   * @param h the handler to use for this thread.
   * @throws SecurityException if the current thread can't modify this thread.
   * @since 1.5 
   */
  public void setUncaughtExceptionHandler(UncaughtExceptionHandler h)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkAccess(this);    
    exceptionHandler = h;
  }

  /** 
   * <p>
   * Returns the handler used when this thread terminates due to an
   * uncaught exception.  The handler used is determined by the following:
   * </p>
   * <ul>
   * <li>If this thread has its own handler, this is returned.</li>
   * <li>If not, then the handler of the thread's <code>ThreadGroup</code>
   * object is returned.</li>
   * <li>If both are unavailable, then <code>null</code> is returned
   *     (which can only happen when the thread was terminated since
   *      then it won't have an associated thread group anymore).</li>
   * </ul>
   * 
   * @return the appropriate <code>UncaughtExceptionHandler</code> or
   *         <code>null</code> if one can't be obtained.
   * @since 1.5 
   */
  public UncaughtExceptionHandler getUncaughtExceptionHandler()
  {
    return exceptionHandler != null ? exceptionHandler : group;
  }

  /** 
   * <p>
   * Sets the default uncaught exception handler used when one isn't
   * provided by the thread or its associated <code>ThreadGroup</code>.
   * This exception handler is used when the thread itself does not
   * have an exception handler, and the thread's <code>ThreadGroup</code>
   * does not override this default mechanism with its own.  As the group
   * calls this handler by default, this exception handler should not defer
   * to that of the group, as it may lead to infinite recursion.
   * </p>
   * <p>
   * Uncaught exception handlers are used when a thread terminates due to
   * an uncaught exception.  Replacing this handler allows default code to
   * be put in place for all threads in order to handle this eventuality.
   * </p>
   *
   * @param h the new default uncaught exception handler to use.
   * @throws SecurityException if a security manager is present and
   *                           disallows the runtime permission
   *                           "setDefaultUncaughtExceptionHandler".
   * @since 1.5 
   */
  public static void 
    setDefaultUncaughtExceptionHandler(UncaughtExceptionHandler h)
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new RuntimePermission("setDefaultUncaughtExceptionHandler"));    
    defaultHandler = h;
  }

  /** 
   * Returns the handler used by default when a thread terminates
   * unexpectedly due to an exception, or <code>null</code> if one doesn't
   * exist.
   *
   * @return the default uncaught exception handler.
   * @since 1.5 
   */
  public static UncaughtExceptionHandler getDefaultUncaughtExceptionHandler()
  {
    return defaultHandler;
  }
  
  /** 
   * Returns the unique identifier for this thread.  This ID is generated
   * on thread creation, and may be re-used on its death.
   *
   * @return a positive long number representing the thread's ID.
   * @since 1.5 
   */
  public long getId()
  {
    return threadId;
  }

  /**
   * <p>
   * This interface is used to handle uncaught exceptions
   * which cause a <code>Thread</code> to terminate.  When
   * a thread, t, is about to terminate due to an uncaught
   * exception, the virtual machine looks for a class which
   * implements this interface, in order to supply it with
   * the dying thread and its uncaught exception.
   * </p>
   * <p>
   * The virtual machine makes two attempts to find an
   * appropriate handler for the uncaught exception, in
   * the following order:
   * </p>
   * <ol>
   * <li>
   * <code>t.getUncaughtExceptionHandler()</code> --
   * the dying thread is queried first for a handler
   * specific to that thread.
   * </li>
   * <li>
   * <code>t.getThreadGroup()</code> --
   * the thread group of the dying thread is used to
   * handle the exception.  If the thread group has
   * no special requirements for handling the exception,
   * it may simply forward it on to
   * <code>Thread.getDefaultUncaughtExceptionHandler()</code>,
   * the default handler, which is used as a last resort.
   * </li>
   * </ol>
   * <p>
   * The first handler found is the one used to handle
   * the uncaught exception.
   * </p>
   *
   * @author Tom Tromey <tromey@redhat.com>
   * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
   * @since 1.5
   * @see Thread#getUncaughtExceptionHandler()
   * @see Thread#setUncaughtExceptionHandler(UncaughtExceptionHandler)
   * @see Thread#getDefaultUncaughtExceptionHandler()
   * @see
   * Thread#setDefaultUncaughtExceptionHandler(java.lang.Thread.UncaughtExceptionHandler)
   */
  public interface UncaughtExceptionHandler
  {
    /**
     * Invoked by the virtual machine with the dying thread
     * and the uncaught exception.  Any exceptions thrown
     * by this method are simply ignored by the virtual
     * machine.
     *
     * @param thr the dying thread.
     * @param exc the uncaught exception.
     */
    void uncaughtException(Thread thr, Throwable exc);
  }

  /** 
   * <p>
   * Represents the current state of a thread, according to the VM rather
   * than the operating system.  It can be one of the following:
   * </p>
   * <ul>
   * <li>NEW -- The thread has just been created but is not yet running.</li>
   * <li>RUNNABLE -- The thread is currently running or can be scheduled
   * to run.</li>
   * <li>BLOCKED -- The thread is blocked waiting on an I/O operation
   * or to obtain a lock.</li>
   * <li>WAITING -- The thread is waiting indefinitely for another thread
   * to do something.</li>
   * <li>TIMED_WAITING -- The thread is waiting for a specific amount of time
   * for another thread to do something.</li>
   * <li>TERMINATED -- The thread has exited.</li>
   * </ul>
   *
   * @since 1.5 
   */
  public enum State
  {
    BLOCKED, NEW, RUNNABLE, TERMINATED, TIMED_WAITING, WAITING;

    /**
     * For compatability with Sun's JDK
     */
    private static final long serialVersionUID = 605505746047245783L;
  }


  /**
   * Returns the current state of the thread.  This
   * is designed for monitoring thread behaviour, rather
   * than for synchronization control.
   *
   * @return the current thread state.
   */
  public State getState()
  {
    VMThread t = vmThread;
    if (t != null)
      return State.valueOf(t.getState());
    if (group == null)
      return State.TERMINATED;
    return State.NEW;
  }

  /**
   * <p>
   * Returns a map of threads to stack traces for each
   * live thread.  The keys of the map are {@link Thread}
   * objects, which map to arrays of {@link StackTraceElement}s.
   * The results obtained from Calling this method are
   * equivalent to calling {@link getStackTrace()} on each
   * thread in succession.  Threads may be executing while
   * this takes place, and the results represent a snapshot
   * of the thread at the time its {@link getStackTrace()}
   * method is called.
   * </p>
   * <p>
   * The stack trace information contains the methods called
   * by the thread, with the most recent method forming the
   * first element in the array.  The array will be empty
   * if the virtual machine can not obtain information on the
   * thread. 
   * </p>
   * <p>
   * To execute this method, the current security manager
   * (if one exists) must allow both the
   * <code>"getStackTrace"</code> and
   * <code>"modifyThreadGroup"</code> {@link RuntimePermission}s.
   * </p>
   * 
   * @return a map of threads to arrays of {@link StackTraceElement}s.
   * @throws SecurityException if a security manager exists, and
   *                           prevents either or both the runtime
   *                           permissions specified above.
   * @since 1.5
   * @see #getStackTrace()
   */
  public static Map<Thread, StackTraceElement[]> getAllStackTraces()
  {
    ThreadGroup group = currentThread().group;
    while (group.getParent() != null)
      group = group.getParent();
    int arraySize = group.activeCount();
    Thread[] threadList = new Thread[arraySize];
    int filled = group.enumerate(threadList);
    while (filled == arraySize)
      {
	arraySize *= 2;
	threadList = new Thread[arraySize];
	filled = group.enumerate(threadList);
      }
    Map traces = new HashMap();
    for (int a = 0; a < filled; ++a)
      traces.put(threadList[a],
		 threadList[a].getStackTrace());
    return traces;
  }

  /**
   * <p>
   * Returns an array of {@link StackTraceElement}s
   * representing the current stack trace of this thread.
   * The first element of the array is the most recent
   * method called, and represents the top of the stack.
   * The elements continue in this order, with the last
   * element representing the bottom of the stack.
   * </p>
   * <p>
   * A zero element array is returned for threads which
   * have not yet started (and thus have not yet executed
   * any methods) or for those which have terminated.
   * Where the virtual machine can not obtain a trace for
   * the thread, an empty array is also returned.  The
   * virtual machine may also omit some methods from the
   * trace in non-zero arrays.
   * </p>
   * <p>
   * To execute this method, the current security manager
   * (if one exists) must allow both the
   * <code>"getStackTrace"</code> and
   * <code>"modifyThreadGroup"</code> {@link RuntimePermission}s.
   * </p>
   *
   * @return a stack trace for this thread.
   * @throws SecurityException if a security manager exists, and
   *                           prevents the use of the
   *                           <code>"getStackTrace"</code>
   *                           permission.
   * @since 1.5
   * @see #getAllStackTraces()
   */
  public StackTraceElement[] getStackTrace()
  {
    SecurityManager sm = SecurityManager.current; // Be thread-safe.
    if (sm != null)
      sm.checkPermission(new RuntimePermission("getStackTrace"));
    ThreadMXBean bean = ManagementFactory.getThreadMXBean();
    ThreadInfo info = bean.getThreadInfo(threadId, Integer.MAX_VALUE);
    return info.getStackTrace();
  }

}
