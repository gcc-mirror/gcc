/* VMThread -- VM interface for Thread of executable code
   Copyright (C) 2003, 2004, 2005 Free Software Foundation

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

/**
 * VM interface for Thread of executable code. Holds VM dependent state.
 * It is deliberately package local and final and should only be accessed
 * by the Thread class.
 * <p>
 * This is the GNU Classpath reference implementation, it should be adapted
 * for a specific VM.
 * <p>
 * The following methods must be implemented:
 * <ul>
 * <li>native void start(long stacksize);
 * <li>native void interrupt();
 * <li>native boolean isInterrupted();
 * <li>native void suspend();
 * <li>native void resume();
 * <li>native void nativeSetPriority(int priority);
 * <li>native void nativeStop(Throwable t);
 * <li>native static Thread currentThread();
 * <li>static native void yield();
 * <li>static native boolean interrupted();
 * </ul>
 * All other methods may be implemented to make Thread handling more efficient
 * or to implement some optional (and sometimes deprecated) behaviour. Default
 * implementations are provided but it is highly recommended to optimize them
 * for a specific VM.
 * 
 * @author Jeroen Frijters (jeroen@frijters.net)
 * @author Dalibor Topic (robilad@kaffe.org)
 */
final class VMThread
{
    /**
     * The Thread object that this VM state belongs to.
     * Used in currentThread() and start().
     * Note: when this thread dies, this reference is *not* cleared
     */
    volatile Thread thread;

    /**
     * Flag that is set when the thread runs, used by stop() to protect against
     * stop's getting lost.
     */
    private volatile boolean running;

    /**
     * VM private data.
     */
    private transient Object vmdata;

    /**
     * Private constructor, create VMThreads with the static create method.
     *
     * @param thread The Thread object that was just created.
     */
    private VMThread(Thread thread)
    {
	this.thread = thread;
    }

    /**
     * This method is the initial Java code that gets executed when a native
     * thread starts. It's job is to coordinate with the rest of the VMThread
     * logic and to start executing user code and afterwards handle clean up.
     */
    private void run()
    {
	try
	{
	    try
	    {
		running = true;
		synchronized(thread)
		{
		    Throwable t = thread.stillborn;
		    if(t != null)
		    {
			thread.stillborn = null;
			throw t;
		    }
		}
		thread.run();
	    }
	    catch(Throwable t)
	    {
		try
		{
		    thread.group.uncaughtException(thread, t);
		}
		catch(Throwable ignore)
		{
		}
	    }
	}
	finally
	{
	    // Setting runnable to false is partial protection against stop
	    // being called while we're cleaning up. To be safe all code in
	    // VMThread be unstoppable.
	    running = false;
	    thread.die();
	    synchronized(this)
	    {
		// release the threads waiting to join us
		notifyAll();
	    }
	}
    }

    /**
     * Creates a native Thread. This is called from the start method of Thread.
     * The Thread is started.
     *
     * @param thread The newly created Thread object
     * @param stacksize Indicates the requested stacksize. Normally zero,
     * non-zero values indicate requested stack size in bytes but it is up
     * to the specific VM implementation to interpret them and may be ignored.
     */
    static void create(Thread thread, long stacksize)
    {
	VMThread vmThread = new VMThread(thread);
	vmThread.start(stacksize);
	thread.vmThread = vmThread;
    }

    /**
     * Gets the name of the thread. Usually this is the name field of the
     * associated Thread object, but some implementation might choose to
     * return the name of the underlying platform thread.
     */
    String getName()
    {
	return thread.name;
    }

    /**
     * Set the name of the thread. Usually this sets the name field of the
     * associated Thread object, but some implementations might choose to
     * set the name of the underlying platform thread.
     * @param name The new name
     */
    void setName(String name)
    {
	thread.name = name;
    }

    /**
     * Set the thread priority field in the associated Thread object and
     * calls the native method to set the priority of the underlying
     * platform thread.
     * @param priority The new priority
     */
    void setPriority(int priority)
    {
	thread.priority = priority;
	nativeSetPriority(priority);
    }

    /**
     * Returns the priority. Usually this is the priority field from the
     * associated Thread object, but some implementation might choose to
     * return the priority of the underlying platform thread.
     * @return this Thread's priority
     */
    int getPriority()
    {
        return thread.priority;
    }

    /**
     * Returns true if the thread is a daemon thread. Usually this is the
     * daemon field from the associated Thread object, but some
     * implementation might choose to return the daemon state of the underlying
     * platform thread.
     * @return whether this is a daemon Thread or not
     */
    boolean isDaemon()
    {
        return thread.daemon;
    }

    /**
     * Returns the number of stack frames in this Thread.
     * Will only be called when when a previous call to suspend() returned true.
     *
     * @deprecated unsafe operation
     */
    native int countStackFrames();

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
     */
    synchronized void join(long ms, int ns) throws InterruptedException
    {
	// Round up
	ms += (ns != 0) ? 1 : 0;

	// Compute end time, but don't overflow
	long now = System.currentTimeMillis();
	long end = now + ms;
	if (end < now)
	    end = Long.MAX_VALUE;

	// A VM is allowed to return from wait() without notify() having been
	// called, so we loop to handle possible spurious wakeups.
	while(thread.vmThread != null)
	{
	    // We use the VMThread object to wait on, because this is a private
	    // object, so client code cannot call notify on us.
	    wait(ms);
	    if(ms != 0)
	    {
		now = System.currentTimeMillis();
		ms = end - now;
		if(ms <= 0)
		{
		    break;
		}
	    }
	}
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
     * leave data in bad states.
     *
     * <p><b>NOTE</b> stop() should take care not to stop a thread if it is
     * executing code in this class.
     *
     * @param t the Throwable to throw when the Thread dies
     * @deprecated unsafe operation, try not to use
     */
    void stop(Throwable t)
    {
	// Note: we assume that we own the lock on thread
	// (i.e. that Thread.stop() is synchronized)
	if(running)
	    nativeStop(t);
	else
	    thread.stillborn = t;
    }

    /**
     * Create a native thread on the underlying platform and start it executing
     * on the run method of this object.
     * @param stacksize the requested size of the native thread stack
     */
    native void start(long stacksize);

    /**
     * Interrupt this thread.
     */
    native void interrupt();

    /**
     * Determine whether this Thread has been interrupted, but leave
     * the <i>interrupted status</i> alone in the process.
     *
     * @return whether the Thread has been interrupted
     */
    native boolean isInterrupted();

    /**
     * Suspend this Thread.  It will not come back, ever, unless it is resumed.
     */
    native void suspend();

    /**
     * Resume this Thread.  If the thread is not suspended, this method does
     * nothing.
     */
    native void resume();

    /**
     * Set the priority of the underlying platform thread.
     *
     * @param priority the new priority
     */
    native void nativeSetPriority(int priority);

    /**
     * Asynchronously throw the specified throwable in this Thread.
     *
     * @param t the exception to throw
     */
    native void nativeStop(Throwable t);

    /**
     * Return the Thread object associated with the currently executing
     * thread.
     *
     * @return the currently executing Thread
     */
    static native Thread currentThread();

    /**
     * Yield to another thread. The Thread will not lose any locks it holds
     * during this time. There are no guarantees which thread will be
     * next to run, and it could even be this one, but most VMs will choose
     * the highest priority thread that has been waiting longest.
     */
    static native void yield();

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
     * @param ms the number of milliseconds to sleep.
     * @param ns the number of extra nanoseconds to sleep (0-999999)
     * @throws InterruptedException if the Thread is (or was) interrupted;
     *         it's <i>interrupted status</i> will be cleared
     */
    static void sleep(long ms, int ns) throws InterruptedException
    {
      // Note: JDK treats a zero length sleep is like Thread.yield(),
      // without checking the interrupted status of the thread.
      // It's unclear if this is a bug in the implementation or the spec.
      // See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6213203
      if (ms == 0 && ns == 0)
	{
	  if (Thread.interrupted())
	    throw new InterruptedException();
	  return;
	}

      // Compute end time, but don't overflow
      long now = System.currentTimeMillis();
      long end = now + ms;
      if (end < now)
	  end = Long.MAX_VALUE;

      // A VM is allowed to return from wait() without notify() having been
      // called, so we loop to handle possible spurious wakeups.
      VMThread vt = Thread.currentThread().vmThread;
      synchronized (vt)
	{
	  while (true)
	    {
	      vt.wait(ms, ns);
	      now = System.currentTimeMillis();
	      if (now >= end)
		break;
	      ms = end - now;
	      ns = 0;
	    }
	}
    }

    /**
     * Determine whether the current Thread has been interrupted, and clear
     * the <i>interrupted status</i> in the process.
     *
     * @return whether the current Thread has been interrupted
     */
    static native boolean interrupted();

    /**
     * Checks whether the current thread holds the monitor on a given object.
     * This allows you to do <code>assert Thread.holdsLock(obj)</code>.
     *
     * @param obj the object to check
     * @return true if the current thread is currently synchronized on obj
     * @throws NullPointerException if obj is null
     */
    static boolean holdsLock(Object obj) 
    {
      /* Use obj.notify to check if the current thread holds
       * the monitor of the object.
       * If it doesn't, notify will throw an exception.
       */
      try 
	{
	  obj.notify();
	  // okay, current thread holds lock
	  return true;
	}
      catch (IllegalMonitorStateException e)
	{
	  // it doesn't hold the lock
	  return false;
	}
    }
}
