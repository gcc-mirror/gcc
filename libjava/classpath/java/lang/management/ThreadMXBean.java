/* ThreadMXBean.java - Interface for a thread bean
   Copyright (C) 2006 Free Software Foundation

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

package java.lang.management;

/**
 * <p>
 * Provides access to information about the threads
 * of the virtual machine.  An instance of this bean is
 * obtained by calling
 * {@link ManagementFactory#getThreadMXBean()}.
 * </p>
 * <p>
 * Each thread within the virtual machine is given an
 * identifier, which is guaranteed to be unique to a
 * particular thread over its lifetime (after which it
 * may be reused). The identifier for a thread may be
 * obtained by calling {@link java.lang.Thread#getId()}.
 * This identifier is used within implementations of this
 * interface to obtain information about a particular thread
 * (or series of threads, in the case of an array of identifiers).
 * </p>
 * <p>
 * This bean supports some optional behaviour, which all
 * virtual machines may not choose to implement.  Specifically,
 * this includes the monitoring of:
 * </p>
 * <ul>
 * <li>the CPU time used by a thread</li>
 * <li>thread contention</li>
 * <li>object monitor usage</li>
 * <li>ownable synchronizer usage</li>
 * </ul>
 * <p>
 * The monitoring of CPU time is further subdivided into
 * the monitoring of either just the current thread or all
 * threads.  The methods
 * {@link #isThreadCpuTimeSupported()},
 * {@link #isCurrentThreadCpuTimeSupported()}
 * {@link #isThreadContentionMonitoringSupported()},
 * {@link #isObjectMonitorUsageSupported()} and
 * {@link #isSynchronizerUsageSupported()} may be
 * used to determine whether or not this functionality is
 * supported.
 * </p>
 * <p>
 * Furthermore, both time and contention monitoring may be
 * disabled.  In fact, thread contention monitoring is disabled
 * by default, and must be explictly turned on by calling
 * the {@link #setThreadContentionMonitoringEnabled(boolean)}
 * method.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface ThreadMXBean
{

  /**
   * This method returns information on all live threads at the
   * time of execution (some threads may have terminated by the
   * time the method completes).  This method is simply a shorthand
   * for calling {@link #getThreadInfo(long[], boolean,
   * boolean)} with the return value of {@link #getAllThreadIds()}.
   *
   * @param lockedMonitors true if the returned {@link ThreadInfo}
   *                       objects should contain information on
   *                       locked monitors.
   * @param lockedSynchronizers true if the returned {@link ThreadInfo}
   *                            objects should contain information
   *                            on locked ownable synchronizers.
   * @return an array of {@link ThreadInfo} objects for all live threads.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   * @throws UnsupportedOperationException if <code>lockedMonitors</code>
   *                                       is true, but object monitor
   *                                       usage monitoring is not supported
   *                                       by the VM, or
   *                                       <code>lockedSynchronizers</code>
   *                                       is true, but ownable synchronizer
   *                                       usage monitoring is not supported
   *                                       by the VM.
   * @since 1.6
   * @see #getThreadInfo(long[], boolean, boolean)
   * @see #getAllThreadIds()
   * @see #isObjectMonitorUsageSupported()
   * @see #isSynchronizerUsageSupported()
   */
  ThreadInfo[] dumpAllThreads(boolean lockedMonitors,
                              boolean lockedSynchronizers);

  /**
   * <p>
   * This method obtains a list of threads which are deadlocked
   * waiting to obtain monitor or ownable synchronizer ownership.
   * This is similar to the behaviour described for
   * {@link #getMonitorDeadlockedThreads()}, except this method also
   * takes in to account deadlocks involving ownable synchronizers.
   * </p>
   * <p>
   * Note that this method is not designed for controlling
   * synchronization, but for troubleshooting problems which cause such
   * deadlocks; it may be prohibitively expensive to use in normal
   * operation.  If only deadlocks involving monitors are of interest,
   * then {@link #findMonitorDeadlockedThreads()} should be used in
   * preference to this method.
   * </p>
   *
   * @return an array of thread identifiers, corresponding to threads
   *         which are currently in a deadlocked situation, or
   *         <code>null</code> if there are no deadlocks.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   * @throws UnsupportedOperationException if the VM does not support
   *                                       the monitoring of ownable
   *                                       synchronizer usage.
   * @since 1.6
   * @see #findMonitorDeadlockedThreads()
   * @see #isSynchronizerUsageSupported()
   */
  long[] findDeadlockedThreads();

  /**
   * <p>
   * This method obtains a list of threads which are deadlocked
   * waiting to obtain monitor ownership.  On entering a synchronized
   * method of an object, or re-entering it after returning from an
   * {@link java.lang.Object#wait()} call, a thread obtains ownership
   * of the object's monitor.
   * </p>
   * <p>
   * Deadlocks can occur in this situation if one or more threads end up
   * waiting for a monitor, P, while also retaining ownership of a monitor,
   * Q, required by the thread that currently owns P.  To give a simple
   * example, imagine thread A calls a synchronized method, R, obtaining the
   * monitor, P.  It then sleeps within that method, allowing thread B
   * to run, but still retaining ownership of P.  B calls another
   * synchronized method, S, which causes it to obtain the monitor, Q,
   * of a different object.  While in that method, it then wants to
   * call the original synchronized method, R,  called by A.  Doing so
   * requires ownership of P, which is still held by A.  Hence, it
   * becomes blocked.
   * </p>
   * <p>
   * A then finishes its sleep, becomes runnable, and is then allowed
   * to run, being the only eligible thread in this scenario.  A tries
   * to call the synchronized method, S.  It also gets blocked, because
   * B still holds the monitor, Q.  Hence, the two threads, A and B,
   * are deadlocked, as neither can give up its monitor without first
   * obtaining the monitor held by the other thread.
   * </p>
   * <p>
   * Calling this method in this scenario would return the thread IDs
   * of A and B.  Note that this method is not designed for controlling
   * synchronization, but for troubleshooting problems which cause such
   * deadlocks; it may be prohibitively expensive to use in normal
   * operation.  This method only returns deadlocks involving monitors;
   * to include deadlocks involving ownable synchronizers,
   * {@link #findDeadlockedThreads()} should be used instead.
   * </p>
   *
   * @return an array of thread identifiers, corresponding to threads
   *         which are currently in a deadlocked situation, or
   *         <code>null</code> if there are no deadlocks.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   * @see #findDeadlockedThreads()
   */
  long[] findMonitorDeadlockedThreads();

  /**
   * Returns all live thread identifiers at the time of initial
   * execution.  Some thread identifiers in the returned array
   * may refer to terminated threads, if this occurs during the
   * lifetime of this method.
   *
   * @return an array of thread identifiers, corresponding to
   *         current live threads.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   */
  long[] getAllThreadIds();

  /**
   * <p>
   * Returns the total number of nanoseconds of CPU time
   * the current thread has used.  This is equivalent to calling
   * <code>{@link #getThreadCpuTime()}(Thread.currentThread.getId())</code>.
   * </p>
   * <p>
   * Note that the value is only nanosecond-precise, and not accurate; there
   * is no guarantee that the difference between two values is really a
   * nanosecond.  Also, the value is prone to overflow if the offset
   * exceeds 2^63.  The use of this method depends on virtual machine
   * support for measurement of the CPU time of the current thread,
   * and on this functionality being enabled.
   * </p>
   *
   * @return the total number of nanoseconds of CPU time the current
   *         thread has used, or -1 if CPU time monitoring is disabled.
   * @throws UnsupportedOperationException if CPU time monitoring is not
   *                                       supported.
   * @see #getCurrentThreadUserTime()
   * @see #isCurrentThreadCpuTimeSupported()
   * @see #isThreadCpuTimeEnabled()
   * @see #setThreadCpuTimeEnabled(boolean)
   */
  long getCurrentThreadCpuTime();

  /**
   * <p>
   * Returns the total number of nanoseconds of CPU time
   * the current thread has executed in user mode.  This is
   * equivalent to calling
   * <code>{@link #getThreadUserTime()}(Thread.currentThread.getId())</code>.
   * </p>
   * <p>
   * Note that the value is only nanosecond-precise, and not accurate; there
   * is no guarantee that the difference between two values is really a
   * nanosecond.  Also, the value is prone to overflow if the offset
   * exceeds 2^63.  The use of this method depends on virtual machine
   * support for measurement of the CPU time of the current thread,
   * and on this functionality being enabled.
   * </p>
   *
   * @return the total number of nanoseconds of CPU time the current
   *         thread has executed in user mode, or -1 if CPU time
   *         monitoring is disabled.
   * @throws UnsupportedOperationException if CPU time monitoring is not
   *                                       supported.
   * @see #getCurrentThreadCpuTime()
   * @see #isCurrentThreadCpuTimeSupported()
   * @see #isThreadCpuTimeEnabled()
   * @see #setThreadCpuTimeEnabled(boolean)
   */
  long getCurrentThreadUserTime();

  /**
   * Returns the number of live daemon threads.
   *
   * @return the number of live daemon threads.
   */
  int getDaemonThreadCount();

  /**
   * Returns the peak number of live threads since
   * the virtual machine was started or the count
   * reset using {@link #resetPeakThreadCount()}.
   *
   * @return the peak live thread count.
   * @see #resetPeakThreadCount()
   */
  int getPeakThreadCount();

  /**
   * Returns the number of live threads, including
   * both daemon threads and non-daemon threads.
   *
   * @return the current number of live threads.
   */
  int getThreadCount();

  /**
   * <p>
   * Returns the total number of nanoseconds of CPU time
   * the specified thread has used.
   * </p>
   * <p>
   * Note that the value is only nanosecond-precise, and not accurate; there
   * is no guarantee that the difference between two values is really a
   * nanosecond.  Also, the value is prone to overflow if the offset
   * exceeds 2^63.  The use of this method depends on virtual machine
   * support for measurement of the CPU time of the current thread,
   * and on this functionality being enabled.
   * </p>
   *
   * @param id the thread identifier of the thread whose CPU time is being
   *           monitored.
   * @return the total number of nanoseconds of CPU time the specified
   *         thread has used, or -1 if CPU time monitoring is disabled.
   * @throws IllegalArgumentException if <code>id</code> <= 0.
   * @throws UnsupportedOperationException if CPU time monitoring is not
   *                                       supported.
   * @see #getThreadUserTime(long)
   * @see #isThreadCpuTimeSupported()
   * @see #isThreadCpuTimeEnabled()
   * @see #setThreadCpuTimeEnabled(boolean)
   */
  long getThreadCpuTime(long id);

  /**
   * Returns information on the specified thread without any
   * stack trace information.  This is equivalent to
   * <code>{@link #getThreadInfo}(id, 0)</code>.  If the
   * identifier specifies a thread which is either non-existant
   * or not alive, then the method returns <code>null</code>.
   *
   * @param id the identifier of the thread to return information
   *           on.
   * @return a {@link ThreadInfo} object pertaining to the specified
   *         thread, or <code>null</code> if the identifier specifies
   *         a thread that doesn't exist or is not alive.
   * @throws IllegalArgumentException if <code>id</code> <= 0.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   */
  ThreadInfo getThreadInfo(long id);

  /**
   * Returns information on the specified threads without any
   * stack trace information.  This is equivalent to
   * <code>{@link #getThreadInfo}(ids, 0)</code>.  If an
   * identifier specifies a thread which is either non-existant
   * or not alive, then the corresponding element in the returned
   * array is <code>null</code>.
   *
   * @param ids an array of thread identifiers to return information
   *           on.
   * @return an array of {@link ThreadInfo} objects matching the
   *         specified threads.  The corresponding element is
   *         <code>null</code> if the identifier specifies
   *         a thread that doesn't exist or is not alive.
   * @throws IllegalArgumentException if an identifier in the array is
   *                                  <= 0.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   */
  ThreadInfo[] getThreadInfo(long[] ids);

  /**
   * Returns information on the specified threads with full
   * stack trace information and optional synchronization
   * information.  If <code>lockedMonitors</code> is false,
   * or there are no locked monitors for a particular thread,
   * then the corresponding {@link ThreadInfo} object will have
   * an empty {@link MonitorInfo} array.  Likewise, if
   * <code>lockedSynchronizers</code> is false, or there are
   * no locked ownable synchronizers for a particular thread,
   * then the corresponding {@link ThreadInfo} object will have
   * an empty {@link LockInfo} array.  If both
   * <code>lockedMonitors</code> and <code>lockedSynchronizers</code>
   * are false, the return value is equivalent to that from
   * <code>{@link #getThreadInfo}(ids, Integer.MAX_VALUE)</code>.
   * If an identifier specifies a thread which is either non-existant
   * or not alive, then the corresponding element in the returned
   * array is <code>null</code>.
   *
   * @param ids an array of thread identifiers to return information
   *           on.
   * @param lockedMonitors true if information on locked monitors
   *                       should be included.
   * @param lockedSynchronizers true if information on locked
   *                            ownable synchronizers should be included.
   * @return an array of {@link ThreadInfo} objects matching the
   *         specified threads.  The corresponding element is
   *         <code>null</code> if the identifier specifies
   *         a thread that doesn't exist or is not alive.
   * @throws IllegalArgumentException if an identifier in the array is
   *                                  <= 0.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   * @throws UnsupportedOperationException if <code>lockedMonitors</code>
   *                                       is true, but object monitor
   *                                       usage monitoring is not supported
   *                                       by the VM, or
   *                                       <code>lockedSynchronizers</code>
   *                                       is true, but ownable synchronizer
   *                                       usage monitoring is not supported
   *                                       by the VM.
   * @since 1.6
   * @see #isObjectMonitorUsageSupported()
   * @see #isSynchronizerUsageSupported()
   */
  ThreadInfo[] getThreadInfo(long[] ids, boolean lockedMonitors,
                             boolean lockedSynchronizers);

  /**
   * Returns information on the specified thread with
   * stack trace information to the supplied depth.  If the
   * identifier specifies a thread which is either non-existant
   * or not alive, then the method returns <code>null</code>.
   * A maximum depth of 0 corresponds to an empty stack trace
   * (an empty array is returned by the appropriate
   * {@link ThreadInfo} method).  A maximum depth of
   * <code>Integer.MAX_VALUE</code> returns the full stack trace.
   *
   * @param id the identifier of the thread to return information
   *           on.
   * @param maxDepth the maximum depth of the stack trace.
   *                 Values of 0 or <code>Integer.MAX_VALUE</code>
   *                 correspond to an empty and full stack trace
   *                 respectively.
   * @return a {@link ThreadInfo} object pertaining to the specified
   *         thread, or <code>null</code> if the identifier specifies
   *         a thread that doesn't exist or is not alive.
   * @throws IllegalArgumentException if <code>id</code> <= 0.
   * @throws IllegalArgumentException if <code>maxDepth</code> < 0.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   */
  ThreadInfo getThreadInfo(long id, int maxDepth);

  /**
   * Returns information on the specified threads with
   * stack trace information to the supplied depth.  If an
   * identifier specifies a thread which is either non-existant
   * or not alive, then the corresponding element in the returned
   * array is <code>null</code>.  A maximum depth of 0 corresponds
   * to an empty stack trace (an empty array is returned by the
   * appropriate {@link ThreadInfo} method).  A maximum depth of
   * <code>Integer.MAX_VALUE</code> returns the full stack trace.
   *
   * @param ids an array of thread identifiers to return information
   *           on.
   * @param maxDepth the maximum depth of the stack trace.
   *                 Values of 0 or <code>Integer.MAX_VALUE</code>
   *                 correspond to an empty and full stack trace
   *                 respectively.
   * @return an array of {@link ThreadInfo} objects matching the
   *         specified threads.  The corresponding element is
   *         <code>null</code> if the identifier specifies
   *         a thread that doesn't exist or is not alive.
   * @throws IllegalArgumentException if an identifier in the array is
   *                                  <= 0.
   * @throws IllegalArgumentException if <code>maxDepth</code> < 0.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   */
  ThreadInfo[] getThreadInfo(long[] ids, int maxDepth);

  /**
   * <p>
   * Returns the total number of nanoseconds of CPU time
   * the specified thread has executed in user mode.
   * </p>
   * <p>
   * Note that the value is only nanosecond-precise, and not accurate; there
   * is no guarantee that the difference between two values is really a
   * nanosecond.  Also, the value is prone to overflow if the offset
   * exceeds 2^63.  The use of this method depends on virtual machine
   * support for measurement of the CPU time of the current thread,
   * and on this functionality being enabled.
   * </p>
   *
   * @param id the thread identifier of the thread whose CPU time is being
   *           monitored.
   * @return the total number of nanoseconds of CPU time the specified
   *         thread has executed in user mode, or -1 if CPU time monitoring
   *         is disabled.
   * @throws IllegalArgumentException if <code>id</code> <= 0.
   * @throws UnsupportedOperationException if CPU time monitoring is not
   *                                       supported.
   * @see #getThreadCpuTime(long)
   * @see #isThreadCpuTimeSupported()
   * @see #isThreadCpuTimeEnabled()
   * @see #setThreadCpuTimeEnabled(boolean)
   */
  long getThreadUserTime(long id);

  /**
   * Returns the total number of threads that have been
   * created and started during the lifetime of the virtual
   * machine.
   *
   * @return the total number of started threads.
   */
  long getTotalStartedThreadCount();

  /**
   * Returns true if the virtual machine supports the monitoring
   * of the CPU time used by the current thread.  This is implied
   * by {@link isThreadCpuTimeSupported()} returning true.
   *
   * @return true if monitoring of the CPU time used by the current
   *         thread is supported by the virtual machine.
   * @see #isThreadCpuTimeEnabled()
   * @see #isThreadCpuTimeSupported()
   * @see #setThreadCpuTimeEnabled(boolean)
   */
  boolean isCurrentThreadCpuTimeSupported();

  /**
   * Returns true if the virtual machine supports the monitoring
   * of object monitor usage.
   *
   * @return true if the monitoring of object monitor usage
   *         is supported by the virtual machine.
   * @since 1.6
   */
  boolean isObjectMonitorUsageSupported();

  /**
   * Returns true if the virtual machine supports the monitoring
   * of ownable synchronizer usage.
   *
   * @return true if the monitoring of ownable synchronizer usage
   *         is supported by the virtual machine.
   * @since 1.6
   */
  boolean isSynchronizerUsageSupported();

  /**
   * Returns true if thread contention monitoring is currently
   * enabled.
   *
   * @return true if thread contention monitoring is enabled.
   * @throws UnsupportedOperationException if the virtual
   *                                       machine does not
   *                                       support contention
   *                                       monitoring.
   * @see #isThreadContentionMonitoringSupported()
   * @see #setThreadContentionMonitoringEnabled(boolean)
   */
  boolean isThreadContentionMonitoringEnabled();

  /**
   * Returns true if thread contention monitoring is supported
   * by the virtual machine.
   *
   * @return true if thread contention monitoring is supported
   *         by the virtual machine.
   * @see #isThreadContentionMonitoringEnabled()
   * @see #setThreadContentionMonitoringEnabled(boolean)
   */
  boolean isThreadContentionMonitoringSupported();

  /**
   * Returns true if monitoring of the CPU time used by a thread
   * is currently enabled.
   *
   * @return true if thread CPU time monitoring is enabled.
   * @throws UnsupportedOperationException if the virtual
   *                                       machine does not
   *                                       support CPU time
   *                                       monitoring.
   * @see #isCurrentThreadCpuTimeSupported()
   * @see #isThreadCpuTimeSupported()
   * @see #setThreadCpuTimeEnabled(boolean)
   */
  boolean isThreadCpuTimeEnabled();

  /**
   * Returns true if the virtual machine supports the monitoring
   * of the CPU time used by all threads.  This implies
   * that {@link isCurrentThreadCpuTimeSupported()} returns true.
   *
   * @return true if monitoring of the CPU time used by the current
   *         thread is supported by the virtual machine.
   * @see #isCurrentThreadCpuTimeSupported()
   * @see #isThreadCpuTimeEnabled()
   * @see #setThreadCpuTimeEnabled(boolean)
   */
  boolean isThreadCpuTimeSupported();

  /**
   * Resets the peak live thread count to the
   * current number of live threads, as returned
   * by {@link #getThreadCount()}.
   *
   * @see #getPeakThreadCount()
   * @see #getThreadCount()
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("control").
   */
  void resetPeakThreadCount();

  /**
   * Toggles the monitoring of thread contention.  Thread
   * contention monitoring is disabled by default.  Each
   * time contention monitoring is re-enabled, the times
   * it maintains are reset.
   *
   * @param enable true if monitoring should be enabled,
   *               false if it should be disabled.
   * @throws UnsupportedOperationException if the virtual
   *                                       machine does not
   *                                       support contention
   *                                       monitoring.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("control").
   * @see #isThreadContentionMonitoringEnabled()
   * @see #isThreadContentionMonitoringSupported()
   */
  void setThreadContentionMonitoringEnabled(boolean enable);

  /**
   * Toggles the monitoring of CPU time used by threads. The
   * initial setting is dependent on the underlying virtual
   * machine.  On enabling CPU time monitoring, the virtual
   * machine may take any value up to and including the current
   * time as the start time for monitoring.
   *
   * @param enable true if monitoring should be enabled,
   *               false if it should be disabled.
   * @throws UnsupportedOperationException if the virtual
   *                                       machine does not
   *                                       support CPU time
   *                                       monitoring.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("control").
   * @see #isCurrentThreadCpuTimeSupported()
   * @see #isThreadCpuTimeEnabled()
   * @see #isThreadCpuTimeSupported()
   */
  void setThreadCpuTimeEnabled(boolean enable);

}
