/* VMThreadMXBeanImpl.java - VM impl. of a thread bean
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

package gnu.java.lang.management;

import java.lang.management.ThreadInfo;

/**
 * Provides access to information about the threads
 * of the virtual machine.  An instance of this bean is
 * obtained by calling
 * {@link ManagementFactory#getThreadMXBean()}.
 * See {@link java.lang.management.ThreadMXBean} for
 * full documentation.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
final class VMThreadMXBeanImpl
{

  /**
   * Cache of how many threads were found.
   */
  private static int filled;

  /**
   * Returns the ids of cycles of deadlocked threads, occurring
   * due to monitor ownership or ownable synchronizer ownership.
   * This will only be called if ownable synchronizer monitoring
   * is supported.
   *
   * @return the ids of the deadlocked threads.
   */
  static native long[] findDeadlockedThreads();

  /**
   * Returns the ids of cycles of deadlocked threads, occurring
   * due to monitor ownership.
   *
   * @return the ids of the deadlocked threads.
   */
  static native long[] findMonitorDeadlockedThreads();

  /* This is the same as in Thread.getAllStackTraces() */
  static Thread[] getAllThreads()
  {
    ThreadGroup group = Thread.currentThread().getThreadGroup();
    while (group.getParent() != null)
      group = group.getParent();
    int arraySize = group.activeCount();
    Thread[] threadList = new Thread[arraySize];
    filled = group.enumerate(threadList);
    while (filled == arraySize)
      {
        arraySize *= 2;
        threadList = new Thread[arraySize];
        filled = group.enumerate(threadList);
      }
    return threadList;
  }

  /**
   * Returns the id of all live threads at the time of execution.
   *
   * @return the live thread ids.
   */
  static long[] getAllThreadIds()
  {
    Thread[] threadList = getAllThreads();
    long[] ids = new long[filled];
    for (int a = 0; a < filled; ++a)
      ids[a] = threadList[a].getId();
    return ids;
  }

  /**
   * Returns the number of nanoseconds of CPU time
   * the current thread has used in total.   This is
   * only called if this feature is enabled and
   * supported.
   *
   * @return the nanoseconds of CPU time used by
   *         the current thread.
   */
  static native long getCurrentThreadCpuTime();

  /**
   * Returns the number of nanoseconds of user time
   * the current thread has used in total.   This is
   * only called if this feature is enabled and
   * supported.
   *
   * @return the nanoseconds of user time used by
   *         the current thread.
   */
  static native long getCurrentThreadUserTime();

  /**
   * Returns the number of live daemon threads.
   *
   * @return the number of live daemon threads.
   */
  static int getDaemonThreadCount()
  {
    Thread[] threadList = getAllThreads();
    int daemonCount = 0;
    for (int a = 0; a < filled; ++a)
      {
        if (threadList[a].isDaemon())
          ++daemonCount;
      }
    return daemonCount;
  }

  /**
   * Fill out the given {@link ThreadInfo} object
   * with ownable synchronizer usage information.
   * This is only called if ownable synchronizer
   * usage monitoring is supported.
   *
   * @param info the {@link ThreadInfo} object to modify.
   */
  static native void getLockInfo(ThreadInfo info);

  /**
   * Fill out the given {@link ThreadInfo} object
   * with monitor usage information.  This is only
   * called if monitor usage monitoring is supported.
   *
   * @param info the {@link ThreadInfo} object to modify.
   */
  static native void getMonitorInfo(ThreadInfo info);

  /**
   * Returns the current peak number of live threads.
   *
   * @return the peak number of live threads.
   */
  static native int getPeakThreadCount();

  /**
   * Returns the number of live threads.
   *
   * @return the number of live threads.
   */
  static int getThreadCount()
  {
    getAllThreads();
    return filled;
  }

  /**
   * Returns the number of nanoseconds of CPU time
   * the specified thread has used in total.   This is
   * only called if this feature is enabled and
   * supported.
   *
   * @param id the thread to obtain statistics on.
   * @return the nanoseconds of CPU time used by
   *         the thread.
   */
  static native long getThreadCpuTime(long id);

  /**
   * Returns the {@link java.lang.management.ThreadInfo}
   * which corresponds to the specified id.
   *
   * @param id the id of the thread.
   * @param maxDepth the depth of the stack trace.
   * @return the corresponding <code>ThreadInfo</code>.
   */
  static native ThreadInfo getThreadInfoForId(long id, int maxDepth);

  /**
   * Returns the number of nanoseconds of user time
   * the specified thread has used in total.   This is
   * only called if this feature is enabled and
   * supported.
   *
   * @param id the thread to obtain statistics on.
   * @return the nanoseconds of user time used by
   *         the thread.
   */
  static native long getThreadUserTime(long id);

  /**
   * Returns the total number of threads that have
   * been started over the lifetime of the virtual
   * machine.
   *
   * @return the total number of threads started.
   */
  static native long getTotalStartedThreadCount();

  /**
   * Resets the peak thread count to the current
   * number of live threads.
   */
  static native void resetPeakThreadCount();

}
