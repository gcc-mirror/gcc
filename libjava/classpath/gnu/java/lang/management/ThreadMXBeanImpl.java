/* ThreadMXBeanImpl.java - Implementation of a thread bean
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

import gnu.classpath.SystemProperties;

import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;

import javax.management.NotCompliantMBeanException;

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
public final class ThreadMXBeanImpl
  extends BeanImpl
  implements ThreadMXBean
{

  /**
   * Constant for current thread time support.
   */
  private static final String CURRENT_THREAD_TIME_SUPPORT =
    "gnu.java.lang.management.CurrentThreadTimeSupport";

  /**
   * Constant for thread time support.
   */
  private static final String THREAD_TIME_SUPPORT =
    "gnu.java.lang.management.ThreadTimeSupport";

  /**
   * Constant for thread contention support.
   */
  private static final String CONTENTION_SUPPORT =
    "gnu.java.lang.management.ThreadContentionSupport";

  /**
   * Constant for initial value of thread time support.
   */
  private static final String TIME_ENABLED =
    "gnu.java.lang.management.ThreadTimeInitallyEnabled";

  /**
   * Constant for monitor usage monitoring support.
   */
  private static final String MONITOR_SUPPORT =
    "gnu.java.lang.management.MonitorUsageMonitoringSupport";

  /**
   * Constant for ownable synchronizer usage monitoring support.
   */
  private static final String SYNCHRONIZER_SUPPORT =
    "gnu.java.lang.management.OwnableSynchronizerUsageMonitoringSupport";

  /**
   * Flag to indicate whether time monitoring is enabled or not.
   */
  private boolean timeEnabled;

  /**
   * Flag to indicate whether contention monitoring is enabled or not.
   */
  private boolean contentionEnabled;

  /**
   * Default constructor to set up flag states.  The
   * VM has to specify whether time monitoring is initially
   * enabled or not.
   *
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  public ThreadMXBeanImpl()
    throws NotCompliantMBeanException
  {
    super(ThreadMXBean.class);
    timeEnabled = Boolean.parseBoolean(SystemProperties.getProperty(TIME_ENABLED));
    contentionEnabled = false;
  }

  public ThreadInfo[] dumpAllThreads(boolean lockedMonitors,
                                     boolean lockedSynchronizers)
  {
    return getThreadInfo(getAllThreadIds(), lockedMonitors,
                         lockedSynchronizers);
  }

  public long[] findDeadlockedThreads()
  {
    checkMonitorPermissions();
    if (!isSynchronizerUsageSupported())
      throw new UnsupportedOperationException("Ownable synchronizer usage " +
                                              "monitoring is not provided " +
                                              "by this VM.");
    return VMThreadMXBeanImpl.findDeadlockedThreads();
  }

  public long[] findMonitorDeadlockedThreads()
  {
    checkMonitorPermissions();
    return VMThreadMXBeanImpl.findMonitorDeadlockedThreads();
  }

  public long[] getAllThreadIds()
  {
    checkMonitorPermissions();
    return VMThreadMXBeanImpl.getAllThreadIds();
  }

  public long getCurrentThreadCpuTime()
  {
    if (!isCurrentThreadCpuTimeSupported())
      throw new UnsupportedOperationException("Current thread CPU " +
                                              "time not supported.");
    if (!timeEnabled)
      return -1;
    return VMThreadMXBeanImpl.getCurrentThreadCpuTime();
  }

  public long getCurrentThreadUserTime()
  {
    if (!isCurrentThreadCpuTimeSupported())
      throw new UnsupportedOperationException("Current thread user " +
                                              "time not supported.");
    if (!timeEnabled)
      return -1;
    return VMThreadMXBeanImpl.getCurrentThreadUserTime();
  }

  public int getDaemonThreadCount()
  {
    return VMThreadMXBeanImpl.getDaemonThreadCount();
  }

  public int getPeakThreadCount()
  {
    return VMThreadMXBeanImpl.getPeakThreadCount();
  }

  public int getThreadCount()
  {
    return VMThreadMXBeanImpl.getThreadCount();
  }

  public long getThreadCpuTime(long id)
  {
    if (!isThreadCpuTimeSupported())
      throw new UnsupportedOperationException("Thread CPU time not " +
                                              "supported.");
    if (id <= 0)
      throw new IllegalArgumentException("Invalid thread id: " + id);
    if (!timeEnabled)
      return -1;
    return VMThreadMXBeanImpl.getThreadCpuTime(id);
  }

  public ThreadInfo getThreadInfo(long id)
  {
    return getThreadInfo(id, 0);
  }

  public ThreadInfo[] getThreadInfo(long[] ids)
  {
    return getThreadInfo(ids, 0);
  }

  public ThreadInfo getThreadInfo(long id, int maxDepth)
  {
    checkMonitorPermissions();
    if (id <= 0)
      throw new IllegalArgumentException("Invalid thread id: " + id);
    if (maxDepth < 0)
      throw new IllegalArgumentException("Invalid depth: " + maxDepth);
    return VMThreadMXBeanImpl.getThreadInfoForId(id, maxDepth);
  }

  public ThreadInfo[] getThreadInfo(long[] ids, int maxDepth)
  {
    checkMonitorPermissions();
    if (maxDepth < 0)
      throw new IllegalArgumentException("Invalid depth: " + maxDepth);
    ThreadInfo[] infos = new ThreadInfo[ids.length];
    for (int a = 0; a < ids.length; ++a)
      {
        if (ids[a] <= 0)
          throw new IllegalArgumentException("Invalid thread id " + a +
                                             ": " + ids[a]);
        infos[a] = VMThreadMXBeanImpl.getThreadInfoForId(ids[a], maxDepth);
      }
    return infos;
  }

  public ThreadInfo[] getThreadInfo(long[] ids, boolean lockedMonitors,
                                    boolean lockedSynchronizers)
  {
    checkMonitorPermissions();
    if (lockedMonitors && !isObjectMonitorUsageSupported())
      throw new UnsupportedOperationException("Monitor usage monitoring is " +
                                              "not provided by this VM.");
    if (lockedSynchronizers && !isSynchronizerUsageSupported())
      throw new UnsupportedOperationException("Ownable synchronizer usage " +
                                              "monitoring is not provided " +
                                              "by this VM.");
    ThreadInfo[] infos = getThreadInfo(ids, Integer.MAX_VALUE);
    if (lockedMonitors)
      for (ThreadInfo info : infos)
        VMThreadMXBeanImpl.getMonitorInfo(info);
    if (lockedSynchronizers)
      for (ThreadInfo info : infos)
        VMThreadMXBeanImpl.getLockInfo(info);
    return infos;
  }

  public long getThreadUserTime(long id)
  {
    if (!isThreadCpuTimeSupported())
      throw new UnsupportedOperationException("Thread user time not " +
                                              "supported.");
    if (id <= 0)
      throw new IllegalArgumentException("Invalid thread id: " + id);
    if (!timeEnabled)
      return -1;
    return VMThreadMXBeanImpl.getThreadUserTime(id);
  }

  public long getTotalStartedThreadCount()
  {
    return VMThreadMXBeanImpl.getTotalStartedThreadCount();
  }

  public boolean isCurrentThreadCpuTimeSupported()
  {
    if (isThreadCpuTimeSupported())
      return true;
    return SystemProperties.getProperty(CURRENT_THREAD_TIME_SUPPORT) != null;
  }

  public boolean isObjectMonitorUsageSupported()
  {
    return SystemProperties.getProperty(MONITOR_SUPPORT) != null;
  }

  public boolean isSynchronizerUsageSupported()
  {
    return SystemProperties.getProperty(SYNCHRONIZER_SUPPORT) != null;
  }

  public boolean isThreadContentionMonitoringEnabled()
  {
    if (isThreadContentionMonitoringSupported())
      return contentionEnabled;
    else
      throw new UnsupportedOperationException("Contention monitoring " +
                                              "not supported.");
  }

  public boolean isThreadContentionMonitoringSupported()
  {
    return SystemProperties.getProperty(CONTENTION_SUPPORT) != null;
  }

  public boolean isThreadCpuTimeEnabled()
  {
    if (isThreadCpuTimeSupported() ||
        isCurrentThreadCpuTimeSupported())
      return timeEnabled;
    else
      throw new UnsupportedOperationException("Thread time not " +
                                              "supported.");
  }

  public boolean isThreadCpuTimeSupported()
  {
    return SystemProperties.getProperty(THREAD_TIME_SUPPORT) != null;
  }

  public void resetPeakThreadCount()
  {
    checkControlPermissions();
    VMThreadMXBeanImpl.resetPeakThreadCount();
  }

  public void setThreadContentionMonitoringEnabled(boolean enable)
  {
    checkControlPermissions();
    if (isThreadContentionMonitoringSupported())
      contentionEnabled = enable;
    else
      throw new UnsupportedOperationException("Contention monitoring " +
                                              "not supported.");
  }

  public void setThreadCpuTimeEnabled(boolean enable)
  {
    checkControlPermissions();
    if (isThreadCpuTimeSupported() ||
        isCurrentThreadCpuTimeSupported())
      timeEnabled = enable;
    else
      throw new UnsupportedOperationException("Thread time not " +
                                              "supported.");
  }

}
