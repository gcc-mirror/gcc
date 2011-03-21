/* TestThread.java -- Tests the thread bean.
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
02110-1301 USA. */

package gnu.classpath.examples.management;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;

import java.util.Arrays;

public class TestThread
{

  public static void main(String[] args)
  {
    ThreadMXBean bean = ManagementFactory.getThreadMXBean();
    System.out.println("Bean: " + bean);
    System.out.println("Monitor deadlocked threads: " + bean.findMonitorDeadlockedThreads());
    long[] ids = bean.getAllThreadIds();
    System.out.println("Live thread ids: " + Arrays.toString(ids));
    boolean currentTimeMonitoring = bean.isCurrentThreadCpuTimeSupported();
    System.out.println("Current thread CPU time monitoring supported: " + currentTimeMonitoring);
    if (currentTimeMonitoring)
      {
        boolean timeEnabled = bean.isThreadCpuTimeEnabled();
        System.out.println("Is time monitoring enabled... " +
                           (timeEnabled ? "yes" : "no"));
        if (!timeEnabled)
          {
            System.out.println("Enabling...");
            bean.setThreadCpuTimeEnabled(true);
            timeEnabled = bean.isThreadCpuTimeEnabled();
            System.out.println("Should now be enabled... " +
                           (timeEnabled ? "yes" : "no"));
          }
        if (timeEnabled)
          {
            System.out.println("Current thread CPU time: "
                               + bean.getCurrentThreadCpuTime()
                               + "ns");
            System.out.println("Current thread user time: "
                               + bean.getCurrentThreadUserTime()
                               + "ns");
          }
      }
    System.out.println("Daemon thread count: " + bean.getDaemonThreadCount());
    System.out.println("Peak thread count: " + bean.getPeakThreadCount());
    System.out.println("Resetting...");
    bean.resetPeakThreadCount();
    System.out.println("Peak thread count: " + bean.getPeakThreadCount());
    System.out.println("Thread count: " + bean.getThreadCount());
    boolean timeMonitoring = bean.isThreadCpuTimeSupported();
    System.out.println("Thread CPU time monitoring supported: " + timeMonitoring);
    if (timeMonitoring)
      {
        for (int a = 0; a < ids.length; ++a)
          {
            System.out.println("Thread " + a
                               + " CPU time: "
                               + bean.getThreadCpuTime(ids[a]) + "ns");
            System.out.println("Thread "
                               + a + " user time: "
                               + bean.getThreadUserTime(ids[a]) + "ns");
          }
      }
    System.out.println("Current thread info: "
                       + bean.getThreadInfo(Thread.currentThread().getId()));
    System.out.println("All thread info: " + Arrays.toString(bean.getThreadInfo(ids)));
    System.out.println("Total started threads: " + bean.getTotalStartedThreadCount());
    boolean contentionMonitoring = bean.isThreadContentionMonitoringSupported();
    System.out.println("Thread contention monitoring supported: " + contentionMonitoring);
    if (contentionMonitoring)
      {
        boolean contentionEnabled = bean.isThreadContentionMonitoringEnabled();
        System.out.println("Thread contention monitoring shouldn't be enabled... " +
                           (contentionEnabled ? "but it is" : "true"));
        if (!contentionEnabled)
          {
            System.out.println("Enabling...");
            bean.setThreadContentionMonitoringEnabled(true);
            contentionEnabled = bean.isThreadContentionMonitoringEnabled();
            System.out.println("Should now be enabled... " +
                               (contentionEnabled ? "it is" : "nope"));
          }
        if (contentionEnabled)
          {
            ThreadInfo[] info = bean.getThreadInfo(ids);
            for (int a = 0; a < info.length; ++a)
              {
                System.out.println("Blocked time for thread "
                                   + info[a].getThreadId() + ": "
                                   + info[a].getBlockedTime() + "ms");
                System.out.println("Waited time for thread "
                                   + info[a].getThreadId() + ": "
                                   + info[a].getWaitedTime() + "ms");
              }
          }
      }
  }
}
