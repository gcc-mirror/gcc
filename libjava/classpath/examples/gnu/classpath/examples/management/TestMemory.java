/* TestMemory.java -- Tests the memory bean.
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

import java.lang.management.MemoryMXBean;
import java.lang.management.ManagementFactory;

public class TestMemory
{
  public static void main(String[] args)
  {
    MemoryMXBean bean = ManagementFactory.getMemoryMXBean();
    System.out.println("Bean: " + bean);
    System.out.println("Heap memory usage: "
                       + bean.getHeapMemoryUsage());
    System.out.println("Non-heap memory usage: "
                       + bean.getNonHeapMemoryUsage());
    System.out.println("Objects pending finalization: "
                       + bean.getObjectPendingFinalizationCount());
    System.out.println("Running garbage collector via bean...");
    bean.gc();
    System.out.println("Heap memory usage: "
                       + bean.getHeapMemoryUsage());
    System.out.println("Non-heap memory usage: "
                       + bean.getNonHeapMemoryUsage());
    System.out.println("Objects pending finalization: "
                       + bean.getObjectPendingFinalizationCount());
    boolean verbosity = bean.isVerbose();
    System.out.println("Verbose memory output: " + (verbosity ? "yes" : "no"));
    System.out.println("Changing verbose setting...");
    bean.setVerbose(!verbosity);
    System.out.println("Verbose memory output: " + (bean.isVerbose() ? "yes" : "no"));
  }
}
