/* TestCompilation.java -- Tests the compilation bean.
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
import java.lang.management.CompilationMXBean;

public class TestCompilation
{

  public static void main(String[] args)
  {
    CompilationMXBean bean = ManagementFactory.getCompilationMXBean();
    if (bean == null)
      {
        System.out.println("The compilation bean is not supported by this VM.");
        System.exit(-1);
      }
    System.out.println("Bean: " + bean);
    System.out.println("JIT compiler name: " + bean.getName());
    boolean timeMonitoring = bean.isCompilationTimeMonitoringSupported();
    System.out.println("Compilation time monitoring supported: " + timeMonitoring);
    if (timeMonitoring)
      {
        System.out.println("Compilation time: "
                           + bean.getTotalCompilationTime() + "ms");
      }
  }

}
