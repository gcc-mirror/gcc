/* TestRuntime.java -- Tests the runtime bean.
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
import java.lang.management.RuntimeMXBean;

import java.util.Date;

public class TestRuntime
{

  public static void main(String[] args)
  {
    RuntimeMXBean vmBean = ManagementFactory.getRuntimeMXBean();
    System.out.println("Bean: " + vmBean);
    boolean bootClassPath = vmBean.isBootClassPathSupported();
    System.out.println("Boot Class Path Supported: " + bootClassPath);
    if (bootClassPath)
      System.out.println("Boot Class Path: " + vmBean.getBootClassPath());
    System.out.println("Class Path: " + vmBean.getClassPath());
    System.out.println("Input Arguments: " + vmBean.getInputArguments());
    System.out.println("Library Path: " + vmBean.getLibraryPath());
    System.out.println("Management Spec. Version: " + vmBean.getManagementSpecVersion());
    System.out.println("Name: " + vmBean.getName());
    System.out.println("Spec Name: " + vmBean.getSpecName());
    System.out.println("Spec Vendor: " + vmBean.getSpecVendor());
    System.out.println("Spec Version: " + vmBean.getSpecVersion());
    System.out.println("Start Time: " + new Date(vmBean.getStartTime()));
    System.out.println("System Properties: " + vmBean.getSystemProperties());
    System.out.println("Uptime: " + vmBean.getUptime() + "ms");
    System.out.println("VM Name: " + vmBean.getVmName());
    System.out.println("VM Vendor: " + vmBean.getVmVendor());
    System.out.println("VM Version: " + vmBean.getVmVersion());
  }
}
