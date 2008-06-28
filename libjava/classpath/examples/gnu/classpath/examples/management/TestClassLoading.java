/* TestClassLoading.java -- Tests the class loading bean.
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

import java.lang.management.ClassLoadingMXBean;
import java.lang.management.ManagementFactory;

import static java.lang.management.ManagementFactory.CLASS_LOADING_MXBEAN_NAME;

import javax.management.Attribute;
import javax.management.MBeanServer;
import javax.management.ObjectName;

public class TestClassLoading
{
  public static void main(String[] args)
    throws Exception
  {
    System.out.println("Testing locally...");
    ClassLoadingMXBean bean = ManagementFactory.getClassLoadingMXBean();
    System.out.println("Bean: " + bean);
    System.out.println("Loaded classes: " + bean.getLoadedClassCount());
    System.out.println("Unloaded classes: " + bean.getUnloadedClassCount());
    System.out.println("Total loaded classes: " + bean.getTotalLoadedClassCount());
    boolean verbosity = bean.isVerbose();
    System.out.println("Verbose class output: " + (verbosity ? "yes" : "no"));
    System.out.println("Changing verbose setting...");
    bean.setVerbose(!verbosity);
    System.out.println("Verbose class output: " + (bean.isVerbose() ? "yes" : "no"));
    System.out.println("Testing via the server...");
    MBeanServer server = ManagementFactory.getPlatformMBeanServer();
    ObjectName classBean = new ObjectName(CLASS_LOADING_MXBEAN_NAME);
    System.out.println("Bean: " + classBean);
    System.out.println("Loaded classes: " + server.getAttribute(classBean, "LoadedClassCount"));
    System.out.println("Unloaded classes: " + server.getAttribute(classBean,
								  "UnloadedClassCount"));
    System.out.println("Total loaded classes: " + server.getAttribute(classBean,
								      "TotalLoadedClassCount"));
    verbosity = (Boolean) server.getAttribute(classBean, "Verbose");
    System.out.println("Verbose class output: " + (verbosity ? "yes" : "no"));
    System.out.println("Changing verbose setting...");
    server.setAttribute(classBean, new Attribute("Verbose", !verbosity));
    System.out.println("Verbose class output: " + ((Boolean)
						   server.getAttribute(classBean, "Verbose") ?
						   "yes" : "no"));
    System.out.println("Testing via the proxy...");
    bean = ManagementFactory.newPlatformMXBeanProxy(server, CLASS_LOADING_MXBEAN_NAME,
						    ClassLoadingMXBean.class);
    System.out.println("Bean: " + bean);
    System.out.println("Loaded classes: " + bean.getLoadedClassCount());
    System.out.println("Unloaded classes: " + bean.getUnloadedClassCount());
    System.out.println("Total loaded classes: " + bean.getTotalLoadedClassCount());
    verbosity = bean.isVerbose();
    System.out.println("Verbose class output: " + (verbosity ? "yes" : "no"));
    System.out.println("Changing verbose setting...");
    bean.setVerbose(!verbosity);
    System.out.println("Verbose class output: " + (bean.isVerbose() ? "yes" : "no"));
  }
}
