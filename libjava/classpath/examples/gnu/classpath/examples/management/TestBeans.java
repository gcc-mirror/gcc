/* TestBeans.java -- Tests the dynamic interface of the beans.
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.management.DynamicMBean;

public class TestBeans
{
  public static void main(String[] args)
    throws Exception
  {
    List beans = new ArrayList();
    /* FIXME: When there's a server, this will be easier... :) */
    beans.add(ManagementFactory.getOperatingSystemMXBean());
    beans.add(ManagementFactory.getRuntimeMXBean());
    beans.add(ManagementFactory.getThreadMXBean());
    beans.add(ManagementFactory.getCompilationMXBean());
    beans.add(ManagementFactory.getClassLoadingMXBean());
    beans.add(ManagementFactory.getMemoryMXBean());
    beans.addAll(ManagementFactory.getMemoryPoolMXBeans());
    beans.addAll(ManagementFactory.getMemoryManagerMXBeans());
    beans.addAll(ManagementFactory.getGarbageCollectorMXBeans());
    Iterator it = beans.iterator();
    while (it.hasNext())
      {
	DynamicMBean bean = (DynamicMBean) it.next();
	if (bean != null)
	  System.out.println(bean.getMBeanInfo());
      }
  }
}
