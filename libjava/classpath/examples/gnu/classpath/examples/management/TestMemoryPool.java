/* TestMemoryPool.java -- Tests the memory pool beans.
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
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;

import java.util.Arrays;
import java.util.Iterator;

public class TestMemoryPool
{

  /**
   * 1mb in bytes
   */
  private static final int MB = 1 << 20;

  public static void main(String[] args)
  {
    Iterator beans = ManagementFactory.getMemoryPoolMXBeans().iterator();
    while (beans.hasNext())
      {
	MemoryPoolMXBean bean = (MemoryPoolMXBean) beans.next();
	System.out.println("Bean: " + bean);
	System.out.println("Name: " + bean.getName());
	System.out.println("Collection usage: " + bean.getCollectionUsage());
	boolean collectionUsage = bean.isCollectionUsageThresholdSupported();
	System.out.println("Collection usage threshold supported: " 
			   + collectionUsage);
	if (collectionUsage)
	  {
	    System.out.println("Collection usage threshold: " 
			       + bean.getCollectionUsageThreshold());
	    System.out.println("Setting collection usage threshold to 1MB (" 
			       + MB + " bytes)");
	    bean.setCollectionUsageThreshold(MB);
	    System.out.println("Collection usage threshold: " 
			       + bean.getCollectionUsageThreshold());
	    System.out.println("Collection usage threshold count: " 
			       + bean.getCollectionUsageThresholdCount());
	    System.out.println("Collection usage threshold exceeded: " 
			       + (bean.isCollectionUsageThresholdExceeded()
				  ? "yes" : "no"));
	  }
	System.out.println("Memory manager names: " 
			   + Arrays.toString(bean.getMemoryManagerNames()));
	System.out.println("Peak usage: " + bean.getPeakUsage());
	System.out.println("Current usage: " + bean.getUsage());
	System.out.println("Resetting peak usage...");
	bean.resetPeakUsage();
	System.out.println("Peak usage: " + bean.getPeakUsage());
	System.out.println("Current usage: " + bean.getUsage());
	boolean usage = bean.isUsageThresholdSupported();
	System.out.println("Usage threshold supported: " + usage);
	if (usage)
	  {
	    System.out.println("Usage threshold: " 
			       + bean.getUsageThreshold());
	    System.out.println("Setting usage threshold to 1MB (" 
			       + MB + " bytes)");
	    bean.setUsageThreshold(MB);
	    System.out.println("Usage threshold: " 
			       + bean.getUsageThreshold());
	    System.out.println("Usage threshold count: " 
			       + bean.getUsageThresholdCount());
	    System.out.println("Usage threshold exceeded: " 
			       + (bean.isUsageThresholdExceeded()
				  ? "yes" : "no"));
	  }
	System.out.println("Valid: " + (bean.isValid() ? "yes" : "no"));
      }
  }
}
