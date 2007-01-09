/* MemoryPoolMXBeanImpl.java - Implementation of a memory pool bean
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

import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryType;
import java.lang.management.MemoryUsage;

import javax.management.NotCompliantMBeanException;

/**
 * Provides access to information about one of the memory 
 * resources or pools used by the current invocation of the
 * virtual machine.  An instance of this bean for each memory
 * pool is obtained by calling
 * {@link ManagementFactory#getMemoryPoolMXBeans()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public final class MemoryPoolMXBeanImpl
  extends BeanImpl
  implements MemoryPoolMXBean
{

  /**
   * The name of the pool.
   */
  private String name;

  /**
   * Constant for collection usage threshold.
   */
  private static final String COLLECTION_USAGE_THRESHOLD = 
    "gnu.java.lang.management.CollectionUsageThresholdSupport";

  /**
   * Constant for thread time support.
   */
  private static final String USAGE_THRESHOLD = 
    "gnu.java.lang.management.UsageThresholdSupport";

  /**
   * Constructs a new <code>MemoryPoolMXBeanImpl</code>.
   *
   * @param name the name of the pool this bean represents.
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  public MemoryPoolMXBeanImpl(String name)
    throws NotCompliantMBeanException
  {
    super(MemoryPoolMXBean.class);
    this.name = name;
  }

  public MemoryUsage getCollectionUsage()
  {
    return VMMemoryPoolMXBeanImpl.getCollectionUsage(name);
  }

  public long getCollectionUsageThreshold()
  {
    if (isCollectionUsageThresholdSupported())
      return VMMemoryPoolMXBeanImpl.getCollectionUsageThreshold(name);
    else
      throw new UnsupportedOperationException("A collection usage "+
					      "threshold is not supported.");
  }

  public long getCollectionUsageThresholdCount()
  {
    if (isCollectionUsageThresholdSupported())
      return VMMemoryPoolMXBeanImpl.getCollectionUsageThresholdCount(name);
    else
      throw new UnsupportedOperationException("A collection usage "+
					      "threshold is not supported.");
  }

  public String[] getMemoryManagerNames()
  {
    return VMMemoryPoolMXBeanImpl.getMemoryManagerNames(name);
  }

  public String getName()
  {
    return name;
  }

  public MemoryUsage getPeakUsage()
  {
    if (isValid())
      return VMMemoryPoolMXBeanImpl.getPeakUsage(name);
    else
      return null;
  }

  public MemoryType getType()
  {
    return 
      MemoryType.valueOf(VMMemoryPoolMXBeanImpl.getType(name));
  }

  public MemoryUsage getUsage()
  {
    if (isValid())
      return VMMemoryPoolMXBeanImpl.getUsage(name);
    else
      return null;
  }

  public long getUsageThreshold()
  {
    if (isUsageThresholdSupported())
      return VMMemoryPoolMXBeanImpl.getUsageThreshold(name);
    else
      throw new UnsupportedOperationException("A usage threshold " +
					      "is not supported.");
  }

  public long getUsageThresholdCount()
  {
    if (isUsageThresholdSupported())
      return VMMemoryPoolMXBeanImpl.getUsageThresholdCount(name);
    else
      throw new UnsupportedOperationException("A usage threshold " +
					      "is not supported.");
  }

  public boolean isCollectionUsageThresholdExceeded()
  {
    return getCollectionUsage().getUsed() >= getCollectionUsageThreshold();
  }

  public boolean isCollectionUsageThresholdSupported()
  {
    return SystemProperties.getProperty(COLLECTION_USAGE_THRESHOLD) != null;
  }

  public boolean isUsageThresholdExceeded()
  {
    return getUsage().getUsed() >= getUsageThreshold();
  }

  public boolean isUsageThresholdSupported()
  {
    return SystemProperties.getProperty(USAGE_THRESHOLD) != null;
  }

  public boolean isValid()
  {
    return VMMemoryPoolMXBeanImpl.isValid(name);
  }

  public void resetPeakUsage()
  {
    checkControlPermissions();
    VMMemoryPoolMXBeanImpl.resetPeakUsage(name);
  }

  public void setCollectionUsageThreshold(long threshold)
  {
    checkControlPermissions();
    if (threshold < 0)
      throw new IllegalArgumentException("Threshold of " + threshold +
					 "is less than zero.");
    if (isCollectionUsageThresholdSupported())
      VMMemoryPoolMXBeanImpl.setCollectionUsageThreshold(name, threshold);
    else
      throw new UnsupportedOperationException("A collection usage "+
					      "threshold is not supported.");
  }

  public void setUsageThreshold(long threshold)
  {
    checkControlPermissions();
    if (threshold < 0)
      throw new IllegalArgumentException("Threshold of " + threshold +
					 "is less than zero.");
    if (isUsageThresholdSupported())
      VMMemoryPoolMXBeanImpl.setUsageThreshold(name, threshold);
    else
      throw new UnsupportedOperationException("A usage threshold " +
					      "is not supported.");
  }

}

