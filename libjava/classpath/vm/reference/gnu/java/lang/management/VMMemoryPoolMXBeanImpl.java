/* MemoryPoolMXBeanImpl.java - VM interface for memory pool beans
   Copyright (C) 2006, 2010  Free Software Foundation, Inc.

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

import java.lang.management.MemoryUsage;

/**
 * Provides access to information on the memory resources or
 * pools used by the current invocation of the virtual machine.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
final class VMMemoryPoolMXBeanImpl
{

  private VMMemoryPoolMXBeanImpl() {} // Prohibits instantiation.

  /**
   * Returns memory usage statistics for the specified pool
   * just after a best-effort attempt to free memory.  This
   * is valid only for certain garbage collectors.
   *
   * @param name the name of the pool to obtain statistics on.
   * @return a {@link java.lang.management.MemoryUsage} object
   *         containing the statistics or <code>null</code>
   *         if this pool does not support such statistics.
   */
  static native MemoryUsage getCollectionUsage(String name);

  /**
   * Returns the collection usage threshold for the specified pool.
   * This is only called if this functionality is supported
   * by the virtual machine (i.e. the appropriate property,
   * <code>gnu.java.lang.management.CollectionUsageThresholdSupport</code>,
   * is defined).  The value is initially zero.
   *
   * @param name the name of the pool to obtain statistics on.
   * @return the collection usage threshold.
   */
  static native long getCollectionUsageThreshold(String name);

  /**
   * Returns the number of times the collection usage threshold
   * has been met or exceeded by the specified pool.
   * This is only called if this functionality is supported
   * by the virtual machine (i.e. the appropriate property,
   * <code>gnu.java.lang.management.CollectionUsageThresholdSupport</code>,
   * is defined).
   *
   * @param name the name of the pool to obtain statistics on.
   * @return the collection usage threshold count.
   */
  static native long getCollectionUsageThresholdCount(String name);

  /**
   * Returns an array of names of memory managers which manage
   * the specified pool.
   *
   * @param name the name of the pool to obtain statistics on.
   * @return a list of memory managers for the pool.
   */
  static native String[] getMemoryManagerNames(String name);

  /**
   * Returns the peak usage level of the specified pool.
   * This is only called if the pool is valid.
   *
   * @param name the name of the pool to obtain statistics on.
   * @return a {@link java.lang.management.MemoryUsage} object
   *         containing the statistics.
   */
  static native MemoryUsage getPeakUsage(String name);

  /**
   * Returns the type of memory used by the specified pool.
   * The value must be either "HEAP" or "NON_HEAP".
   *
   * @param name the name of the pool to obtain statistics on.
   * @return the type of the given pool.
   */
  static native String getType(String name);

  /**
   * Returns the current usage level of the specified pool.
   * This is only called if the pool is valid.
   *
   * @param name the name of the pool to obtain statistics on.
   * @return a {@link java.lang.management.MemoryUsage} object
   *         containing the statistics.
   */
  static native MemoryUsage getUsage(String name);

  /**
   * Returns the usage threshold for the specified pool.
   * This is only called if this functionality is supported
   * by the virtual machine (i.e. the appropriate property,
   * <code>gnu.java.lang.management.UsageThresholdSupport</code>,
   * is defined).  The value is initially defined by the
   * virtual machine.
   *
   * @param name the name of the pool to obtain statistics on.
   * @return the usage threshold.
   */
  static native long getUsageThreshold(String name);

  /**
   * Returns the number of times the usage threshold
   * has been met or exceeded by the specified pool.
   * This is only called if this functionality is supported
   * by the virtual machine (i.e. the appropriate property,
   * <code>gnu.java.lang.management.UsageThresholdSupport</code>,
   * is defined).
   *
   * @param name the name of the pool to obtain statistics on.
   * @return the usage threshold count.
   */
  static native long getUsageThresholdCount(String name);

  /**
   * Returns true if the specified pool is still valid i.e.
   * it is still in use by the virtual machine.
   *
   * @param name the name of the pool to check the validity of.
   * @return true if the pool is valid.
   */
  static native boolean isValid(String name);

  /**
   * Resets the peak usage level to the current usage level for
   * the specified pool.
   *
   * @param name the name of the pool to reset the peak usage of.
   */
  static native void resetPeakUsage(String name);

  /**
   * Sets the collection usage threshold for the specified
   * pool to the supplied value.
   * This is only called if this functionality is supported
   * by the virtual machine (i.e. the appropriate property,
   * <code>gnu.java.lang.management.CollectionUsageThresholdSupport</code>,
   * is defined).
   *
   * @param name the name of the pool to set the threshold of.
   * @param threshold the new threshold level.
   */
  static native void setCollectionUsageThreshold(String name, long threshold);

  /**
   * Sets the usage threshold for the specified pool to the supplied value.
   * This is only called if this functionality is supported
   * by the virtual machine (i.e. the appropriate property,
   * <code>gnu.java.lang.management.UsageThresholdSupport</code>,
   * is defined).
   *
   * @param name the name of the pool to set the threshold of.
   * @param threshold the new threshold level.
   */
  static native void setUsageThreshold(String name, long threshold);

}
