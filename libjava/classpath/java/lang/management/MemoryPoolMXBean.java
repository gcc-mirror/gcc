/* MemoryPoolMXBean.java - Interface for a memory pool bean
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

package java.lang.management;

/**
 * <p>
 * Provides access to information about one of the memory
 * resources or pools used by the virtual machine.  Instances
 * of this bean are obtained by calling
 * {@link ManagementFactory#getMemoryPoolMXBeans()}.  One
 * bean is returned for each memory pool provided.
 * </p>
 * <p>
 * The memory pool bean allows the usage of the pool to be
 * monitored.  The bean can provide statistics on the current
 * and peak usage of the pool, and on the threshold levels the
 * pool uses.
 * </p>
 * <p>
 * {@link getUsage()} returns an approximation of the current
 * usage of the pool.  Calls to this method are expected to be
 * generally quick to perform; if the call is expensive, the
 * documentation of the bean should specify so.  For memory
 * pool beans that represent the memory used by garbage
 * collectors, the usage level includes both referenced and
 * unreferenced objects.
 * </p>
 * <p>
 * {@link getPeakUsage()} and {@link resetPeakUsage()} enable
 * the retrieval of the peak usage level and setting it to the
 * current usage level, respectively.  Initially, the peak usage
 * level is relative to the start of the virtual machine.
 * </p>
 * <p>
 * Memory pools may also include optional support for usage thresholds.
 * The usage threshold is a particular level of memory usage.  When this
 * value is crossed (the current memory usage becomes equal to or greater
 * than this threshold level), the usage threshold count is increased.
 * This feature is designed for monitoring the trend in memory usage.
 * Support for a collection usage threshold is also provided, for
 * particular garbage collectors.  This is used to monitor the amount
 * of memory left uncollected after a garbage collection cycle.  There
 * is no need to make special garbage collection runs to support this;
 * the level following collection just needs to be monitored.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface MemoryPoolMXBean
{

  /**
   * Returns memory usage statistics after a best-effort attempt
   * has been made to remove unused objects from the pool.  This
   * method is designed for use by the pools of garbage collectors,
   * in order to monitor the amount of memory used after collections.
   * It will return <code>null</code> if such functionality is
   * unsupported by the memory pool represented by this bean.
   *
   * @return the memory usage of the memory pool after the most
   *         recent garbage collection cycle, or <code>null</code>
   *         if this operation is not supported.
   */
  MemoryUsage getCollectionUsage();

  /**
   * Returns the collection usage threshold level in bytes.  This
   * value is initially zero.
   *
   * @return the collection usage threshold in bytes.
   * @throws UnsupportedOperationException if the collection usage
   *                                       threshold is not supported.
   * @see #getCollectionUsageThresholdCount()
   * @see #isCollectionUsageThresholdExceeded()
   * @see #isCollectionUsageThresholdSupported()
   * @see #setCollectionUsageThreshold(long)
   */
  long getCollectionUsageThreshold();

  /**
   * Returns the number of times the usage level has matched or
   * exceeded the collection usage threshold.
   *
   * @return the number of times the usage level has matched
   *         or exceeded the collection usage threshold.
   * @throws UnsupportedOperationException if the collection usage
   *                                       threshold is not supported.
   * @see #getCollectionUsageThreshold()
   * @see #isCollectionUsageThresholdExceeded()
   * @see #isCollectionUsageThresholdSupported()
   * @see #setCollectionUsageThreshold(long)
   */
  long getCollectionUsageThresholdCount();

  /**
   * Returns the names of the memory managers associated with this
   * pool.  Each pool has at least one memory manager.
   *
   * @return an array containing the name of each memory manager
   *         responsible for this pool.
   */
  String[] getMemoryManagerNames();

  /**
   * Returns the name of the memory pool.
   *
   * @return the memory pool name.
   */
  String getName();

  /**
   * Returns memory usage statistics for the peak memory usage
   * of the pool.  The peak is the maximum memory usage occurring
   * since the virtual machine was started or since the peak
   * was reset by {@link #resetPeakUsage()}.  The return value
   * may be <code>null</code> if this pool is no longer valid.
   *
   * @return the memory usage of the memory pool at its peak,
   *         or <code>null</code> if this pool is no longer valid.
   */
  MemoryUsage getPeakUsage();

  /**
   * Returns the type of memory used by this pool.  This can be
   * either heap or non-heap memory.
   *
   * @return the type of this pool.
   */
  MemoryType getType();

  /**
   * Returns memory usage statistics for the current memory usage
   * of the pool.  The return value may be <code>null</code> if
   * this pool is no longer valid.  Obtaining these values is
   * expected to be a relatively quick operation; if this will
   * instead be an expensive operation to perform, the documentation
   * of the implementating bean should specify that this is the
   * case.  The values are intended to be an estimate for monitoring
   * purposes.
   *
   * @return the memory usage of the memory pool at present,
   *         or <code>null</code> if this pool is no longer valid.
   */
  MemoryUsage getUsage();

  /**
   * Returns the usage threshold level in bytes.  This
   * value is initially defined by the virtual machine.
   *
   * @return the usage threshold in bytes.
   * @throws UnsupportedOperationException if the usage threshold
   *                                       is not supported.
   * @see #getUsageThresholdCount()
   * @see #isUsageThresholdExceeded()
   * @see #isUsageThresholdSupported()
   * @see #setUsageThreshold(long)
   */
  long getUsageThreshold();

  /**
   * Returns the number of times the usage level has matched or
   * exceeded the usage threshold.
   *
   * @return the number of times the usage level has matched
   *         or exceeded the usage threshold.
   * @throws UnsupportedOperationException if the usage threshold
   *                                       is not supported.
   * @see #getUsageThreshold()
   * @see #isUsageThresholdExceeded()
   * @see #isUsageThresholdSupported()
   * @see #setUsageThreshold(long)
   */
  long getUsageThresholdCount();

  /**
   * Returns true if the collection usage level is equal to
   * or greater than the collection usage threshold.
   *
   * @return true if the collection usage threshold has been
   *         matched or exceeded.
   * @throws UnsupportedOperationException if the collection usage
   *                                       threshold is not supported.
   * @see #getCollectionUsageThreshold()
   * @see #getCollectionUsageThresholdCount()
   * @see #isCollectionUsageThresholdSupported()
   * @see #setCollectionUsageThreshold(long)
   */
  boolean isCollectionUsageThresholdExceeded();

  /**
   * Returns true if this memory pool supports a collection usage
   * level threshold.
   *
   * @return true if a collection usage level threshold is supported.
   * @see #getCollectionUsageThreshold()
   * @see #getCollectionUsageThresholdCount()
   * @see #isCollectionUsageThresholdExceeded()
   * @see #setCollectionUsageThreshold(long)
   */
  boolean isCollectionUsageThresholdSupported();

  /**
   * Returns true if the usage level is equal to
   * or greater than the usage threshold.
   *
   * @return true if the usage threshold has been
   *         matched or exceeded.
   * @throws UnsupportedOperationException if the usage threshold
   *                                       is not supported.
   * @see #getUsageThreshold()
   * @see #getUsageThresholdCount()
   * @see #isUsageThresholdSupported()
   * @see #setUsageThreshold(long)
   */
  boolean isUsageThresholdExceeded();

  /**
   * Returns true if this memory pool supports a usage level threshold.
   *
   * @return true if a usage level threshold is supported.
   * @see #getUsageThreshold()
   * @see #getUsageThresholdCount()
   * @see #isUsageThresholdExceeded()
   * @see #setUsageThreshold(long)
   */
  boolean isUsageThresholdSupported();

  /**
   * Returns true if this memory pool is still valid.  A memory pool
   * becomes invalid when it is removed by the virtual machine and
   * no longer used.
   *
   * @return true if this memory pool is valid.
   */
  boolean isValid();

  /**
   * Resets the peak memory usage level to the current memory usage
   * level.
   *
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("control").
   */
  void resetPeakUsage();

  /**
   * Sets the collection threshold usage level to the given value.
   * A value of zero disables the collection threshold.
   *
   * @param threshold the new threshold level.
   * @throws IllegalArgumentException if the threshold hold level
   *                                  is negative.
   * @throws UnsupportedOperationException if the collection usage
   *                                       threshold is not supported.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("control").
   * @see #getCollectionUsageThreshold()
   * @see #getCollectionUsageThresholdCount()
   * @see #isCollectionUsageThresholdExceeded()
   * @see #isCollectionUsageThresholdSupported()
   */
  void setCollectionUsageThreshold(long threshold);

  /**
   * Sets the threshold usage level to the given value.  A value of
   * zero disables the threshold.
   *
   * @param threshold the new threshold level.
   * @throws IllegalArgumentException if the threshold hold level
   *                                  is negative.
   * @throws UnsupportedOperationException if the usage threshold
   *                                       is not supported.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("control").
   * @see #getUsageThreshold()
   * @see #getUsageThresholdCount()
   * @see #isUsageThresholdExceeded()
   * @see #isUsageThresholdSupported()
   */
  void setUsageThreshold(long threshold);

}
