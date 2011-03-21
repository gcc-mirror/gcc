/* MemoryUsage.java - Information on the usage of a memory pool.
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

import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.SimpleType;
/**
 * <p>
 * Retains information on the usage of a particular memory
 * pool, or heap/non-heap memory as a whole.  Memory usage
 * is represented by four values (all in bytes):
 * </p>
 * <ul>
 * <li><strong>Initial Level</strong>: This is the initial
 * amount of memory allocated for the pool by the operating
 * system.  This value may be undefined.</li>
 * <li><strong>Used Level</strong>: This is the amount of
 * memory currently in use.</li>
 * <li><strong>Committed Level</strong>: This is the current
 * amount of memory allocated for the pool by the operating
 * system.  This value will always be equal to or greater than
 * the current amount of memory in use.  It may drop below
 * the initial amount, if the virtual machine judges this to
 * be practical.</li>
 * <li><strong>Maximum Level</strong>: This is the maximum
 * amount of memory that may be allocated for the pool by
 * the operating system.  Like the initial amount, it may
 * be undefined.  If it is defined, it will be greater than
 * or equal to the used and committed amounts and may change
 * over time.  It is not guaranteed that the maximum amount
 * of memory may actually be allocated to the pool.  For
 * example, a request for an amount of memory greater than
 * the current committed level, but less than the maximum,
 * may still fail due to resources at the operating system
 * level not being sufficient to fulfill the demand.</li>
 * </ul>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 * @see MemoryMXBean
 * @see MemoryPoolMXBean
 */
public class MemoryUsage
{

  /**
   * The initial amount of memory allocated.
   */
  private long init;

  /**
   * The amount of memory used.
   */
  private long used;

  /**
   * The amount of memory committed for use.
   */
  private long committed;

  /**
   * The maximum amount of memory available.
   */
  private long maximum;

  /**
   * Constructs a new {@link MemoryUsage} object with
   * the specified allocation levels.
   *
   * @param init the initial amount of memory allocated,
   *             or -1 if this value is undefined.  Must
   *             be >= -1.
   * @param used the amount of memory used.  Must be >= 0,
   *             and <= committed.
   * @param committed the amount of memory committed for use
   *                  at present.  Must be >= 0 and <=
   *                  maximum (if defined).
   * @param maximum the maximum amount of memory that may be
   *                used, or -1 if this value is undefined.
   *                Must be >= -1.
   * @throws IllegalArgumentException if the values break any
   *                                  of the limits specified
   *                                  above.
   */
  public MemoryUsage(long init, long used, long committed,
                     long maximum)
  {
    if (init < -1)
      throw new IllegalArgumentException("Initial value of "
                                         + init + " is too small.");
    if (used < 0)
      throw new IllegalArgumentException("Used value of "
                                         + used + " is too small.");
    if (committed < 0)
      throw new IllegalArgumentException("Committed value of "
                                         + committed + " is too small.");
    if (committed < used)
      throw new IllegalArgumentException("Committed value of "
                                         + committed + " is below "
                                         + used + ", the amount used.");
    if (maximum < -1)
      throw new IllegalArgumentException("Maximum value of "
                                         + maximum + " is too small.");
    if (maximum != -1 && maximum < committed)
      throw new IllegalArgumentException("Maximum value of "
                                         + maximum + " is below "
                                         + committed + ", the amount "
                                         + "committed.");
    this.init = init;
    this.used = used;
    this.committed = committed;
    this.maximum = maximum;
  }

  /**
   * <p>
   * Returns a {@link MemoryUsage} instance using the values
   * given in the supplied
   * {@link javax.management.openmbean.CompositeData} object.
   * The composite data instance should contain the following
   * attributes:
   * </p>
   * <ul>
   * <li>init</li>
   * <li>used</li>
   * <li>committed</li>
   * <li>max</li>
   * </ul>
   * <p>
   * All should have the type, <code>java.lang.Long</code>.
   * </p>
   *
   * @param data the composite data structure to take values from.
   * @return a new instance containing the values from the
   *         composite data structure, or <code>null</code>
   *         if the data structure was also <code>null</code>.
   * @throws IllegalArgumentException if the composite data structure
   *                                  does not match the structure
   *                                  outlined above, or the values
   *                                  are invalid.
   */
  public static MemoryUsage from(CompositeData data)
  {
    if (data == null)
      return null;
    CompositeType type = data.getCompositeType();
    ThreadInfo.checkAttribute(type, "Init", SimpleType.LONG);
    ThreadInfo.checkAttribute(type, "Used", SimpleType.LONG);
    ThreadInfo.checkAttribute(type, "Committed", SimpleType.LONG);
    ThreadInfo.checkAttribute(type, "Max", SimpleType.LONG);
    return new MemoryUsage(((Long) data.get("Init")).longValue(),
                           ((Long) data.get("Used")).longValue(),
                           ((Long) data.get("Committed")).longValue(),
                           ((Long) data.get("Max")).longValue());
  }

  /**
   * Returns the amount of memory committed for use by this
   * memory pool (in bytes).  This amount is guaranteed to
   * be available, unlike the maximum.
   *
   * @return the committed amount of memory.
   */
  public long getCommitted()
  {
    return committed;
  }

  /**
   * Returns the initial amount of memory allocated to the
   * pool (in bytes).  This method may return -1, if the
   * value is undefined.
   *
   * @return the initial amount of memory allocated, or -1
   *         if this value is undefined.
   */
  public long getInit()
  {
    return init;
  }

  /**
   * Returns the maximum amount of memory available for this
   * pool (in bytes).  This amount is not guaranteed to
   * actually be usable.  This method may return -1, if the
   * value is undefined.
   *
   * @return the maximum amount of memory available, or -1
   *         if this value is undefined.
   */
  public long getMax()
  {
    return maximum;
  }

  /**
   * Returns the amount of memory used (in bytes).
   *
   * @return the amount of used memory.
   */
  public long getUsed()
  {
    return used;
  }

  /**
   * Returns a {@link java.lang.String} representation of
   * this {@link MemoryUsage} object.  This takes the form
   * <code>java.lang.management.MemoryUsage[init=i, used=u,
   * committed=c, maximum=m]</code>, where <code>i</code>
   * is the initial level, <code>u</code> is the used level,
   * <code>c</code> is the committed level and <code>m</code>
   * is the maximum level.
   *
   * @return the string specified above.
   */
  public String toString()
  {
    int megabyte = 1024 * 1024;
    return getClass().getName() +
      "[init=" + init + " bytes (~" + (init / megabyte) +
      "MB), used=" + used + " bytes (~" + (used / megabyte) +
      "MB), committed=" + committed + " bytes (~" + (committed / megabyte) +
      "MB), maximum=" + maximum + " bytes (~" + (maximum / megabyte) +
      "MB)]";
  }

}
