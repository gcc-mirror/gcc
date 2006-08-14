/* MemoryNotificationInfo.java - Emitted memory notification info.
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

import gnu.java.lang.management.MemoryMXBeanImpl;

import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.SimpleType;

/**
 * <p>
 * Represents the content of a notification emitted by the
 * {@link MemoryMXBean}.  Such notifications are emitted when
 * one of the memory pools exceeds its usage or collection
 * usage threshold.  This object contains the following information,
 * representing the state of the pool at the time of the
 * notification:
 * </p>
 * <ul>
 * <li>The name of the pool.</li>
 * <li>The memory usage of the pool at the time of notification.</li>
 * <li>The number of times the pool has exceeded this particular
 * threshold in the past.</li>
 * </ul>
 * <p>
 * Two types of notification are emitted by the {@link MemoryMXBean}:
 * one for exceeding the usage threshold and one for exceeding the
 * collection usage threshold.  The value returned by {@link #getCount()}
 * is dependent on this type; if the threshold exceeded is the usage
 * threshold, then the usage threshold count is returned.  If, instead,
 * the collection usage threshold is exceeded, then the collection usage
 * threshold count is returned.
 * </p>
 * <p>
 * This data is held in the user data part of the notification (returned
 * by {@link javax.management.Notification#getUserData()}) encapsulated in
 * a {@link javax.management.openmbean.CompositeData} object.  The
 * {@link #from(javax.management.openmbean.CompositeData)} method may be
 * used to unwrap the value and obtain an instance of this class.
 * </p>
 * 
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MemoryNotificationInfo
{

  /**
   * The type of notification emitted when the usage threshold is exceeded.
   * After a notification is emitted, the usage level must drop below the
   * threshold again before another notification is emitted.  The value is
   * <code>java.management.memory.threshold.exceeded</code>.
   */
  public static final String MEMORY_THRESHOLD_EXCEEDED = 
    "java.management.memory.threshold.exceeded";

  /**
   * The type of notification emitted when the collection usage threshold
   * is exceeded, following a garbage collection cycle.  The value is
   * <code>java.management.memory.collection.threshold.exceeded</code>.
   */
  public static final String MEMORY_COLLECTION_THRESHOLD_EXCEEDED =
    "java.management.memory.collection.threshold.exceeded";

  /**
   * The name of the memory pool which exceeded the threshold.
   */
  private String poolName;

  /**
   * The usage level of the memory pool at the time of notification.
   */
  private MemoryUsage usage;

  /**
   * The number of times the threshold has been crossed.
   */
  private long count;

  /**
   * Constructs a new {@link MemoryNotificationInfo} object using the
   * specified pool name, usage level and threshold crossing count.
   *
   * @param poolName the name of the pool which has exceeded a threshold.
   * @param usage the usage level of the pool at the time of notification.
   * @param count the number of times the threshold has been crossed.
   */
  public MemoryNotificationInfo(String poolName, MemoryUsage usage, long count)
  {
    this.poolName = poolName;
    this.usage = usage;
    this.count = count;
  }

  /**
   * <p>
   * Returns a {@link MemoryNotificationInfo} instance using the values
   * given in the supplied
   * {@link javax.management.openmbean.CompositeData} object.
   * The composite data instance should contain the following
   * attributes with the specified types:
   * </p>
   * <table>
   * <th><td>Name</td><td>Type</td></th>
   * <tr><td>poolName</td><td>java.lang.String</td></tr>
   * <tr><td>usage</td><td>javax.management.openmbean.CompositeData
   * </td></tr>
   * <tr><td>count</td><td>java.lang.Long</td></tr>
   * </table>
   * <p>
   * The usage level is further described as:
   * </p>
   * <table>
   * <th><td>Name</td><td>Type</td></th>
   * <tr><td>init</td><td>java.lang.Long</td></tr>
   * <tr><td>used</td><td>java.lang.Long</td></tr>
   * <tr><td>committed</td><td>java.lang.Long</td></tr>
   * <tr><td>max</td><td>java.lang.Long</td></tr>
   * </table>
   * 
   * @param data the composite data structure to take values from.
   * @return a new instance containing the values from the 
   *         composite data structure, or <code>null</code>
   *         if the data structure was also <code>null</code>.
   * @throws IllegalArgumentException if the composite data structure
   *                                  does not match the structure
   *                                  outlined above.
   */
  public static MemoryNotificationInfo from(CompositeData data)
  {
    if (data == null)
      return null;
    CompositeType type = data.getCompositeType();
    ThreadInfo.checkAttribute(type, "poolName", SimpleType.STRING);
    ThreadInfo.checkAttribute(type, "usage", MemoryMXBeanImpl.usageType);
    ThreadInfo.checkAttribute(type, "count", SimpleType.LONG);
    MemoryUsage usage = MemoryUsage.from((CompositeData) data.get("usage"));
    return new MemoryNotificationInfo(((String) data.get("poolName")),
				      usage,
				      ((Long) data.get("count")).longValue());
  }

  /**
   * Returns the number of times the memory pool has crossed the usage
   * threshold, as of the time of notification.  If this is the notification
   * represented by the type {@link #MEMORY_THRESHOLD_EXCEEDED}, then the
   * count is the usage threshold count.  If this is the notification
   * represented by the type {@link #MEMORY_COLLECTION_THRESHOLD_EXCEEDED},
   * then the count is the collection usage threshold count.
   *
   * @return the number of times the appropriate threshold has been crossed.
   */
  public long getCount()
  {
    return count;
  }

  /**
   * Returns the name of the pool which has crossed a threshold.
   *
   * @return the name of the pool.
   */
  public String getPoolName()
  {
    return poolName;
  }

  /**
   * Returns the usage levels at the time of notification.
   *
   * @return the usage levels.
   */
  public MemoryUsage getUsage()
  {
    return usage;
  }

}

