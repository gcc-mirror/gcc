/* MonitorInfo.java - Information on a monitor lock.
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
 * Provides information on a monitor lock held by a thread.
 * A monitor lock is obtained when a thread enters a synchronized
 * block or method.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public class MonitorInfo
  extends LockInfo
{

  /**
   * The stack depth at which the lock was obtained.
   */
  private int stackDepth;

  /**
   * The stack frame at which the lock was obtained.
   */
  private StackTraceElement stackFrame;

  /**
   * Constructs a new {@link MonitorInfo} using the specified
   * lock class name and identity hash code, and the given
   * stack depth and frame.
   *
   * @param className the class name of the lock object.
   * @param identityHashCode the identity hash code of the lock object.
   * @param stackDepth the depth of the stack at which the lock
   *                   was obtained.
   * @param stackFrame the frame of the stack at which the lock was
   *                   obtained.
   * @throws IllegalArgumentException if the stack depth and frame are
   *                                  inconsistent i.e. the frame is
   *                                  <code>null</code> but the depth is
   *                                  &ge; 0, or the frame is not
   *                                  <code>null</code> but the depth is
   *                                  &lt; 0.
   */
  public MonitorInfo(String className, int identityHashCode, int stackDepth,
                     StackTraceElement stackFrame)
  {
    super(className, identityHashCode);
    if (stackFrame == null && stackDepth >= 0)
      throw new IllegalArgumentException("The stack frame is null, but the " +
                                         "stack depth is greater than or equal " +
                                         "to zero.");
    if (stackFrame != null && stackDepth < 0)
      throw new IllegalArgumentException("The stack frame is not null, but the " +
                                         "stack depth is less than zero.");
    this.stackDepth = stackDepth;
    this.stackFrame = stackFrame;
  }

  /**
   * <p>
   * Returns a {@link MonitorInfo} instance using the values
   * given in the supplied
   * {@link javax.management.openmbean.CompositeData} object.
   * The composite data instance should contain the following
   * attributes with the specified types:
   * </p>
   * <table>
   * <th><td>Name</td><td>Type</td></th>
   * <tr><td>className</td><td>java.lang.String</td></tr>
   * <tr><td>identityHashCode</td><td>java.lang.Integer</td></tr>
   * <tr><td>lockedStackDepth</td><td>java.lang.Integer</td></tr>
   * <tr><td>lockedStackFrame</td><td>javax.management.openmbean.CompositeData
   * </td></tr>
   * </table>
   * <p>
   * The stack trace is further described as:
   * </p>
   * <table>
   * <th><td>Name</td><td>Type</td></th>
   * <tr><td>className</td><td>java.lang.String</td></tr>
   * <tr><td>methodName</td><td>java.lang.String</td></tr>
   * <tr><td>fileName</td><td>java.lang.String</td></tr>
   * <tr><td>lineNumber</td><td>java.lang.Integer</td></tr>
   * <tr><td>nativeMethod</td><td>java.lang.Boolean</td></tr>
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
  public static MonitorInfo from(CompositeData data)
  {
    if (data == null)
      return null;
    CompositeType type = data.getCompositeType();
    ThreadInfo.checkAttribute(type, "ClassName", SimpleType.STRING);
    ThreadInfo.checkAttribute(type, "IdentityHashCode", SimpleType.INTEGER);
    ThreadInfo.checkAttribute(type, "LockedStackDepth", SimpleType.INTEGER);
    ThreadInfo.checkAttribute(type, "LockedStackFrame",
                              ThreadInfo.getStackTraceType());
    CompositeData frame = (CompositeData) data.get("LockedStackFrame");
    return new MonitorInfo((String) data.get("ClassName"),
                           (Integer) data.get("IdentityHashCode"),
                           (Integer) data.get("LockedStackDepth"),
                           new StackTraceElement((String) frame.get("ClassName"),
                                                 (String) frame.get("MethodName"),
                                                 (String) frame.get("FileName"),
                                                 (Integer) frame.get("LineNumber")));
  }

  /**
   * Returns the depth of the stack at which the lock was obtained.
   * This works as an index into the array returned by
   * {@link ThreadInfo#getStackTrace()}.
   *
   * @return the depth of the stack at which the lock was obtained,
   *         or a negative number if this information is unavailable.
   */
  public int getLockedStackDepth()
  {
    return stackDepth;
  }

  /**
   * Returns the stack frame at which the lock was obtained.
   *
   * @return the stack frame at which the lock was obtained,
   *         or <code>null</code> if this informati0on is unavailable.
   */
  public StackTraceElement getLockedStackFrame()
  {
    return stackFrame;
  }

}
