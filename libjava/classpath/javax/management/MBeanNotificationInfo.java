/* MBeanNotificationInfo.java -- Information about a bean's notification.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

import java.util.Arrays;

/**
 * <p>
 * Describes the notifications emitted by a management bean.
 * An instance of this class is specific to notifications
 * involving a particular type of object.  A new instance
 * should be created for each Java class used for notifications,
 * and the Java class name forms the name of the instance.
 * Each instance lists a number of notification types; these
 * are not types in the sense of different Java classes, but
 * instead form the names of notifications following the same
 * syntax as Java property and package names.
 * </p>
 * <p>
 * For instance, a management bean may emit two notifications
 * containing {@link java.lang.String} objects.  Both would be described
 * using one instance of this class, with a member of the array
 * returned by {@link #getNotifTypes()} for each one.  If another
 * notification containing a {@link java.util.Date} object were to
 * be added, this would require a new instance of this class.
 * </p>
 * <p>
 * The information in this class is immutable as standard.
 * Of course, subclasses may change this, but this
 * behaviour is not recommended.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanNotificationInfo
  extends MBeanFeatureInfo
  implements Cloneable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -3888371564530107064L;

  /**
   * The types of notification described by this instance.
   *
   * @serial the types of notification.
   */
  private String[] types;

  /**
   * Constructs a new {@link MBeanNotificationInfo} with the
   * specified name, description and notification types. The
   * notification types array may be <code>null</code> or of
   * zero length, in order to indicate the absence of any types.
   *
   * @param types an array of {@link java.lang.String} objects,
   *              containing the names of the notifications emitted
   *              of this Java type.  The names use the dot notation
   *              familiar from Java property and package names.
   * @param name the name of the Java class the notifications described
   *             by this object are instances of.
   * @param description a description of the data.
   * @throws IllegalArgumentException for some reason...
   */
  public MBeanNotificationInfo(String[] types, String name,
                               String description)
  {
    super(name, description);
    this.types = types;
  }

  /**
   * Returns a clone of this instance.  The clone is created
   * using just the method provided by {@link java.lang.Object}.
   * Thus, the clone is just a shallow clone as returned by
   * that method, and does not contain any deeper cloning based
   * on the subject of this class.
   *
   * @return a clone of this instance.
   * @see java.lang.Cloneable
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        /* This shouldn't happen; we implement Cloneable */
        throw new IllegalStateException("clone() called on " +
                                        "non-cloneable object.");
      }
  }

  /**
   * Compares this feature with the supplied object.  This returns
   * true iff the object is an instance of {@link
   * MBeanNotificationInfo}, {@link Object#equals()} returns true for
   * a comparison of both the name and description of this
   * notification with that of the specified object, and the two
   * notification type arrays contain the same elements in the same
   * order (but one may be longer than the other).
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link MBeanNotificationInfo}
   *         instance,
   *         <code>name.equals(object.getName())</code>,
   *         <code>description.equals(object.getDescription())</code>
   *         and the corresponding elements of the type arrays are
   *         equal.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof MBeanNotificationInfo)
      {
        if (!(super.equals(obj)))
          return false;
        MBeanNotificationInfo o = (MBeanNotificationInfo) obj;
        String[] oTypes = o.getNotifTypes();
        for (int a = 0; a < types.length; ++a)
          {
            if (a == oTypes.length)
              return true;
            if (!(types[a].equals(oTypes[a])))
              return false;
          }
        return true;
      }
    else
      return false;
  }

  /**
   * Returns the notification types that the management bean may emit.
   * The notification types are strings using the dot notation
   * familiar from Java property and package names.  Changing the
   * returned array does not affect the values retained by this
   * instance.
   *
   * @return the notification types.
   */
  public String[] getNotifTypes()
  {
    return types;
  }

  /**
   * Returns the hashcode of the notification information as the sum
   * of the hashcode of the superclass and the hashcode of the types
   * array.
   *
   * @return the hashcode of the notification information.
   */
  public int hashCode()
  {
    return super.hashCode() + Arrays.hashCode(types);
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.MBeanNotificationInfo</code>),
   * the name and description of the notification and the
   * contents of the array of types.
   * </p>
   * <p>
   * As instances of this class are immutable, the return value
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  public String toString()
  {
    if (string == null)
      {
        super.toString();
        string = string.substring(0, string.length() - 1)
          + ",types=" + Arrays.toString(types)
          + "]";
      }
    return string;
  }

}
