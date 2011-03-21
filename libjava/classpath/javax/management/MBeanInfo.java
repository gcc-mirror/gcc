/* MBeanInfo.java -- Information about a management bean.
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

import java.io.Serializable;

import java.util.Arrays;

/**
 * <p>
 * Describes the interface of a management bean.  This allows
 * the user to access the bean dynamically, without knowing
 * the details of any of its attributes, operations,
 * constructors or notifications beforehand.  The information
 * is immutable as standard.  Of course, subclasses may change
 * this, but this behaviour is not recommended.
 * </p>
 * <p>
 * The contents of this class, for standard management beans,
 * are dynamically compiled using reflection.
 * {@link #getClassName()} and {@link #getConstructors()}
 * return the name of the class and its constructors, respectively.
 * This is much the same as could be obtained by reflection on the
 * bean.  {@link #getAttributes()} and {@link #getOperations()},
 * however, do something more in splitting the methods of the
 * class into two sets.  Those of the form, <code>getXXX</code>,
 * <code>setXXX</code> and <code>isXXX</code> are taken to be
 * the accessors and mutators of a series of attributes, with
 * <code>XXX</code> being the attribute name.  These are returned
 * by {@link getAttributes()} and the {@link Attribute} class can
 * be used to manipulate them.  The remaining methods are classified
 * as operations and returned by {@link getOperations()}.
 * </p>
 * <p>
 * Beans can also broadcast notifications.  If the bean provides this
 * facility, by implementing the {@link NotificationBroadcaster}
 * interface, then an array of {@link MBeanNotificationInfo} objects
 * may be obtained from {@link #getNotifications()}, which describe
 * the notifications emitted.
 * </p>
 * <p>
 * Model management beans and open management beans also supply an
 * instance of this class, as part of implementing the
 * {@link DynamicMBean#getMBeanInfo()} method of {@link DynamicMBean}.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanInfo
  implements Cloneable, Serializable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -6451021435135161911L;

  /**
   * A description of the bean.
   *
   * @serial The bean's description.
   */
  private String description;

  /**
   * The class name of the management bean.
   *
   * @serial The bean's class name.
   */
  private String className;

  /**
   * Descriptions of the attributes provided by the bean.
   */
  private MBeanAttributeInfo[] attributes;

  /**
   * Descriptions of the operations provided by the bean.
   */
  private MBeanOperationInfo[] operations;

  /**
   * Descriptions of the bean's constructors.
   */
  private MBeanConstructorInfo[] constructors;

  /**
   * Descriptions of the notifications emitted by the bean.
   *
   * @serial The bean's notifications.
   */
  private MBeanNotificationInfo[] notifications;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Constructs a new {@link MBeanInfo} using the supplied
   * class name and description with the given attributes,
   * operations, constructors and notifications.  The class
   * name does not have to actually specify a valid class that
   * can be loaded by the MBean server or class loader; it merely
   * has to be a syntactically correct class name.  Any of the
   * arrays may be <code>null</code>; this will be treated as if
   * an empty array was supplied.  A copy of the arrays is
   * taken, so later changes have no effect.
   *
   * @param name the name of the class this instance describes.
   * @param desc a description of the bean.
   * @param attribs the attribute descriptions for the bean,
   *                or <code>null</code>.
   * @param cons the constructor descriptions for the bean,
   *             or <code>null</code>.
   * @param ops the operation descriptions for the bean,
   *            or <code>null</code>.
   * @param notifs the notification descriptions for the bean,
   *               or <code>null</code>.
   */
  public MBeanInfo(String name, String desc, MBeanAttributeInfo[] attribs,
                   MBeanConstructorInfo[] cons, MBeanOperationInfo[] ops,
                   MBeanNotificationInfo[] notifs)
  {
    className = name;
    description = desc;

    if (attribs == null)
      attributes = new MBeanAttributeInfo[0];
    else
      attributes = (MBeanAttributeInfo[]) attribs.clone();

    if (cons == null)
      constructors = new MBeanConstructorInfo[0];
    else
      constructors = (MBeanConstructorInfo[]) cons.clone();

    if (ops == null)
      operations = new MBeanOperationInfo[0];
    else
      operations = (MBeanOperationInfo[]) ops.clone();

    if (notifs == null)
      notifications = new MBeanNotificationInfo[0];
    else
      notifications = (MBeanNotificationInfo[]) notifs.clone();
  }

  /**
   * Returns a shallow clone of the information.  This is
   * simply a new copy of each string and a clone
   * of each array, which still references the same objects,
   * as obtained by the {@link Object} implementation of
   * {@link Object#clone()}.  As the fields can not be
   * changed, this method is only really of interest to
   * subclasses which may add new mutable fields or make
   * the existing ones mutable.
   *
   * @return a shallow clone of this {@link MBeanInfo}.
   */
  public Object clone()
  {
    MBeanInfo clone = null;
    try
      {
        clone = (MBeanInfo) super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        /* This won't happen as we implement Cloneable */
      }
    return clone;
  }

  /**
   * Compares this feature with the supplied object.  This returns
   * true iff the object is an instance of {@link MBeanInfo} and
   * {@link Object#equals()} returns true for a comparison of the
   * class name and description, and the arrays each contain the same
   * elements in the same order (but one may be longer than the
   * other).
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link MBeanInfo}
   *         instance,
   *         <code>className.equals(object.getClassName())</code>,
   *         <code>description.equals(object.getDescription())</code>
   *         and the corresponding elements of the arrays are
   *         equal.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof MBeanInfo))
      return false;
    if (!(super.equals(obj)))
      return false;
    MBeanInfo o = (MBeanInfo) obj;
    MBeanAttributeInfo[] attr = o.getAttributes();
    for (int a = 0; a < attributes.length; ++a)
      {
        if (a == attr.length)
          return true;
        if (!(attributes[a].equals(attr[a])))
          return false;
      }
    MBeanConstructorInfo[] cons = o.getConstructors();
    for (int a = 0; a < constructors.length; ++a)
      {
        if (a == cons.length)
          return true;
        if (!(constructors[a].equals(cons[a])))
          return false;
      }
    MBeanOperationInfo[] ops = o.getOperations();
    for (int a = 0; a < operations.length; ++a)
      {
        if (a == ops.length)
          return true;
        if (!(operations[a].equals(ops[a])))
          return false;
      }
    MBeanNotificationInfo[] notifs = o.getNotifications();
    for (int a = 0; a < notifications.length; ++a)
      {
        if (a == notifs.length)
          return true;
        if (!(notifications[a].equals(notifs[a])))
          return false;
      }
    return (className.equals(o.getClassName()) &&
            description.equals(o.getDescription()));
  }

  /**
   * Returns descriptions of each of the attributes provided
   * by this management bean.  The returned value is a shallow
   * copy of the attribute array maintained by this instance.
   * Hence, changing the elements of the returned array will not
   * affect the attribute array, and the elements (instances
   * of the {@link MBeanAttributeInfo} class) are immutable.
   *
   * @return an array of {@link MBeanAttributeInfo} objects,
   *         representing the attributes emitted by this
   *         management bean.
   */
  public MBeanAttributeInfo[] getAttributes()
  {
    return (MBeanAttributeInfo[]) attributes.clone();
  }

  /**
   * Returns the class name of the management bean.
   *
   * @return the bean's class name.
   */
  public String getClassName()
  {
    return className;
  }

  /**
   * Returns descriptions of each of the constructors provided
   * by this management bean.  The returned value is a shallow
   * copy of the constructor array maintained by this instance.
   * Hence, changing the elements of the returned array will not
   * affect the constructor array, and the elements (instances
   * of the {@link MBeanConstructorInfo} class) are immutable.
   *
   * @return an array of {@link MBeanConstructorInfo} objects,
   *         representing the constructors emitted by this
   *         management bean.
   */
  public MBeanConstructorInfo[] getConstructors()
  {
    return (MBeanConstructorInfo[]) constructors.clone();
  }

  /**
   * Returns a description of the management bean.
   *
   * @return the bean's description.
   */
  public String getDescription()
  {
    return description;
  }

  /**
   * Returns descriptions of each of the notifications emitted
   * by this management bean.  The returned value is a shallow
   * copy of the notification array maintained by this instance.
   * Hence, changing the elements of the returned array will not
   * affect the notification array, and the elements (instances
   * of the {@link MBeanNotificationInfo} class) are immutable.
   *
   * @return an array of {@link MBeanNotificationInfo} objects,
   *         representing the notifications emitted by this
   *         management bean.
   */
  public MBeanNotificationInfo[] getNotifications()
  {
    return (MBeanNotificationInfo[]) notifications.clone();
  }

  /**
   * Returns descriptions of each of the operations provided
   * by this management bean.  The returned value is a shallow
   * copy of the operation array maintained by this instance.
   * Hence, changing the elements of the returned array will not
   * affect the operation array, and the elements (instances
   * of the {@link MBeanOperationInfo} class) are immutable.
   *
   * @return an array of {@link MBeanOperationInfo} objects,
   *         representing the operations emitted by this
   *         management bean.
   */
  public MBeanOperationInfo[] getOperations()
  {
    return (MBeanOperationInfo[]) operations.clone();
  }

  /**
   * Returns the hashcode of the information as the sum of the
   * hashcode of the classname, description and each array.
   *
   * @return the hashcode of the information.
   */
  public int hashCode()
  {
    return className.hashCode() + description.hashCode()
      + Arrays.hashCode(attributes) + Arrays.hashCode(constructors)
      + Arrays.hashCode(operations) + Arrays.hashCode(notifications);
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.MBeanInfo</code>),
   * the name and description of the bean and the contents
   * of the four arrays.
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
      string = getClass().getName()
        + "[name=" + className
        + ",desc=" + description
        + ",attributes=" + Arrays.toString(attributes)
        + ",constructors=" + Arrays.toString(constructors)
        + ",operations=" + Arrays.toString(operations)
        + ",notifications=" + Arrays.toString(notifications)
        + "]";
    return string;
  }

}
