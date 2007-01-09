/* OpenMBeanInfo.java -- Open typed info about a management bean.
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

package javax.management.openmbean;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanOperationInfo;

/**
 * Describes an open management bean.  Open management beans are
 * management beans where {@link
 * javax.management.DynamicMBean#getMBeanInfo()} returns an
 * implementation of this interface.  This interface includes those
 * methods specified by {@link javax.management.MBeanInfo},
 * so implementations should extend this class.  Each method
 * which returns an array of one of the <code>MBeanXXXInfo</code>
 * classes should return an array containing instances
 * of the equivalent open version (<code>OpenMBeanXXXInfo</code>).
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface OpenMBeanInfo
{

  /**
   * Compares this attribute with the supplied object.  This returns
   * true iff the object is an instance of {@link OpenMBeanInfo}
   * with the same class name and equal instances of the info classes.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link OpenMBeanInfo}
   *         instance, 
   *         <code>className.equals(object.getClassName())</code>
   *         and each info class has an equal in the other object.
   */
  boolean equals(Object obj);

  /**
   * Returns descriptions of each of the attributes provided by this
   * management bean.  The elements should be implementations of the
   * {@link OpenMBeanAttributeInfo} class.
   *
   * @return an array of {@link OpenMBeanAttributeInfo} objects,
   *         representing the attributes emitted by this
   *         management bean.
   */
  MBeanAttributeInfo[] getAttributes();

  /**
   * Returns the class name of the management bean.
   *
   * @return the bean's class name.
   */
  String getClassName();

  /**
   * Returns descriptions of each of the constructors provided by this
   * management bean.  The elements should be implementations of the
   * {@link OpenMBeanConstructorInfo} class.
   *
   * @return an array of {@link OpenMBeanConstructorInfo} objects,
   *         representing the constructors emitted by this
   *         management bean.
   */
  MBeanConstructorInfo[] getConstructors();

  /**
   * Returns a description of this operation.
   *
   * @return a human-readable description.
   */
  String getDescription();

  /**
   * Returns descriptions of each of the notifications provided by this
   * management bean.  The elements should be implementations of the
   * {@link OpenMBeanNotificationInfo} class.
   *
   * @return an array of {@link OpenMBeanNotificationInfo} objects,
   *         representing the notifications emitted by this
   *         management bean.
   */
  MBeanNotificationInfo[] getNotifications();

  /**
   * Returns descriptions of each of the operations provided by this
   * management bean.  The elements should be implementations of the
   * {@link OpenMBeanOperationInfo} class.
   *
   * @return an array of {@link OpenMBeanOperationInfo} objects,
   *         representing the operations emitted by this
   *         management bean.
   */
  MBeanOperationInfo[] getOperations();

  /**
   * Returns the hashcode of the bean information as the sum of the
   * hashcodes of the class name and each array (calculated using
   * java.util.HashSet(<code>java.util.Arrays.asList(signature)).hashCode()</code>).
   *
   * @return the hashcode of the bean information.
   */
  int hashCode();

  /**
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanInfo</code>)
   * along with the class name and textual representations
   * of each array.
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  String toString();

}
