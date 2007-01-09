/* OpenMBeanInfoSupport.java -- Open typed info about a bean.
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

import java.util.Arrays;
import java.util.HashSet;

import javax.management.MBeanInfo;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanOperationInfo;

/**
 * Describes an open management bean. 
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class OpenMBeanInfoSupport
  extends MBeanInfo
  implements OpenMBeanInfo
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 4349395935420511492L;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Constructs a new {@link OpenMBeanInfo} using the supplied
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
   * @throws ArrayStoreException if a members of an array
   *                             is not assignable to the equivalent
   *                             <code>MBeanXXXInfo</code> class.
   */
  public OpenMBeanInfoSupport(String name, String desc, 
			      OpenMBeanAttributeInfo[] attribs,
			      OpenMBeanConstructorInfo[] cons, 
			      OpenMBeanOperationInfo[] ops,
			      MBeanNotificationInfo[] notifs)
  {
    super(name, desc, (MBeanAttributeInfo[]) attribs,
	  (MBeanConstructorInfo[]) cons,
	  (MBeanOperationInfo[]) ops,
	  notifs);
  }

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
  public boolean equals(Object obj)
  {
    if (!(obj instanceof OpenMBeanInfo))
      return false;
    OpenMBeanInfo o = (OpenMBeanInfo) obj;
    return getClassName().equals(o.getClassName()) &&
      getAttributes().equals(o.getAttributes()) &&
      getConstructors().equals(o.getConstructors()) &&
      getNotifications().equals(o.getNotifications()) &&
      getOperations().equals(o.getOperations());
  }

  /**
   * <p>
   * Returns the hashcode of the bean information as the sum of the
   * hashcodes of the class name and each array (calculated using
   * java.util.HashSet(<code>java.util.Arrays.asList(signature)).hashCode()</code>).
   * </p>
   * <p>
   * As instances of this class are immutable, the return value
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hashcode of the bean information.
   */
  public int hashCode()
  {
    if (hashCode == null)
      hashCode = 
	Integer.valueOf(getClassName().hashCode() + 
			new HashSet(Arrays.asList(getAttributes())).hashCode() +
			new HashSet(Arrays.asList(getConstructors())).hashCode() +
			new HashSet(Arrays.asList(getNotifications())).hashCode() +
			new HashSet(Arrays.asList(getOperations())).hashCode());
    return hashCode.intValue();
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanInfo</code>)
   * along with the class name and textual representations
   * of each array.
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
	+ "[className=" + getClassName() 
	+ ",attributes=" + Arrays.toString(getAttributes())
	+ ",constructors=" + Arrays.toString(getConstructors())
	+ ",notifications=" + Arrays.toString(getNotifications())
	+ ",operations=" + Arrays.toString(getOperations())
	+ "]";
    return string;
  }

}
