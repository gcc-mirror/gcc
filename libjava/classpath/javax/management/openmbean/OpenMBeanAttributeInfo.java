/* OpenMBeanAttributeInfo.java -- Open typed info about an attribute.
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

/**
 * Describes an attribute associated with an open management bean.
 * This interface includes those methods specified by {@link
 * javax.management.MBeanAttributeInfo}, so implementations should
 * extend this class.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface OpenMBeanAttributeInfo
  extends OpenMBeanParameterInfo
{

  /**
   * Compares this attribute with the supplied object.  This returns
   * true iff the object is an instance of {@link OpenMBeanAttributeInfo}
   * with an equal name and open type, the same default, minimum,
   * maximum and legal values and the same access properties
   * ({@link #isIs()}, {@link #isReadable()}, {@link #isWritable()}).
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link OpenMBeanParameterInfo}
   *         instance, 
   *         <code>name.equals(object.getName())</code>,
   *         <code>openType.equals(object.getOpenType())</code>,
   *         <code>defaultValue.equals(object.getDefaultValue())</code>,
   *         <code>minValue.equals(object.getMinValue())</code>,
   *         <code>maxValue.equals(object.getMaxValue())</code>,
   *         <code>legalValues.equals(object.getLegalValues())</code>,
   *         <code>is == object.isIs()</code>,
   *         <code>isRead == object.isReadable()</code>,
   *         and <code>isWrite == object.isWritable()</code>.
   */
  boolean equals(Object obj);

  /**
   * Returns the hashcode of the attribute information as the sum of
   * the hashcodes of the name, open type, default value, maximum
   * value, minimum value, the set of legal values and the access
   * properties.
   *
   * @return the hashcode of the attribute information.
   */
  int hashCode();

  /**
   * Returns true if the accessor method of this attribute
   * is of the form <code>isXXX</code>.
   *
   * @return true if the accessor takes the form <code>isXXX</code>.
   */
  boolean isIs();

  /**
   * Returns true if value of this attribute can be read.
   *
   * @return true if the value of the attribute can be read.
   */
  boolean isReadable();

  /**
   * Returns true if the value of this attribute can be changed.
   *
   * @return true if the value of the attribute can be changed.
   */
  boolean isWritable();

  /**
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanAttributeInfo</code>)
   * along with the name, open type, default, minimum, maximum
   * and legal values of the parameter and the access permissions
   * ({@link #isIs()}, {@link #isReadable()}, {@link #isWritable()}).
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  String toString();

}
