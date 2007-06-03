/* OpenMBeanParameterInfo.java -- Open typed info about a parameter.
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

import java.util.Set;

/**
 * Describes the parameters of a constructor or operation associated
 * with an open management bean.  This interface includes those methods
 * specified by {@link javax.management.MBeanParameterInfo}, so
 * implementations should extend this class.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface OpenMBeanParameterInfo
{

  /**
   * Compares this parameter with the supplied object.  This returns
   * true iff the object is an instance of {@link OpenMBeanParameterInfo}
   * with an equal name and open type and the same default, minimum,
   * maximum and legal values.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link OpenMBeanParameterInfo}
   *         instance, 
   *         <code>name.equals(object.getName())</code>,
   *         <code>openType.equals(object.getOpenType())</code>,
   *         <code>defaultValue.equals(object.getDefaultValue())</code>,
   *         <code>minValue.equals(object.getMinValue())</code>,
   *         <code>maxValue.equals(object.getMaxValue())</code>,
   *         and <code>legalValues.equals(object.getLegalValues())</code>.
   */
  boolean equals(Object obj);

  /**
   * Returns the default value of this parameter, or <code>null</code>
   * if there is no default value.
   *
   * @return the default value of the parameter, or <code>null</code>
   *         if there is no default.
   */
  Object getDefaultValue();

  /**
   * Returns a description of this parameter.
   *
   * @return a human-readable description.
   */
  String getDescription();

  /**
   * Returns a {@link java.util.Set} enumerating the legal values
   * of this parameter, or <code>null</code> if no such limited
   * set exists for this parameter.
   *
   * @return a set of legal values, or <code>null</code> if no such
   *         set exists.
   */
  Set<?> getLegalValues();

  /**
   * Returns the maximum value of this parameter, or <code>null</code>
   * if there is no maximum.
   *
   * @return the maximum value, or <code>null</code> if none exists.
   */
  Comparable<?> getMaxValue();

  /**
   * Returns the minimum value of this parameter, or <code>null</code>
   * if there is no minimum.
   *
   * @return the minimum value, or <code>null</code> if none exists.
   */
  Comparable<?> getMinValue();

  /**
   * Returns the name of this parameter.
   *
   * @return the name of the parameter.
   */
  String getName();

  /**
   * Returns the open type instance which represents the type of this
   * parameter.
   *
   * @return the open type of this parameter.
   */
  OpenType<?> getOpenType();

  /**
   * Returns true if this parameter has a default value.
   *
   * @return true if this parameter has a default.
   */
  boolean hasDefaultValue();

  /**
   * Returns the hashcode of the parameter information as the sum of
   * the hashcodes of the name, open type, default value, maximum
   * value, minimum value and the set of legal values.
   *
   * @return the hashcode of the parameter information.
   */
  int hashCode();

  /**
   * Returns true if there is a set of legal values for this
   * parameter.
   *
   * @return true if a set of legal values exists for this
   *         parameter.
   */
  boolean hasLegalValues();

  /**
   * Returns true if there is a maximum value for this parameter.
   *
   * @return true if a maximum value exists for this parameter.
   */
  boolean hasMaxValue();

  /**
   * Returns true if there is a minimum value for this parameter.
   *
   * @return true if a minimum value exists for this parameter.
   */
  boolean hasMinValue();

  /**
   * Returns true if the specified object is a valid value for
   * this parameter.
   *
   * @param obj the object to test.
   * @return true if <code>obj</code> is a valid value for this
   *         parameter.
   */
  boolean isValue(Object obj);

  /**
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanParameterInfo</code>)
   * along with the name, open type, default, minimum, maximum
   * and legal values of the parameter.
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  String toString();

}
