/* Descriptor.java -- Metadata container.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

/**
 * <p>
 * Provides metadata for a management element as a series
 * of fields, formed from name-value pairs.  Descriptors
 * are usually attached to one of the <code>Info</code>
 * classes, such as {@link MBeanAttributeInfo}.
 * </p>
 * <p>
 * Field names are not case-sensitive, but are case-preserving
 * (in that the same use of case will be returned by
 * {@link #getFields()} and {@link #getFieldNames()}).
 * The type of the value should match the type returned
 * by the <code>getType()</code> method of the associated
 * <code>Info</code> object.  In the case of {@link MXBean}s,
 * this should be the mapped type returned by the mapping rules.
 * </p>
 * <p>
 * The contents of a descriptor may be immutable, in which
 * case, attempts to change the contents of the descriptor
 * will cause an exception to be thrown.  Immutable descriptors
 * are usually instances or subclasses of {@link ImmutableDescriptor},
 * while mutable descriptors are usually instances or subclasses
 * of {@link javax.management.modelmbean.DescriptorSupport}.
 * </p>
 * <p>
 * A series of field names are predefined, but additional
 * ones can be added as needed.  Predefined names never include
 * a period ('.'), and so additional names may safely avoid
 * conflicts by including this character.  It is recommended that
 * additional names make use of the same syntax as Java package
 * names e.g. <code>gnu.classpath.ImportantMetadata</code>.
 * </p>
 * <p>
 * The predefined names are as follows:
 * </p>
 * <table>
 * <th>
 * <td>Name</td><td>Type</td><td>Used In</td><td>Meaning</td>
 * </th>
 * <tr>
 * <td>defaultValue</td><td>Object</td><td>{@link MBeanAttributeInfo},
 * {@link MBeanParameterInfo}</td><td>Default value for an attribute
 * or parameter.</td>
 * </tr>
 * <tr>
 * <td>deprecated</td><td>String</td><td>Any</td><td>The annotated element
 * has been deprecated.  Conventially, the field's value takes the form
 * of the version in which the element was deprecated, followed by an
 * explaination.</td>
 * </tr>
 * <tr>
 * <td>descriptionResourceBundleBaseName</td><td>String</td><td>Any</td>
 * <td>The base name for the bundle in which the <code>descriptionResourceKey</code>
 * can be found.</td>
 * </tr>
 * <tr>
 * <td>descriptionResourceKey</td><td>String</td><td>Any</td>
 * <td>The name of the resource key which contains a localized description of
 * this element.</td>
 * </tr>
 * <tr>
 * <td>enabled</td><td>String</td><td>{@link MBeanAttributeInfo},
 * {@link MBeanNotificationInfo}, {@link MBeanOperationInfo}</td>
 * <td>Specifies whether the annotated element is currently enabled or
 * not, via a value of either <code>"true"</code> or <code>"false"</code>.
 * </tr>
 * <tr>
 * <td>immutableInfo</td><td>String</td><td>{@link MBeanInfo}</td>
 * <td>If the value of this field is <code>"true"</code>, this means that
 * the annotated element will not change and thus may be cached.</td>
 * </tr>
 * <tr>
 * <td>infoTimeout</td><td>String or Long</td><td>{@link MBeanInfo}</td>
 * <td>If this field is present, and non-zero, it will contain a value
 * in milliseconds for which the value of the annotated element will
 * remain unchanged, allowing it to be safely cached for that period.</td>
 * </tr>
 * <tr>
 * <td>interfaceClassName</td><td>String</td><td>{@link MBeanInfo}</td>
 * <td>The Java interface name associated with the bean, as returned
 * by {@link Class#getName()}.</td>
 * </tr>
 * <tr>
 * <td>legalValues</td><td>Set<?></td><td>{@link MBeanAttributeInfo},
 * {@link MBeanParameterInfo}</td><td>Legal values for an attribute
 * or parameter.</td>
 * </tr>
 * <tr>
 * <td>maxValue</td><td>Object</td><td><td>{@link MBeanAttributeInfo},
 * {@link MBeanParameterInfo}</td><td>Maximum legal value for an attribute
 * or parameter.</td>
 * </tr>
 * <tr>
 * <td>metricType</td><td>String</td><td>{@link MBeanAttributeInfo},
 * {@link MBeanOperationInfo}</td><td>Specifies the type of metric represented
 * by the annotated element.  This will be either <code>"counter"</code>
 * (an increasing value, which will only be reset and never decrease)
 * or <code>"gauge"</code> (an increasing and decreasing value).</td>
 * </tr>
 * <tr>
 * <td>minValue</td><td>Object</td><td>{@link MBeanAttributeInfo},
 * {@link MBeanParameterInfo}</td><td>Minimum legal value for an attribute
 * or parameter.</td>
 * </tr>
 * <tr>
 * <td>mxbean</td><td>String</td><td>{@link MBeanInfo}</td>
 * <td>Specifies whether the annotated element is an {@link MXBean} or
 * not, via a value of either <code>"true"</code> or <code>"false"</code>.
 * </tr>
 * <tr>
 * <td>openType</td><td>{@link javax.management.openmbean.OpenType}</td>
 * <td>{@link MBeanAttributeInfo}, {@link MBeanOperationInfo}</td>,
 * {@link MBeanNotificationInfo}, {@link MBeanParameterInfo}</td>
 * <td>Specifies the open type of the attribute or parameter, the return
 * value of the operation or the user data of the notification respectively.
 * This is present on <code>Open*Info</code> instances and for {@link MXBean}s.</td>
 * <tr>
 * <td>originalType</td><td>String</td><td>{@link MBeanAttributeInfo},
 * {@link MBeanOperationInfo}, {@link MBeanParameterInfo}</td>
 * <td>The original Java type of an element which has been converted
 * to another type according to the {@link MXBean} typing rules.
 * For example, {@link java.lang.management.MemoryType} becomes a
 * String after conversion.  This field would contain
 * <code>"java.lang.management.MemoryType"</code> to represent the
 * earlier type of an element with the converted type.</td>
 * </tr>
 * <tr>
 * <td>severity</td><td>Integer or String</td>
 * <td>{@link MBeanNotificationInfo}</td><td>Represents the severity
 * of the notification, ranging from 1 (most severe) to 6 (least
 * severe), with 0 representing unknown severity.</td>
 * </tr>
 * <tr>
 * <td>since</td><td>String</td><td>Any</td>
 * <td>The version in which this field was introduced.</td>
 * </tr>
 * <tr>
 * <td>units</td><td>String</td><td>{@link MBeanAttributeInfo},
 * {@link MBeanOperationInfo}, {@link MBeanParameterInfo}</td>
 * <td>The units used by the value of an attribute or parameter,
 * or the return value of an operation, such as <code>"bytes"</code>,
 * <code>"milliseconds"</code> or <code>"kilogrammes"</code>.
 * </tr>
 * </table>
 * <p>Some names are also defined as part of the Model MBeans package.
 * See {@link javax.management.modelmbean.ModelMBeanInfo}.</p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface Descriptor
  extends Serializable, Cloneable
{

  /**
   * Returns a clone of this descriptor, which can then be modified
   * independently of this descriptor.  If the descriptor is
   * immutable, it is sufficient to return this instance.
   *
   * @return a clone of this descriptor.
   * @throws RuntimeOperationsException if creation of the new
   *                                    descriptor fails for any
   *                                    reason.
   */
  Object clone()
    throws RuntimeOperationsException;

  /**
   * <p>
   * Returns true if this descriptor is equivalent to the
   * supplied object.  This is true if the following holds:
   * </p>
   * <ul>
   * <li>The given object is also a {@link Descriptor}.</li>
   * <li>Has the same field names, independent of case.</li>
   * <li>Has the same values, based on the following:
   * <ul>
   * <li>If one value is <code>null</code>, the other must be.</li>
   * <li>If one value is a primitive array, the other must be a
   * primitive array of the same type with the same elements.</li>
   * <li>If one value is an {@link Object} array, the other
   * must be and {@link java.util.Arrays#deepEquals(Object[],Object[])}
   * must return true.</li>
   * <li>Otherwise, {@link Object#equals(Object)} must return true.</li>
   * </ul>
   *
   * @param obj the object to compare according to the above.
   * @return true if the above holds.
   * @see Object#equals(Object)
   * @see Object#hashCode()
   * @since 1.6
   */
  boolean equals(Object obj);

  /**
   * Returns the field names of the descriptor.  If the
   * descriptor is empty, an empty array is returned.
   *
   * @return the field names as an array of Strings.
   */
  String[] getFieldNames();

  /**
   * <p>
   * Returns all the field name and value pairs, in the
   * form <code>name=value</code>.  The value is converted
   * to a String as follows:
   * </p>
   * <ul>
   * <li>If the value is a String, it is used as is.</li>
   * <li>If the value is <code>null</code>, the printed
   * value will be empty.</li>
   * <li>Otherwise, the value will be converted to a
   * String using its {@link Object#toString()} method,
   * and included as <code>"(" + string + ")"</code>.</li>
   * </ul>
   * <p>If the descriptor is empty, an empty array is returned.</p>
   *
   * @return the field names and values as an array of Strings.
   * @see #setFields(String[],Object[])
   */
  String[] getFields();

  /**
   * Returns the value of the specified field, or <code>null</code>
   * if no value is present for the given field name.
   *
   * @param name the field name.
   * @return the value of the field, or <code>null</code> if there
   *         is no value present.
   * @throws RuntimeOperationsException if the field name is illegal.
   */
  Object getFieldValue(String name);

  /**
   * Returns the values corresponding to the fields named in
   * the specified array, in the same order.  If an empty
   * array is supplied, an empty array is returned.  A value
   * of <code>null</code> leads to behaviour equivalent to
   * {@link #getFields()}.  Field values are obtained as specified
   * in {@link #getFieldValue(String)}, with <code>null</code>
   * being returned if the field is not present.  This applies
   * even if the given field name is <code>null</code> or
   * the empty string.
   *
   * @param names an array of field names whose values should
   *              be returned.
   * @return the values of the specified fields.
   * @see #getFields()
   * @see #getFieldValue(String)
   */
  Object[] getFieldValues(String... names);

  /**
   * <p>
   * Returns the hash code of the descriptor.  The hashcode
   * is computed as the sum of the hashcodes for each field,
   * which in turn is calculated as the sum of
   * the hashcode of the name, <code>n</code>, computed
   * using <code>n.toLowerCase().hashCode()</code>, and the
   * hashcode of the value, <code>v</code>, computed
   * using:
   * </p>
   * <ul>
   * <li>If <code>v</code> is <code>null</code>, then the
   * hash code is 0.</li>
   * <li>If <code>v</code> is a primitive array, then the
   * hash code is computed using the appropriate method
   * from {@link java.util.Arrays}.</li>
   * <li>If <code>v</code> is an {@link java.lang.Object}
   * array, then the hash code is computed using the
   * {@link java.util.Arrays#deepHashCode(Object[])} method.</li>
   * <li>Otherwise, the hashcode is equal to
   * <code>v.hashCode()</code>.
   * </ul>
   *
   * @return a hashcode for this descriptor.
   * @since 1.6
   * @see Object#equals(Object)
   * @see Object#hashCode()
   */
  int hashCode();

  /**
   * Returns true if all the fields have legal values, given
   * their names.  Validity is determined by the implementation.
   *
   * @return true if the values are legal.
   * @throws RuntimeOperationsException if the validity check
   *                                    fails for some reason.
   */
  boolean isValid()
    throws RuntimeOperationsException;

  /**
   * Removes a field from the descriptor.  If the field name
   * is illegal or not found, this method fails silently.
   *
   * @param name the name of the field to remove.
   * @throws RuntimeOperationsException if the field exists
   *                                    and the descriptor is
   *                                    immutable.  This wraps
   *                                    an {@link UnsupportedOperationException}.
   */
  void removeField(String name);

  /**
   * Attempts to set the specified field to the supplied
   * value.  If the field does not exist, it will be created.
   * If the field value given is invalid, then an exception will
   * be thrown.  Validity is determined by the implementation
   * of the descriptor.
   *
   * @param name the field to set.  Can not be <code>null</code>
   *             or empty.
   * @param value the value to use, the validity of which is
   *              determined by the implementation.
   * @throws RuntimeOperationsException if the name or value is
   *                                    illegal (wrapping a
   *                                    {@link IllegalArgumentException})
   *                                    or if the descriptor is
   *                                    immutable (wrapping a
   *                                    {@link UnsupportedOperationException}.
   */
  void setField(String name, Object value)
    throws RuntimeOperationsException;

  /**
   * Sets the field named in the first array to the corresponding
   * value specified in the second.  The array sizes must match.
   * Empty arrays have no effect.  An invalid value will cause
   * an exception to be thrown.
   *
   * @param names the names of the fields to change.  Neither
   *              the array or its elements may be <code>null</code>.
   * @param values the values to use.  The array must not be
   *               <code>null</code>.  The value of the elements
   *               depends on the validity constraints of the
   *               implementation.
   * @throws RuntimeOperationsException if the arrays differ in
   *                                    length, or a name or value is
   *                                    illegal (wrapping a
   *                                    {@link IllegalArgumentException})
   *                                    or if the descriptor is
   *                                    immutable (wrapping a
   *                                    {@link UnsupportedOperationException}.
   * @see #setField(String,Object)
   */
  void setFields(String[] names, Object[] values);

}
