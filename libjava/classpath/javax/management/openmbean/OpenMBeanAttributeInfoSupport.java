/* OpenMBeanAttributeInfoSupport.java -- Open typed info about an attribute.
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.management.MBeanAttributeInfo;

/**
 * Describes an attribute of an open management bean.  
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class OpenMBeanAttributeInfoSupport
  extends MBeanAttributeInfo
  implements OpenMBeanAttributeInfo
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -4867215622149721849L;

  /**
   * The open type of the attribute.
   */
  private OpenType<?> openType;

  /**
   * The default value of the attribute (may be <code>null</code>).
   */
  private Object defaultValue;

  /**
   * The possible legal values of the attribute (may be <code>null</code>).
   */
  private Set<?> legalValues;

  /**
   * The minimum value of the attribute (may be <code>null</code>).
   */
  private Comparable<?> minValue;

  /**
   * The maximum value of the attribute (may be <code>null</code>).
   */
  private Comparable<?> maxValue;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Constructs a new {@link OpenMBeanAttributeInfo} using the
   * specified name, description, open type and access properties.
   * The name, description and open type may not be <code>null</code>
   * and the name and description may not be equal to the empty
   * string.
   *
   * @param name the name of the attribute.
   * @param desc a description of the attribute.
   * @param type the open type of the attribute.
   * @param isReadable true if the attribute's value can be read.
   * @param isWritable true if the attribute's value can be changed.
   * @param isIs true if the attribute uses an accessor of the form isXXX.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   */
  public OpenMBeanAttributeInfoSupport(String name, String desc, OpenType<?> type,
				       boolean isReadable, boolean isWritable,
				       boolean isIs)
  {
    super(name, type == null ? null : type.getClassName(), desc, isReadable,
	  isWritable, isIs);
    if (name == null)
      throw new IllegalArgumentException("The name may not be null.");
    if (desc == null)
      throw new IllegalArgumentException("The description may not be null.");
    if (type == null)
      throw new IllegalArgumentException("The type may not be null.");
    if (name.length() == 0)
      throw new IllegalArgumentException("The name may not be the empty string.");
    if (desc.length() == 0)
      throw new IllegalArgumentException("The description may not be the " +
					 "empty string.");
  }

  /**
   * Constructs a new {@link OpenMBeanAttributeInfo} using the
   * specified name, description, open type and default value.  The
   * name, description and open type cannot be <code>null</code> and
   * the name and description may not be equal to the empty string.
   * The default value may be <code>null</code>.  If non-null, it must
   * be a valid value of the given open type.  Default values are not
   * applicable to the open types, {@link ArrayType} and {@link
   * TabularType}.
   *
   * @param name the name of the attribute.
   * @param desc a description of the attribute.
   * @param type the open type of the attribute.
   * @param isReadable true if the attribute's value can be read.
   * @param isWritable true if the attribute's value can be changed.
   * @param isIs true if the attribute uses an accessor of the form isXXX.
   * @param defaultValue the default value of the attribute.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   * @throws OpenDataException if <code>defaultValue<code> is non-null
   *                           and is either not a value of the given
   *                           open type or the open type is an instance
   *                           of {@link ArrayType} or {@link TabularType}.
   */
  public <T> OpenMBeanAttributeInfoSupport(String name, String desc, OpenType<T> type,
					   boolean isReadable, boolean isWritable,
					   boolean isIs, T defaultValue)
    throws OpenDataException
  {
    this(name, desc, type, isReadable, isWritable, isIs, defaultValue, null);
  }

  /**
   * <p>
   * Constructs a new {@link OpenMBeanAttributeInfo} using the
   * specified name, description, open type, access properties, 
   * default, maximum and minimum values.  The name, description
   * and open type cannot be <code>null</code> and the name and
   * description may not be equal to the empty string.  The
   * default, maximum and minimum values may be <code>null</code>.
   * The following conditions apply when the attributes mentioned
   * are non-null:
   * </p>
   * <ul>
   * <li>The values must be valid values for the given open type.</li>
   * <li>Default values are not applicable to the open types, {@link
   * ArrayType} and {@link TabularType}.</li>
   * <li>The minimum value must be smaller than or equal to the maximum value
   * (literally, <code>minValue.compareTo(maxValue) <= 0</code>.</li>
   * <li>The minimum value must be smaller than or equal to the default value
   * (literally, <code>minValue.compareTo(defaultValue) <= 0</code>.</li>
   * <li>The default value must be smaller than or equal to the maximum value
   * (literally, <code>defaultValue.compareTo(maxValue) <= 0</code>.</li>
   * </ul>
   * 
   * @param name the name of the attribute.
   * @param desc a description of the attribute.
   * @param type the open type of the attribute.
   * @param isReadable true if the attribute's value can be read.
   * @param isWritable true if the attribute's value can be changed.
   * @param isIs true if the attribute uses an accessor of the form isXXX.
   * @param defaultValue the default value of the attribute, or <code>null</code>.
   * @param minimumValue the minimum value of the attribute, or <code>null</code>.
   * @param maximumValue the maximum value of the attribute, or <code>null</code>.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   * @throws OpenDataException if any condition in the list above is broken.
   */
  @SuppressWarnings("unchecked")
  public <T> OpenMBeanAttributeInfoSupport(String name, String desc, OpenType<T> type,
					   boolean isReadable, boolean isWritable,
					   boolean isIs, T defaultValue,
					   Comparable<T> minimumValue,
					   Comparable<T> maximumValue)
    throws OpenDataException
  {
    this(name, desc, type, isReadable, isWritable, isIs);
    if (defaultValue != null && !(type.isValue(defaultValue)))
      throw new OpenDataException("The default value is not a member of the " +
				  "open type given.");
    if (minimumValue != null && !(type.isValue(minimumValue)))
      throw new OpenDataException("The minimum value is not a member of the " +
				  "open type given.");
    if (maximumValue != null && !(type.isValue(maximumValue)))
      throw new OpenDataException("The maximum value is not a member of the " +
				  "open type given.");
    if (defaultValue != null && (type instanceof ArrayType || 
				 type instanceof TabularType))
      throw new OpenDataException("Default values are not applicable for " +
				  "array or tabular types.");
    if (minimumValue != null && maximumValue != null 
	&& minimumValue.compareTo((T) maximumValue) > 0)
      throw new OpenDataException("The minimum value is greater than the " +
				  "maximum.");
    if (minimumValue != null && defaultValue != null 
	&& minimumValue.compareTo(defaultValue) > 0)
      throw new OpenDataException("The minimum value is greater than the " +
				  "default.");
    if (defaultValue != null && maximumValue != null
	&& maximumValue.compareTo(defaultValue) < 0)
      throw new OpenDataException("The default value is greater than the " +
				  "maximum.");
    
    openType = type;
    this.defaultValue = defaultValue;
    minValue = minimumValue;
    maxValue = maximumValue;
  }

  /**
   * <p>
   * Constructs a new {@link OpenMBeanAttributeInfo} using the
   * specified name, description, open type, access properties, default
   * value and set of legal values.  The name, description and open type
   * cannot be <code>null</code> and the name and description may not be
   * equal to the empty string.  The default, maximum and minimum values
   * may be <code>null</code>.  The following conditions apply when the
   * attributes mentioned are non-null:
   * </p>
   * <ul>
   * <li>The default value and each of the legal values must be a valid
   * value for the given open type.</li>
   * <li>Default and legal values are not applicable to the open types, {@link
   * ArrayType} and {@link TabularType}.</li>
   * <li>The default value is not in the set of legal values.</li>
   * </ul>
   * <p>
   * The legal values are copied from the array into a unmodifiable set,
   * so future modifications to the array have no effect.
   * </p>
   *
   * @param name the name of the attribute.
   * @param desc a description of the attribute.
   * @param type the open type of the attribute.
   * @param isReadable true if the attribute's value can be read.
   * @param isWritable true if the attribute's value can be changed.
   * @param isIs true if the attribute uses an accessor of the form isXXX.
   * @param defaultValue the default value of the attribute, or <code>null</code>.
   * @param legalValues the legal values of the attribute.  May be
   *                    <code>null</code> or an empty array.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   * @throws OpenDataException if any condition in the list above is broken.
   */
  public <T> OpenMBeanAttributeInfoSupport(String name, String desc, OpenType<T> type,
					   boolean isReadable, boolean isWritable,
					   boolean isIs, T defaultValue,
					   T[] legalValues)
    throws OpenDataException
  {
    this(name, desc, type, isReadable, isWritable, isIs);
    if (defaultValue != null && !(type.isValue(defaultValue)))
      throw new OpenDataException("The default value is not a member of the " +
				  "open type given.");
    if (defaultValue != null && (type instanceof ArrayType || 
				 type instanceof TabularType))
      throw new OpenDataException("Default values are not applicable for " +
				  "array or tabular types.");
    if (legalValues != null && (type instanceof ArrayType || 
				type instanceof TabularType))
      throw new OpenDataException("Legal values are not applicable for " +
				  "array or tabular types.");
    if (legalValues != null && legalValues.length > 0)
      {
	Set<T> lv = new HashSet<T>(legalValues.length);
	for (int a = 0; a < legalValues.length; ++a)
	  {
	    if (legalValues[a] != null && 
		!(type.isValue(legalValues[a])))
	      throw new OpenDataException("The legal value, " 
					  + legalValues[a] + 
					  "is not a member of the " +
					  "open type given.");
	    lv.add(legalValues[a]);
	  }
	if (defaultValue != null && !(lv.contains(defaultValue)))
	  throw new OpenDataException("The default value is not in the set " +
				      "of legal values.");
	this.legalValues = Collections.unmodifiableSet(lv);
      }
    openType = type;
    this.defaultValue = defaultValue;
  }

  /**
   * Compares this attribute with the supplied object.  This returns
   * true iff the object is an instance of {@link OpenMBeanAttributeInfo}
   * with an equal name and open type and the same default, minimum,
   * maximum and legal values and the same access properties.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link OpenMBeanAttributeInfo}
   *         instance, 
   *         <code>name.equals(object.getName())</code>,
   *         <code>openType.equals(object.getOpenType())</code>,
   *         <code>isRead == object.isReadable()</code>,
   *         <code>isWrite == object.isWritable()</code>,
   *         <code>isIs == object.isIs()</code>,
   *         <code>defaultValue.equals(object.getDefaultValue())</code>,
   *         <code>minValue.equals(object.getMinValue())</code>,
   *         <code>maxValue.equals(object.getMaxValue())</code>,
   *         and <code>legalValues.equals(object.getLegalValues())</code>.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof OpenMBeanAttributeInfo))
      return false;
    OpenMBeanAttributeInfo o = (OpenMBeanAttributeInfo) obj;
    return getName().equals(o.getName()) &&
      openType.equals(o.getOpenType()) &&
      isReadable() == o.isReadable() &&
      isWritable() == o.isWritable() &&
      isIs() == o.isIs() &&
      (defaultValue == null ? o.getDefaultValue() == null :
       defaultValue.equals(o.getDefaultValue())) &&
      (minValue == null ? o.getMinValue() == null :
       minValue.equals(o.getMinValue())) &&
      (maxValue == null ? o.getMaxValue() == null :
       maxValue.equals(o.getMaxValue())) &&
      (legalValues == null ? o.getLegalValues() == null :
       legalValues.equals(o.getLegalValues()));
  }

  /**
   * Returns the default value of this attribute, or <code>null</code>
   * if there is no default value.
   *
   * @return the default value of the attribute, or <code>null</code>
   *         if there is no default.
   */
  public Object getDefaultValue()
  {
    return defaultValue;
  }

  /**
   * Returns a {@link java.util.Set} enumerating the legal values
   * of this attribute, or <code>null</code> if no such limited
   * set exists for this attribute.
   *
   * @return a set of legal values, or <code>null</code> if no such
   *         set exists.
   */
  public Set<?> getLegalValues()
  {
    return legalValues;
  }

  /**
   * Returns the maximum value of this attribute, or <code>null</code>
   * if there is no maximum.
   *
   * @return the maximum value, or <code>null</code> if none exists.
   */
  public Comparable<?> getMaxValue()
  {
    return maxValue;
  }

  /**
   * Returns the minimum value of this attribute, or <code>null</code>
   * if there is no minimum.
   *
   * @return the minimum value, or <code>null</code> if none exists.
   */
  public Comparable<?> getMinValue()
  {
    return minValue;
  }

  /**
   * Returns the open type instance which represents the type of this
   * attribute.
   *
   * @return the open type of this attribute.
   */
  public OpenType<?> getOpenType()
  {
    return openType;
  }

  /**
   * Returns true if this attribute has a default value
   * (i.e. the value is non-null).
   *
   * @return true if this attribute has a default.
   */
  public boolean hasDefaultValue()
  {
    return defaultValue != null;
  }

  /**
   * <p>
   * Returns the hashcode of the attribute information as the sum of
   * the hashcodes of the name, open type, default value, maximum
   * value, minimum value and the set of legal values.
   * </p>
   * <p>
   * As instances of this class are immutable, the hash code
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hashcode of the attribute information.
   */
  public int hashCode()
  {
    if (hashCode == null)
      hashCode = Integer.valueOf(getName().hashCode() + 
				 openType.hashCode() +
				 Boolean.valueOf(isReadable()).hashCode() +
				 (2 * 
				  Boolean.valueOf(isWritable()).hashCode()) +
				 (4 * Boolean.valueOf(isIs()).hashCode()) +
				 (defaultValue == null ? 0 :
				  defaultValue.hashCode()) +
				 (minValue == null ? 0 :
				  minValue.hashCode()) +
				 (maxValue == null ? 0 :
				  maxValue.hashCode()) +
				 (legalValues == null ? 0 :
				  legalValues.hashCode()));
    return hashCode.intValue();
  }

  /**
   * Returns true if there is a set of legal values for this
   * attribute (i.e. the value is non-null).
   *
   * @return true if a set of legal values exists for this
   *         attribute.
   */
  public boolean hasLegalValues()
  {
    return legalValues != null;
  }

  /**
   * Returns true if there is a maximum value for this attribute
   * (i.e. the value is non-null).
   *
   * @return true if a maximum value exists for this attribute.
   */
  public boolean hasMaxValue()
  {
    return maxValue != null;
  }

  /**
   * Returns true if there is a minimum value for this attribute.
   * (i.e. the value is non-null).
   *
   * @return true if a minimum value exists for this attribute.
   */
  public boolean hasMinValue()
  {
    return minValue != null;
  }

  /**
   * Returns true if the specified object is a valid value for
   * this attribute.
   *
   * @param obj the object to test.
   * @return true if <code>obj</code> is a valid value for this
   *         attribute.
   */
  public boolean isValue(Object obj)
  {
    return openType.isValue(obj);
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanAttributeInfo</code>)
   * along with the name, open type, access properties, default,
   * minimum, maximum  and legal values of the attribute.
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
	+ "[name=" + getName() 
	+ ",openType=" + openType
	+ ",isReadable=" + isReadable()
	+ ",isWritable=" + isWritable()
	+ ",isIs=" + isIs()
	+ ",defaultValue=" + defaultValue
	+ ",minValue=" + minValue
	+ ",maxValue=" + maxValue
	+ ",legalValues=" + legalValues
	+ "]";
    return string;
  }

}
