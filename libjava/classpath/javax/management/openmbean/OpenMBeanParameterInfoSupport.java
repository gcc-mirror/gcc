/* OpenMBeanParameterInfoSupport.java -- Open typed info about a parameter.
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

import javax.management.MBeanParameterInfo;

/**
 * Describes the parameters of a constructor or operation associated
 * with an open management bean.  
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class OpenMBeanParameterInfoSupport
  extends MBeanParameterInfo
  implements OpenMBeanParameterInfo
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -7235016873758443122L;

  /**
   * The open type of the parameter.
   */
  private OpenType<?> openType;

  /**
   * The default value of the parameter (may be <code>null</code>).
   */
  private Object defaultValue;

  /**
   * The possible legal values of the parameter (may be <code>null</code>).
   */
  private Set<?> legalValues;

  /**
   * The minimum value of the parameter (may be <code>null</code>).
   */
  private Comparable<Object> minValue;

  /**
   * The maximum value of the parameter (may be <code>null</code>).
   */
  private Comparable<Object> maxValue;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Constructs a new {@link OpenMBeanParameterInfo} using the specified
   * name, description and open type.  None of these values may be
   * <code>null</code> and the name and description may not be equal
   * to the empty string.
   *
   * @param name the name of the parameter.
   * @param desc a description of the parameter.
   * @param type the open type of the parameter.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   */
  public OpenMBeanParameterInfoSupport(String name, String desc, OpenType<?> type)
  {
    super(name, type == null ? null : type.getClassName(), desc);
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
    openType = type;
  }

  /**
   * Constructs a new {@link OpenMBeanParameterInfo} using the
   * specified name, description, open type and default value.  The
   * name, description and open type cannot be <code>null</code> and
   * the name and description may not be equal to the empty string.
   * The default value may be <code>null</code>.  If non-null, it must
   * be a valid value of the given open type.  Default values are not
   * applicable to the open types, {@link ArrayType} and {@link
   * TabularType}.
   *
   * @param name the name of the parameter.
   * @param desc a description of the parameter.
   * @param type the open type of the parameter.
   * @param defaultValue the default value of the parameter.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   * @throws OpenDataException if <code>defaultValue<code> is non-null
   *                           and is either not a value of the given
   *                           open type or the open type is an instance
   *                           of {@link ArrayType} or {@link TabularType}.
   */
  public <T> OpenMBeanParameterInfoSupport(String name, String desc, OpenType<T> type,
					   T defaultValue)
    throws OpenDataException
  {
    this(name, desc, type, defaultValue, null);
  }

  /**
   * <p>
   * Constructs a new {@link OpenMBeanParameterInfo} using the
   * specified name, description, open type, default, maximum and
   * minimum values.  The name, description and open type cannot be
   * <code>null</code> and the name and description may not be equal
   * to the empty string.  The default, maximum and minimum values may
   * be <code>null</code>.  The following conditions apply when the
   * parameters mentioned are non-null:
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
   * @param name the name of the parameter.
   * @param desc a description of the parameter.
   * @param type the open type of the parameter.
   * @param defaultValue the default value of the parameter, or <code>null</code>.
   * @param minimumValue the minimum value of the parameter, or <code>null</code>.
   * @param maximumValue the maximum value of the parameter, or <code>null</code>.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   * @throws OpenDataException if any condition in the list above is broken.
   */
  public <T> OpenMBeanParameterInfoSupport(String name, String desc, OpenType<T> type,
					   T defaultValue, Comparable<T> minimumValue,
					   Comparable<T> maximumValue)
    throws OpenDataException
  {
    this(name, desc, type);
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
    if (minValue != null && maxValue != null 
	&& minValue.compareTo(maxValue) > 0)
      throw new OpenDataException("The minimum value is greater than the " +
				  "maximum.");
    if (minValue != null && defaultValue != null 
	&& minValue.compareTo(defaultValue) > 0)
      throw new OpenDataException("The minimum value is greater than the " +
				  "default.");
    if (defaultValue != null && maxValue != null
	&& maxValue.compareTo(defaultValue) < 0)
      throw new OpenDataException("The default value is greater than the " +
				  "maximum.");
    
    this.defaultValue = defaultValue;
    minValue = (Comparable<Object>) minimumValue;
    maxValue = (Comparable<Object>) maximumValue;
  }

  /**
   * <p>
   * Constructs a new {@link OpenMBeanParameterInfo} using the
   * specified name, description, open type, default value and
   * set of legal values.  The name, description and open type cannot be
   * <code>null</code> and the name and description may not be equal
   * to the empty string.  The default, maximum and minimum values may
   * be <code>null</code>.  The following conditions apply when the
   * parameters mentioned are non-null:
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
   * @param name the name of the parameter.
   * @param desc a description of the parameter.
   * @param type the open type of the parameter.
   * @param defaultValue the default value of the parameter, or <code>null</code>.
   * @param legalValues the legal values of the parameter.  May be
   *                    <code>null</code> or an empty array.
   * @throws IllegalArgumentException if the name, description or
   *                                  open type are <code>null</code>
   *                                  or the name or description are
   *                                  the empty string.
   * @throws OpenDataException if any condition in the list above is broken.
   */
  public <T> OpenMBeanParameterInfoSupport(String name, String desc, OpenType<T> type,
					   T defaultValue, T[] legalValues)
    throws OpenDataException
  {
    this(name, desc, type);
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
	Set lv = new HashSet(legalValues.length);
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
    this.defaultValue = defaultValue;
  }

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
  public boolean equals(Object obj)
  {
    if (!(obj instanceof OpenMBeanParameterInfo))
      return false;
    OpenMBeanParameterInfo o = (OpenMBeanParameterInfo) obj;
    return getName().equals(o.getName()) &&
      openType.equals(o.getOpenType()) &&
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
   * Returns the default value of this parameter, or <code>null</code>
   * if there is no default value.
   *
   * @return the default value of the parameter, or <code>null</code>
   *         if there is no default.
   */
  public Object getDefaultValue()
  {
    return defaultValue;
  }

  /**
   * Returns a {@link java.util.Set} enumerating the legal values
   * of this parameter, or <code>null</code> if no such limited
   * set exists for this parameter.
   *
   * @return a set of legal values, or <code>null</code> if no such
   *         set exists.
   */
  public Set<?> getLegalValues()
  {
    return legalValues;
  }

  /**
   * Returns the maximum value of this parameter, or <code>null</code>
   * if there is no maximum.
   *
   * @return the maximum value, or <code>null</code> if none exists.
   */
  public Comparable<?> getMaxValue()
  {
    return maxValue;
  }

  /**
   * Returns the minimum value of this parameter, or <code>null</code>
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
   * parameter.
   *
   * @return the open type of this parameter.
   */
  public OpenType<?> getOpenType()
  {
    return openType;
  }

  /**
   * Returns true if this parameter has a default value
   * (i.e. the value is non-null).
   *
   * @return true if this parameter has a default.
   */
  public boolean hasDefaultValue()
  {
    return defaultValue != null;
  }

  /**
   * <p>
   * Returns the hashcode of the parameter information as the sum of
   * the hashcodes of the name, open type, default value, maximum
   * value, minimum value and the set of legal values.
   * </p>
   * <p>
   * As instances of this class are immutable, the hash code
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hashcode of the parameter information.
   */
  public int hashCode()
  {
    if (hashCode == null)
      hashCode = Integer.valueOf(getName().hashCode() + 
				 openType.hashCode() +
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
   * parameter (i.e. the value is non-null).
   *
   * @return true if a set of legal values exists for this
   *         parameter.
   */
  public boolean hasLegalValues()
  {
    return legalValues != null;
  }

  /**
   * Returns true if there is a maximum value for this parameter
   * (i.e. the value is non-null).
   *
   * @return true if a maximum value exists for this parameter.
   */
  public boolean hasMaxValue()
  {
    return maxValue != null;
  }

  /**
   * Returns true if there is a minimum value for this parameter.
   * (i.e. the value is non-null).
   *
   * @return true if a minimum value exists for this parameter.
   */
  public boolean hasMinValue()
  {
    return minValue != null;
  }

  /**
   * Returns true if the specified object is a valid value for
   * this parameter.
   *
   * @param obj the object to test.
   * @return true if <code>obj</code> is a valid value for this
   *         parameter.
   */
  public boolean isValue(Object obj)
  {
    return openType.isValue(obj);
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanParameterInfo</code>)
   * along with the name, open type, default, minimum, maximum
   * and legal values of the parameter.
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
	+ ",defaultValue=" + defaultValue
	+ ",minValue=" + minValue
	+ ",maxValue=" + maxValue
	+ ",legalValues=" + legalValues
	+ "]";
    return string;
  }

}
