/* MBeanAttributeInfo.java -- Information about an attribute of a bean.
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

import java.lang.reflect.Method;
import java.lang.reflect.Type;

/**
 * Describes the attributes of a management bean.
 * The information in this class is immutable as standard.
 * Of course, subclasses may change this, but this
 * behaviour is not recommended.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanAttributeInfo
  extends MBeanFeatureInfo
  implements Cloneable
{

  /**
   * Compatible with JDK 1.6
   */
  private static final long serialVersionUID = 8644704819898565848L;

  /**
   * The type of the attribute.
   *
   * @serial the attribute type.
   */
  private String attributeType;

  /**
   * True if the attribute's value can be changed.
   *
   * @serial true if the value can be changed.
   */
  private boolean isWrite;

  /**
   * True if the attribute's value can be read.
   *
   * @serial true if the value can be read.
   */
  private boolean isRead;

  /**
   * True if the attribute is a boolean and thus
   * has a isXXX accessor rather than a getXXX accessor.
   *
   * @serial true if the attribute has an isXXX accessor.
   */
  private boolean is;

  /**
   * Constructs a new {@link MBeanAttributeInfo} using the specified
   * name and description, with the given accessor and mutator
   * methods.  A <code>null</code> value for the accessor method
   * indicates that the value can not be read.  A <code>null</code>
   * value for the mutator method indicates that the value can not be
   * changed.
   *
   * @param name the name of the attribute.
   * @param desc a description of the attribute.
   * @param getter the accessor method, or <code>null</code> if the value
   *               can not be read.
   * @param setter the mutator method, or <code>null</code> if the value
   *               can not be changed.
   * @throws IntrospectionException if both the accessor and mutator method
   *                                are <code>null</code>.
   */
  public MBeanAttributeInfo(String name, String desc, 
			    Method getter, Method setter)
    throws IntrospectionException
  {
    super(name, desc);
    if (getter == null && setter == null)
      throw new IntrospectionException("Both the getter and setter methods can " +
				       "not be null.");
    if (getter == null)
      {
	Type t = setter.getGenericParameterTypes()[0];
	if (t instanceof Class)
	  attributeType = ((Class) t).getName();
	else
	  attributeType = t.toString();
	isRead = false;
	is = false;
      }
    else
      {
	Type t = getter.getGenericReturnType();
	if (t instanceof Class)
	  attributeType = ((Class) t).getName();
	else
	  attributeType = t.toString();
	isRead = true;
	is = getter.getName().startsWith("is");
      }
    if (setter != null)
      isWrite = true;
  }

  /**
   * Constructs a new {@link MBeanAttributeInfo} using the specified
   * name, description and type with the given settings for the accessor
   * and mutator methods.  
   *
   * @param name the name of the attribute.
   * @param type the type of the attribute, in the form of its class name.
   * @param desc a description of the attribute.
   * @param isReadable true if the attribute's value can be read.
   * @param isWritable true if the attribute's value can be changed.
   * @param isIs true if the attribute uses an accessor of the form isXXX.
   * @throws IllegalArgumentException if the attribute is both unreadable
   *                                  and unwritable.
   */
  public MBeanAttributeInfo(String name, String type, String desc,
			    boolean isReadable, boolean isWritable,
			    boolean isIs)
  {
    super(name, desc);
    if (!isReadable && !isWritable)
      throw new IllegalArgumentException("The attribute can not be both " +
					 "unreadable and unwritable.");
    attributeType = type;
    isRead = isReadable;
    isWrite = isWritable;
    is = isIs;
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
   * Compares this feature with the supplied object.  This
   * returns true iff the object is an instance of
   * {@link MBeanAttributeInfo}, {@link Object#equals()}
   * returns true for a comparison of both the name and
   * description of this attribute  with that of the specified
   * object (performed by the superclass), and the type and
   * boolean flags of the two instances are equal.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link MBeanAttributeInfo}
   *         instance, 
   *         <code>name.equals(object.getName())</code>,
   *         <code>description.equals(object.getDescription())</code>,
   *         <code>attributeType.equals(object.getType())</code>,
   *         <code>isRead == object.isReadable()</code>,
   *         <code>isWrite == object.isWritable()</code>,
   *         <code>is == object.isIs()</code>
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof MBeanAttributeInfo))
      return false;
    if (!(super.equals(obj)))
      return false;
    MBeanAttributeInfo o = (MBeanAttributeInfo) obj;
    return (attributeType.equals(o.getType()) &&
	    isRead == o.isReadable() &&
	    isWrite == o.isWritable() &&
	    is == o.isIs());
  }

  /**
   * Returns the type of this attribute, in the form of its class name.
   *
   * @return the type of this attribute.
   */
  public String getType()
  {
    return attributeType;
  }

  /**
   * Returns the hashcode of the attribute information as the sum of
   * the hashcode of the superclass, the hashcode of the type,
   * the hashcode of {@link #isReadable()}, twice the hashcode
   * of {@link #isWritable()} and four times the hashcode
   * of {@link #isIs()}.
   *
   * @return the hashcode of the attribute information.
   */
  public int hashCode()
  {
    return super.hashCode() + attributeType.hashCode()
      + Boolean.valueOf(isRead).hashCode()
      + (2 * Boolean.valueOf(isWrite).hashCode())
      + (4 * Boolean.valueOf(is).hashCode());
  }

  /**
   * Returns true if the accessor method of this attribute
   * is of the form <code>isXXX</code>.
   *
   * @return true if the accessor takes the form <code>isXXX</code>.
   */
  public boolean isIs()
  {
    return is;
  }

  /**
   * Returns true if value of this attribute can be read.
   *
   * @return true if the value of the attribute can be read.
   */
  public boolean isReadable()
  {
    return isRead;
  }

  /**
   * Returns true if the value of this attribute can be changed.
   *
   * @return true if the value of the attribute can be changed.
   */
  public boolean isWritable()
  {
    return isWrite;
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.MBeanAttributeInfo</code>),
   * the name, description and type of the attribute and the 
   * current settings of the {@link #isReadable()}, 
   * {@link #isWritable()} and {@link #isIs()} properties.
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
	  + ",type=" + attributeType
	  + ",isReadable=" + (isRead ? "yes" : "no")
	  + ",isWritable=" + (isWrite ? "yes" : "no")
	  + ",isIs=" + (is ? "yes" : "no")
	  + "]";
      }
    return string;
  }

}
