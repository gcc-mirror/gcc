/* OpenType.java -- Superclass of all open types.
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

import java.io.Serializable;

import java.util.Arrays;
import java.util.List;

/**
 * The superclass of all open types, which describe the
 * applicable data values for open MBeans.  An open type
 * is defined by its name and description, and the name
 * of the Java class it maps to.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public abstract class OpenType<T>
  implements Serializable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -9195195325186646468L;

  /**
   * The name of the Java class this type represents.
   */
  private String className;

  /**
   * The name of this type.
   */
  private String typeName;

  /**
   * A description of this type.
   */
  private String description;

  /**
   * An array which defines the set of Java types which can be
   * used as open types.  Note that each type is also available
   * in array form, possibly with multiple dimensions.
   *
   * @deprecated Use {@link ALLOWED_CLASSNAMES_LIST} instead.
   */
  @Deprecated
  public static final String[] ALLOWED_CLASSNAMES = {
    "java.lang.Void",
    "java.lang.Boolean",
    "java.lang.Character",
    "java.lang.Byte",
    "java.lang.Short",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.String",
    "java.math.BigDecimal",
    "java.math.BigInteger",
    "java.util.Date",
    "javax.management.ObjectName",
    CompositeData.class.getName(),
    TabularData.class.getName()
  };

  /**
   * A list which defines the set of Java types that may be
   * used as open types.  Note that each type is also available
   * in array form, possibly with multiple dimensions.
   */
  public static final List<String> ALLOWED_CLASSNAMES_LIST =
    Arrays.asList(ALLOWED_CLASSNAMES);

  /**
   * Constructs a new {@link OpenType} for the specified class
   * with the given name and description.  The name of the class
   * must be taken from the list of {@link ALLOWED_CLASSNAMES}.
   * Arrays are implictly included in this, and follow the usual
   * syntax of {@link java.lang.Class#getName()} with the name
   * preceded by n instances of '[' (where n is the number of
   * dimensions) and an L.  The name and description can not be
   * <code>null</code> or the empty string.
   *
   * @param className the name of the Java class this type
   *                  represents.
   * @param name the name of the type.
   * @param desc the description of the type.
   * @throws IllegalArgumentException if either of <code>name</code>
   *                                  or <code>desc</code> are
   *                                  <code>null</code> or the empty
   *                                  string.
   * @throws OpenDataException if the class name does not reference
   *                           a listed class (from @{link ALLOWED_CLASSNAMES})
   */
  protected OpenType(String className, String name, String desc)
    throws OpenDataException
  {
    if (name == null || name.equals(""))
      throw new IllegalArgumentException("The name can not be null " +
                                         "or the empty string.");
    if (desc == null || desc.equals(""))
      throw new IllegalArgumentException("The description can not " +
                                         "be null or the empty string.");
    Class<?> type;
    try
      {
        type = Class.forName(className);
      }
    catch (ClassNotFoundException e)
      {
        throw (OpenDataException) new OpenDataException("The class name, " + className +
                                                        ", is unavailable.").initCause(e);
      }
    while (type.isArray())
      type = type.getComponentType();
    if (!(type.isPrimitive() || ALLOWED_CLASSNAMES_LIST.contains(type.getName())))
      throw new OpenDataException("The class name, " + className +
                                  ", does not specify a valid open type.");
    this.className = className;
    typeName = name;
    description = desc;
  }

  /**
   * Performs an equality test on this object and the one specified.
   *
   * @param obj the object to test against this one.
   * @return true if the two objects are equivalent.
   * @see java.lang.Object#hashCode()
   */
  public abstract boolean equals(Object obj);

  /**
   * Returns the name of the Java class this type represents.  This must
   * be one of the {@link ALLOWED_CLASSNAMES} or an array of one of them.
   * The specification of arrays follows the standard set by
   * {@link java.lang.Class#getName()} i.e. the name is the class name
   * preceded by n instances of '[' and an 'L', where n is number of
   * dimensions used by the array.
   *
   * @return the class name.
   */
  public String getClassName()
  {
    return className;
  }

  /**
   * Returns a description of this open type.
   *
   * @return the description.
   */
  public String getDescription()
  {
    return description;
  }

  /**
   * Returns the name of this open type.
   *
   * @return the type name.
   */
  public String getTypeName()
  {
    return typeName;
  }

  /**
   * Returns a hash code for this open type.  The hash code
   * should be consistent with the {@link equals()} method.
   * Thus, it should continue to return the same value while
   * the values used by the {@link equals()} method remain
   * the same, and should return different hash codes for
   * objects which are judged to be different using the
   * {@link equals()} method.
   *
   * @return the hash code of this instance.
   */
  public abstract int hashCode();

  /**
   * Returns true if this open type represents an array type.
   *
   * @return true if this open type represents an array type.
   */
  public boolean isArray()
  {
    return className.startsWith("[");
  }

  /**
   * Returns true if the specified object is a member of this
   * type.
   *
   * @param obj the object to test for membership.
   * @return true if the object is a member of this type.
   */
  public abstract boolean isValue(Object obj);

  /**
   * Returns a textual representation of this type.
   *
   * @return a {@link java.lang.String} representation of this
   *         type.
   */
  public abstract String toString();

}
