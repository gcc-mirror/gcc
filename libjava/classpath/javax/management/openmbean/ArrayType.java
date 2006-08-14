/* ArrayType.java -- Open type descriptor for an array.
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

import java.lang.reflect.Array;

import java.util.Arrays;

/**
 * The open type descriptor for arrays of open data values.
 * 
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ArrayType
  extends OpenType
{
  
  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 720504429830309770L;

  /**
   * The number of dimensions arrays of this type has.
   */
  private int dimension;

  /**
   * The element type of arrays of this type.
   */
  private OpenType elementType;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Returns the class name of the array, given the element
   * class name and its dimensions.
   *
   * @param className the name of the class used by the
   *                  array's elements.
   * @param dim the dimensions of the array.
   * @return the array's class name.
   */
  private static String getArrayClassName(String className, int dim)
  {
    char[] brackets = new char[dim];
    Arrays.fill(brackets, '[');
    return String.valueOf(brackets) + "L" + className;
  }

  /**
   * <p>
   * Constructs a new {@link ArrayType} instance for an array of the
   * specified type with the supplied number of dimensions.  The attributes
   * used by the superclass, {@link OpenType}, are automatically defined,
   * based on these values.  Both the class name and type name are set
   * to the value returned by the {@link java.lang.Class#getName()} of
   * the array's class (i.e. the element type, preceded by n instances of
   * '[' and an 'L', where n is the number of dimensions the array has).
   * The description is based upon the template <code>n-dimension array
   * of e</code>, where n is the number of dimensions of the array, and
   * e is the element type.  The class name of the actual elements is
   * obtainable by calling {@link OpenType#getClassName()} on the result
   * of {@link #getElementOpenType()}.
   * </p>
   * <p>
   * As an example, the array type returned by
   * <code>new ArrayType(6, SimpleType.INTEGER)</code> has the following
   * values:
   * </p>
   * <table>
   * <th><td>Attribute</td><td>Value</td></th>
   * <tr><td>Class Name</td><td><code>[[[[[[Ljava.lang.Integer;</code>
   * </td></tr>
   * <tr><td>Type Name</td><td><code>[[[[[[Ljava.lang.Integer;</code>
   * </td></tr>
   * <tr><td>Description</td><td><code>6-dimension array of
   * java.lang.Integer</code></td></tr>
   * <tr><td>Element Type Class Name</td><td><code>java.lang.Integer</code>
   * </td></tr>
   * </table>
   * <p>
   * The dimensions of the array must be equal to or greater than 1.  The
   * element type must be an instance of {@link SimpleType},
   * {@link CompositeType} or {@link TabularType}.
   * </p>
   *
   * @param dim the dimensions of the array.
   * @param elementType the type of the elements of the array.
   * @throws IllegalArgumentException if <code>dim</code> is less than 1.
   * @throws OpenDataException if the element type is not an instance of either
   *                           {@link SimpleType}, {@link CompositeType}
   *                           or {@link TabularType}.
   */
  public ArrayType(int dim, OpenType elementType)
    throws OpenDataException
  {
    super(getArrayClassName(elementType.getClassName(), dim),
	  getArrayClassName(elementType.getClassName(), dim),
	  dim + "-dimension array of " + elementType.getClassName());
    if (dim < 1)
      throw new IllegalArgumentException("Dimensions must be greater " +
					 "than or equal to 1.");
    if (!(elementType instanceof SimpleType ||
	  elementType instanceof CompositeType ||
	  elementType instanceof TabularType))
      throw new OpenDataException("The element type must be a simple " +
				  "type, a composite type or a tabular " +
				  "type.");
    dimension = dim;
    this.elementType = elementType;
  }

  /**
   * <p>
   * Compares this array type with another object
   * for equality.  The objects are judged to be equal if:
   * </p>
   * <ul>
   * <li><code>obj</code> is not null.</li>
   * <li><code>obj</code> is an instance of
   * {@link ArrayType}.</li>
   * <li>The dimensions are equal.</li>
   * <li>The element types are equal.</li>
   * </ul>
   * 
   * @param obj the object to compare with.
   * @return true if the conditions above hold.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof ArrayType))
      return false;
    ArrayType atype = (ArrayType) obj;
    return (atype.getDimension() == dimension &&
	    atype.getElementOpenType().equals(elementType));
  }

  /**
   * Returns the number of dimensions used by arrays
   * of this type.
   *
   * @return the number of dimensions.
   */
  public int getDimension()
  {
    return dimension;
  }

  /**
   * Returns the open type descriptor which describes
   * the type of the elements of this array type.
   *
   * @return the type of the elements.
   */
  public OpenType getElementOpenType()
  {
    return elementType;
  }

  /**
   * <p>
   * Returns the hash code of the array type.
   * This is computed as the sum of the hash code of the
   * element type together with the number of dimensions
   * the array has.  These are the same elements
   * of the type that are compared as part of the
   * {@link #equals(java.lang.Object)} method, thus ensuring
   * that the hashcode is compatible with the equality
   * test.
   * </p>
   * <p>
   * As instances of this class are immutable, the hash code
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hash code of this instance.
   */
  public int hashCode()
  {
    if (hashCode == null)
      hashCode = Integer.valueOf(dimension + elementType.hashCode());
    return hashCode.intValue();
  }

  /**
   * <p>
   * Returns true if the specified object is a member of this
   * array type.  The object is judged to be so if it is
   * non-null, an array and one of the following two conditions
   * holds:
   * </p>
   * <ul>
   * <li>This {@link ArrayType} instance has a {@link SimpleType}
   * as its element type.  Thus, the object must have the same
   * class name as that returned by {@link SimpleType#getClassName()}
   * for this class.</li>
   * <li>This {@link ArrayType} instance has a {@link CompositeType}
   * or a {@link TabularType} as its element type.  Thus, the object
   * must be assignable to such an array, and have elements which
   * are either null or valid values for the element type.</li>
   * </ul>
   *
   * @param obj the object to test for membership.
   * @return true if the object is a member of this type.
   */
  public boolean isValue(Object obj)
  {
    if (obj == null)
      return false;
    Class objClass = obj.getClass();
    if (!(objClass.isArray()))
      return false;
    if (elementType instanceof SimpleType)
      return getClassName().equals(objClass.getName());
    Class elementClass = null;
    try
      {
	elementClass = Class.forName(getClassName());
      }
    catch (ClassNotFoundException e)
      {
	throw new IllegalStateException("The array type's element " +
					"class could not be found.", e);
      }
    if (!(elementClass.isAssignableFrom(objClass)))
      return false;
    for (int a = 0; a < Array.getLength(obj); ++a)
      {
	Object elem = Array.get(obj, a);
	if (elem != null &&
	    (!(elementType.isValue(elem))))
	  return false;
      }
    return true;
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.ArrayType</code>)
   * and each element of the instance which is relevant to
   * the definition of {@link equals(java.lang.Object)} and
   * {@link hashCode()} (i.e. the type name, the number of
   * dimensions and the element type).
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
	+ "[name=" + getTypeName()
	+ ", dimension=" + dimension
	+ ", elementType=" + elementType
	+ "]";
    return string;
  }

}
