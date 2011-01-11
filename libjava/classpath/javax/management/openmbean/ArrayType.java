/* ArrayType.java -- Open type descriptor for an array.
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

import java.lang.reflect.Array;

import java.util.HashMap;
import java.util.Map;

/**
 * The open type descriptor for arrays of open data values.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ArrayType<T>
  extends OpenType<T>
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
  private OpenType<?> elementType;

  /**
   * True if this type represents a primitive array.
   */
  private boolean primitiveArray;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * A cache of {@link ArrayType} instances created
   * by {@link #getArrayType(OpenType)}.
   */
  private static final Map<OpenType<?>,ArrayType<?>> cache =
    new HashMap<OpenType<?>,ArrayType<?>>();

  /**
   * A cache of {@link ArrayType} instances created
   * by {@link #getPrimitiveArrayType(Class)}.
   */
  private static final Map<Class<?>,ArrayType<?>> primCache =
    new HashMap<Class<?>,ArrayType<?>>();

  /**
   * Returns the class name of the array, given the element
   * class name and its dimensions.
   *
   * @param elementType the type of the array's elements.
   * @param dim the dimensions of the array.
   * @param primitive true if this should be a primitive array.
   * @return the array's class name.
   * @throws OpenDataException if the class name does not reference
   *                           a loadable class.
   */
  private static final String getArrayClassName(OpenType<?> elementType,
                                                int dim,
                                                boolean primitive)
    throws OpenDataException
  {
    Class<?> type;
    if (primitive)
      type = getPrimitiveTypeClass((SimpleType<?>) elementType);
    else
      {
        String className = elementType.getClassName();
        try
          {
            type = Class.forName(className);
          }
        catch (ClassNotFoundException e)
          {
            throw new OpenDataException("The class name, " + className +
                                        ", is unavailable.");
          }
      }
    while (type.isArray())
      type = type.getComponentType();
    return
      Array.newInstance(type,
                        new int[getDimensions(elementType, dim)]).getClass().getName();
  }

  /**
   * Returns the dimensions of the new {@link ArrayType},
   * based on whether the given element type is already an
   * {@link ArrayType} or not.
   *
   * @param elementType the type of the array.
   * @param dim the proposed dimensions.
   * @return the resultant dimensions.
   * @throws IllegalArgumentException if <code>dim</code> is less than 1.
   */
  private static final int getDimensions(OpenType<?> elementType,
                                         int dim)
  {
    if (dim < 1)
      throw new IllegalArgumentException("Dimensions must be greater " +
                                         "than or equal to 1.");
    if (elementType instanceof ArrayType)
      return dim + ((ArrayType<?>) elementType).getDimension();
    return dim;
  }

  /**
   * Returns the appropriate primitive type name, given the
   * corresponding wrapper class.
   *
   * @param type the type to convert.
   * @return the corresponding primitive type.
   * @throws OpenDataException if {@code type} is not a valid
   *                           {@link Class} for a primitive type.
   *
   */
  private static final SimpleType<?> getPrimitiveType(Class<?> type)
    throws OpenDataException
  {
    if (type.equals(Boolean.TYPE))
      return SimpleType.BOOLEAN;
    if (type.equals(Byte.TYPE))
      return SimpleType.BYTE;
    if (type.equals(Character.TYPE))
      return SimpleType.CHARACTER;
    if (type.equals(Double.TYPE))
      return SimpleType.DOUBLE;
    if (type.equals(Float.TYPE))
      return SimpleType.FLOAT;
    if (type.equals(Integer.TYPE))
      return SimpleType.INTEGER;
    if (type.equals(Long.TYPE))
      return SimpleType.LONG;
    if (type.equals(Short.TYPE))
      return SimpleType.SHORT;
    if (type.equals(Void.TYPE))
      return SimpleType.VOID;
    throw new OpenDataException(type + " is not a primitive type.");
  }

  /**
   * Returns the appropriate primitive type name, given the
   * corresponding wrapper class.
   *
   * @param type the type to convert.
   * @return the corresponding primitive type.
   * @throws OpenDataException if {@code type} is not a valid
   *                           {@link SimpleType} for a primitive type.
   *
   */
  private static final Class<?> getPrimitiveTypeClass(SimpleType<?> type)
    throws OpenDataException
  {
    if (type.equals(SimpleType.BOOLEAN))
      return Boolean.TYPE;
    if (type.equals(SimpleType.BYTE))
      return Byte.TYPE;
    if (type.equals(SimpleType.CHARACTER))
      return Character.TYPE;
    if (type.equals(SimpleType.DOUBLE))
      return Double.TYPE;
    if (type.equals(SimpleType.FLOAT))
      return Float.TYPE;
    if (type.equals(SimpleType.INTEGER))
      return Integer.TYPE;
    if (type.equals(SimpleType.LONG))
      return Long.TYPE;
    if (type.equals(SimpleType.SHORT))
      return Short.TYPE;
    if (type.equals(SimpleType.VOID))
      return Void.TYPE;
    throw new OpenDataException(type + " is not a primitive type.");
  }

  /**
   * Returns the element type that will actually be used, if the
   * specified element type is passed to a constructor.  This is
   * necessary to ensure that a non-array type is still returned when
   * an {@link ArrayType} is constructed from an {@link ArrayType}.
   *
   * @param elemType the element type that was supplied.
   * @return the element type that will be used.
   */
  private static final OpenType<?> getElementType(OpenType<?> elemType)
  {
    if (elemType instanceof ArrayType)
      return ((ArrayType<?>) elemType).getElementOpenType();
    return elemType;
  }

  /**
   * Returns the element type name that will actually be used, if the
   * specified element type is passed to a constructor.  This is
   * necessary to ensure that a non-array type is still returned when
   * an {@link ArrayType} is constructed from an {@link ArrayType},
   * and that primitive arrays are described correctly.
   *
   * @param elemType the element type that was supplied.
   * @return the element type name that will be used.
   * @throws OpenDataException if the element type is not a valid
   *                           {@link SimpleType} for a primitive type.
   */
  private static final String getElementTypeName(OpenType<?> elemType)
    throws OpenDataException
  {
    OpenType<?> trueElemType = getElementType(elemType);
    if (elemType instanceof ArrayType &&
        ((ArrayType<?>) elemType).isPrimitiveArray())
      return getPrimitiveTypeClass((SimpleType<?>) trueElemType).getName();
    return trueElemType.getClassName();
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
  public ArrayType(int dim, OpenType<?> elementType)
    throws OpenDataException
  {
    super(getArrayClassName(elementType, dim, false),
          getArrayClassName(elementType, dim, false),
          getDimensions(elementType, dim) + "-dimension array of "
          + getElementTypeName(elementType));
    if (!(elementType instanceof SimpleType ||
          elementType instanceof CompositeType ||
          elementType instanceof TabularType ||
          elementType instanceof ArrayType))
      throw new OpenDataException("The element type must be a simple " +
                                  "type, an array type, a composite type " +
                                  "or a tabular type.");
    dimension = getDimensions(elementType, dim);
    this.elementType = getElementType(elementType);
    primitiveArray = (elementType instanceof ArrayType &&
                      ((ArrayType<?>) elementType).isPrimitiveArray());
  }

  /**
   * <p>
   * Constructs a new {@link ArrayType} instance for a unidimensional
   * array of the specified {@link SimpleType}.  The attributes
   * used by the superclass, {@link OpenType}, are automatically defined,
   * based on these values.  Both the class name and type name are set
   * to the value returned by the {@link java.lang.Class#getName()} of
   * the array's class.  If the array is of a primitive type (indicated
   * by giving {@code primitiveArray} the value {@code true}), the
   * name will be '[' followed by the appropriate letter for the
   * primitive type (see {@link java.lang.Class#getName()}).  If the
   * array is not of a primitive type, then the name is formed from
   * the element type, preceded by '[' and an 'L', in the same way
   * as when the multi-dimensional constructor is used.
   * </p>
   * <p>
   * The description is based upon the template <code>1-dimension array
   * of e</code>, where e is either the primitive type or a class name,
   * depending on whether the array itself is of a primitive type or not.
   * The class name of the actual elements is obtainable by calling
   * {@link OpenType#getClassName()} on the result of
   * {@link #getElementOpenType()}.  This will be the appropriate wrapper
   * class for a primitive type.
   * </p>
   * <p>
   * As an example, the array type returned by
   * <code>new ArrayType(SimpleType.INTEGER, true)</code> has the following
   * values:
   * </p>
   * <table>
   * <th><td>Attribute</td><td>Value</td></th>
   * <tr><td>Class Name</td><td><code>[I</code>
   * </td></tr>
   * <tr><td>Type Name</td><td><code>[I</code>
   * </td></tr>
   * <tr><td>Description</td><td><code>1-dimension array of int</code></td></tr>
   * <tr><td>Element Type Class Name</td><td><code>java.lang.Integer</code>
   * </td></tr>
   * </table>
   *
   * @param elementType the type of the elements of the array.
   * @param primitiveArray true if the array should be of a primitive type.
   * @throws OpenDataException if {@code primitiveArray} is {@code true},
   *                           and {@link elementType} is not a valid
   *                           {@link SimpleType} for a primitive type.
   * @since 1.6
   */
  public ArrayType(SimpleType<?> elementType, boolean primitiveArray)
    throws OpenDataException
  {
    super(getArrayClassName(elementType, 1, primitiveArray),
          getArrayClassName(elementType, 1, primitiveArray),
          "1-dimension array of " +
          (primitiveArray ? getPrimitiveTypeClass(elementType).getName()
           : elementType.getClassName()));
    dimension = 1;
    this.elementType = elementType;
    this.primitiveArray = primitiveArray;
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
   * <li>The primitive array flag is set the same in both
   * instances.</li>
   * </ul>
   *
   * @param obj the object to compare with.
   * @return true if the conditions above hold.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof ArrayType))
      return false;
    ArrayType<?> atype = (ArrayType<?>) obj;
    return (atype.getDimension() == dimension &&
            atype.getElementOpenType().equals(elementType) &&
            atype.isPrimitiveArray() == primitiveArray);
  }

  /**
   * <p>
   * Returns a new {@link ArrayType} instance in a type-safe
   * manner, by ensuring that the type of the given {@link OpenType}
   * matches the component type used in the type of the
   * returned instance.  If the given {@link OpenType} is a
   * {@link SimpleType}, {@link CompositeType} or
   * {@link TabularType}, then a 1-dimensional array of that
   * type is returned.  Otherwise, if the type is
   * an {@link ArrayType} of n dimensions, the returned
   * type is also an {@link ArrayType} but of n+1 dimensions.
   * For example,
   * {@code ArrayType.getArrayType(ArrayType.getArrayType(SimpleType.STRING))}
   * returns a 2-dimensional array of {@link SimpleType#String}.
   * </p>
   * <p>
   * This method caches its results, so that the same instance
   * is returned from subsequent calls with the same parameters.
   * </p>
   *
   * @param elementType the element type of the new array type.
   * @throws OpenDataException if the class name of {@code elementType}
   *                           is not in {@link OpenType#ALLOWED_CLASSNAMES_LIST}.
   * @since 1.6
   */
  @SuppressWarnings("unchecked")
  public static <E> ArrayType<E[]> getArrayType(OpenType<E> elementType)
    throws OpenDataException
  {
    ArrayType<E[]> arr = (ArrayType<E[]>) cache.get(elementType);
    if (arr != null)
      return arr;
    arr = new ArrayType<E[]>(1, elementType);
    cache.put(elementType, arr);
    return arr;
  }

  /**
   * <p>
   * Returns a new {@link ArrayType} instance for the given
   * primitive type in a type-safe* manner, by ensuring that
   * the type of the given {@link OpenType} matches the type
   * used in the returned instance.  If the type is
   * an array of n dimensions, the returned
   * type is also an {@link ArrayType} of n dimensions.
   * </p>
   * <p>
   * As an example, the array type returned by
   * <code>getPrimitiveArrayType(Integer.TYPE)</code> has the
   * following values:
   * </p>
   * <table>
   * <th><td>Attribute</td><td>Value</td></th>
   * <tr><td>Class Name</td><td><code>[I</code>
   * </td></tr>
   * <tr><td>Type Name</td><td><code>[I</code>
   * </td></tr>
   * <tr><td>Description</td><td><code>1-dimension array of int</code></td></tr>
   * <tr><td>Element Type Class Name</td><td><code>java.lang.Integer</code>
   * </td></tr>
   * </table>
   * <p>
   * This method caches its results, so that the same instance
   * is returned from subsequent calls with the same parameters.
   * </p>
   *
   * @param type the type of the new {@link ArrayType}.
   * @throws IllegalArgumentException if the type is not a primitive
   *                                  array.
   * @since 1.6
   */
  @SuppressWarnings("unchecked")
  public static <T> ArrayType<T> getPrimitiveArrayType(Class<T> type)
  {
    ArrayType<T> arr = (ArrayType<T>) primCache.get(type);
    if (arr != null)
      return arr;
    Class<?> comType = type;
    int dim = 0;
    do
      {
        comType = comType.getComponentType();
        ++dim;
        if (comType == null)
          throw new IllegalArgumentException("The given class is " +
                                             "not an array.");
      } while (comType.isArray());
    try
      {
        arr = new ArrayType<T>(getPrimitiveType(comType), true);
      }
    catch (OpenDataException e)
      {
        throw new IllegalArgumentException("The array is not of a primitive " +
                                           "type", e);
      }
    while (dim > 1)
      try
        {
          arr = new ArrayType<T>(1, arr);
          --dim;
        }
      catch (OpenDataException e)
        {
          throw (Error)
            new InternalError("Couldn't generate extra dimensions").initCause(e);
        }
    primCache.put(type, arr);
    return arr;
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
  public OpenType<?> getElementOpenType()
  {
    return elementType;
  }

  /**
   * <p>
   * Returns the hash code of the array type.
   * This is computed as the sum of the hash code of the
   * element type together with the number of dimensions
   * the array has and the primitive array flag.  These
   * are the same elements of the type that are compared as
   * part of the {@link #equals(java.lang.Object)} method,
   * thus ensuring that the hashcode is compatible with the
   * equality test.
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
      hashCode = Integer.valueOf(dimension +
                                 elementType.hashCode() +
                                 Boolean.valueOf(primitiveArray).hashCode());
    return hashCode.intValue();
  }

  /**
   * Returns true if this instance represents an array of
   * a primitive type.
   *
   * @return true if the array is of a primitive type.
   */
  public boolean isPrimitiveArray()
  {
    return primitiveArray;
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
    Class<?> objClass = obj.getClass();
    if (!(objClass.isArray()))
      return false;
    if (elementType instanceof SimpleType)
      return getClassName().equals(objClass.getName());
    Class<?> elementClass = null;
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
        + ", primitiveArray=" + primitiveArray
        + "]";
    return string;
  }

}
