/* java.lang.reflect.Array - manipulate arrays by reflection
   Copyright (C) 1998, 1999, 2001, 2003, 2005  Free Software Foundation, Inc.

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


package java.lang.reflect;

/**
 * Array holds static helper functions that allow you to create and
 * manipulate arrays by reflection. Operations know how to perform widening
 * conversions, but throw {@link IllegalArgumentException} if you attempt
 * a narrowing conversion. Also, when accessing primitive arrays, this
 * class performs object wrapping and unwrapping as necessary.<p>
 *
 * <B>Note:</B> This class returns and accepts types as Classes, even
 * primitive types; there are Class types defined that represent each
 * different primitive type.  They are <code>java.lang.Boolean.TYPE,
 * java.lang.Byte.TYPE,</code>, also available as <code>boolean.class,
 * byte.class</code>, etc.  These are not to be confused with the
 * classes <code>java.lang.Boolean, java.lang.Byte</code>, etc., which are
 * real classes. Note also that the shorthand <code>Object[].class</code>
 * is a convenient way to get array Classes.<p>
 *
 * <B>Performance note:</B> This class performs best when it does not have
 * to convert primitive types.  The further along the chain it has to convert,
 * the worse performance will be.  You're best off using the array as whatever
 * type it already is, and then converting the result.  You will do even
 * worse if you do this and use the generic set() function.
 *
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Per Bothner (bothner@cygnus.com)
 * @see java.lang.Boolean#TYPE
 * @see java.lang.Byte#TYPE
 * @see java.lang.Short#TYPE
 * @see java.lang.Character#TYPE
 * @see java.lang.Integer#TYPE
 * @see java.lang.Long#TYPE
 * @see java.lang.Float#TYPE
 * @see java.lang.Double#TYPE
 * @since 1.1
 * @status updated to 1.4
 */
public final class Array
{

  /**
   * This class is uninstantiable.
   */
  private Array()
  {
  }

  /**
   * Creates a new single-dimensioned array.
   * @param componentType the type of the array to create
   * @param length the length of the array to create
   * @return the created array, cast to an Object
   * @throws NullPointerException if <code>componentType</code> is null
   * @throws IllegalArgumentException if <code>componentType</code> is
   *         <code>Void.TYPE</code>
   * @throws NegativeArraySizeException when length is less than 0
   * @throws OutOfMemoryError if memory allocation fails
   */
  public static Object newInstance(Class<?> componentType, int length)
  {
    if (! componentType.isPrimitive())
      return VMArray.createObjectArray(componentType, length);
    if (componentType == boolean.class)
      return new boolean[length];
    if (componentType == byte.class)
      return new byte[length];
    if (componentType == char.class)
      return new char[length];
    if (componentType == short.class)
      return new short[length];
    if (componentType == int.class)
      return new int[length];
    if (componentType == long.class)
      return new long[length];
    if (componentType == float.class)
      return new float[length];
    if (componentType == double.class)
      return new double[length];
    // assert componentType == void.class
    throw new IllegalArgumentException();
  }

  /**
   * Creates a new multi-dimensioned array.  The new array has the same
   * component type as the argument class, and the number of dimensions
   * in the new array is the sum of the dimensions of the argument class
   * and the length of the argument dimensions. Virtual Machine limitations
   * forbid too many dimensions (usually 255 is the maximum); but even
   * 50 dimensions of 2 elements in each dimension would exceed your memory
   * long beforehand!
   *
   * @param componentType the type of the array to create.
   * @param dimensions the dimensions of the array to create.  Each element
   *        in <code>dimensions</code> makes another dimension of the new
   *        array.  Thus, <code>Array.newInstance(java.lang.Boolean,
   *        new int[]{1,2,3})</code> is the same as
   *        <code>new java.lang.Boolean[1][2][3]</code>
   * @return the created array, cast to an Object
   * @throws NullPointerException if componentType or dimension is null
   * @throws IllegalArgumentException if the the size of
   *         <code>dimensions</code> is 0 or exceeds the maximum number of
   *         array dimensions in the VM; or if componentType is Void.TYPE
   * @throws NegativeArraySizeException when any of the dimensions is less
   *         than 0
   * @throws OutOfMemoryError if memory allocation fails
   */
  public static Object newInstance(Class<?> componentType, int[] dimensions)
  {
    if (dimensions.length <= 0)
      throw new IllegalArgumentException ("Empty dimensions array.");
    return createMultiArray(componentType, dimensions, 0);
  }

  /**
   * Gets the array length.
   * @param array the array
   * @return the length of the array
   * @throws IllegalArgumentException if <code>array</code> is not an array
   * @throws NullPointerException if <code>array</code> is null
   */
  public static int getLength(Object array)
  {
    if (array instanceof Object[])
      return ((Object[]) array).length;
    if (array instanceof boolean[])
      return ((boolean[]) array).length;
    if (array instanceof byte[])
      return ((byte[]) array). length;
    if (array instanceof char[])
      return ((char[]) array).length;
    if (array instanceof short[])
      return ((short[]) array).length;
    if (array instanceof int[])
      return ((int[]) array).length;
    if (array instanceof long[])
      return ((long[]) array).length;
    if (array instanceof float[])
      return ((float[]) array).length;
    if (array instanceof double[])
      return ((double[]) array).length;
    if (array == null)
      throw new NullPointerException();
    throw new IllegalArgumentException();
  }

  /**
   * Gets an element of an array.  Primitive elements will be wrapped in
   * the corresponding class type.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the element at <code>array[index]</code>
   * @throws IllegalArgumentException if <code>array</code> is not an array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #getBoolean(Object, int)
   * @see #getByte(Object, int)
   * @see #getChar(Object, int)
   * @see #getShort(Object, int)
   * @see #getInt(Object, int)
   * @see #getLong(Object, int)
   * @see #getFloat(Object, int)
   * @see #getDouble(Object, int)
   */
  public static Object get(Object array, int index)
  {
    if (array instanceof Object[])
      return ((Object[]) array)[index];
    if (array instanceof boolean[])
      return ((boolean[]) array)[index] ? Boolean.TRUE : Boolean.FALSE;
    if (array instanceof byte[])
      return new Byte(((byte[]) array)[index]);
    if (array instanceof char[])
      return new Character(((char[]) array)[index]);
    if (array instanceof short[])
      return new Short(((short[]) array)[index]);
    if (array instanceof int[])
      return new Integer(((int[]) array)[index]);
    if (array instanceof long[])
      return new Long(((long[]) array)[index]);
    if (array instanceof float[])
      return new Float(((float[]) array)[index]);
    if (array instanceof double[])
      return new Double(((double[]) array)[index]);
    if (array == null)
      throw new NullPointerException();
    throw new IllegalArgumentException();
  }

  /**
   * Gets an element of a boolean array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the boolean element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a boolean
   *         array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static boolean getBoolean(Object array, int index)
  {
    if (array instanceof boolean[])
      return ((boolean[]) array)[index];
    if (array == null)
      throw new NullPointerException();
    throw new IllegalArgumentException();
  }
  
  /**
   * Gets an element of a byte array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the byte element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a byte
   *         array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static byte getByte(Object array, int index)
  {
    if (array instanceof byte[])
      return ((byte[]) array)[index];
    if (array == null)
      throw new NullPointerException();
    throw new IllegalArgumentException();
  }

  /**
   * Gets an element of a char array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the char element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a char
   *         array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static char getChar(Object array, int index)
  {
    if (array instanceof char[])
      return ((char[]) array)[index];
    if (array == null)
      throw new NullPointerException();
    throw new IllegalArgumentException();
  }

  /**
   * Gets an element of a short array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the short element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a byte
   *         or char array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static short getShort(Object array, int index)
  {
    if (array instanceof short[])
      return ((short[]) array)[index];
    return getByte(array, index);
  }

  /**
   * Gets an element of an int array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the int element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a byte,
   *         char, short, or int array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static int getInt(Object array, int index)
  {
    if (array instanceof int[])
      return ((int[]) array)[index];
    if (array instanceof char[])
      return ((char[]) array)[index];
    return getShort(array, index);
  }

  /**
   * Gets an element of a long array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the long element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a byte,
   *         char, short, int, or long array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static long getLong(Object array, int index)
  {
    if (array instanceof long[])
      return ((long[]) array)[index];
    return getInt(array, index);
  }

  /**
   * Gets an element of a float array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the float element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a byte,
   *         char, short, int, long, or float array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static float getFloat(Object array, int index)
  {
    if (array instanceof float[])
      return ((float[]) array)[index];
    return getLong(array, index);
  }

  /**
   * Gets an element of a double array.
   *
   * @param array the array to access
   * @param index the array index to access
   * @return the double element at <code>array[index]</code>
   * @throws IllegalArgumentException  if <code>array</code> is not a byte,
   *         char, short, int, long, float, or double array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #get(Object, int)
   */
  public static double getDouble(Object array, int index)
  {
    if (array instanceof double[])
      return ((double[]) array)[index];
    return getFloat(array, index);
  }

  /**
   * Sets an element of an array. If the array is primitive, then the new
   * value is unwrapped and widened.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not an array,
   *         or the array is primitive and unwrapping value fails, or the
   *         value is not assignable to the array component type
   * @throws NullPointerException if array is null, or if array is primitive
   *         and value is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #setBoolean(Object, int, boolean)
   * @see #setByte(Object, int, byte)
   * @see #setChar(Object, int, char)
   * @see #setShort(Object, int, short)
   * @see #setInt(Object, int, int)
   * @see #setLong(Object, int, long)
   * @see #setFloat(Object, int, float)
   * @see #setDouble(Object, int, double)
   */
  public static void set(Object array, int index, Object value)
  {
    if (array instanceof Object[])
      {
	// Too bad the API won't let us throw the easier ArrayStoreException!
	if (value != null
	    && ! array.getClass().getComponentType().isInstance(value))
	  throw new IllegalArgumentException();
	((Object[]) array)[index] = value;
      }
    else if (value instanceof Byte)
      setByte(array, index, ((Byte) value).byteValue());
    else if (value instanceof Short)
      setShort(array, index, ((Short) value).shortValue());
    else if (value instanceof Integer)
      setInt(array, index, ((Integer) value).intValue());
    else if (value instanceof Long)
      setLong(array, index, ((Long) value).longValue());
    else if (value instanceof Float)
      setFloat(array, index, ((Float) value).floatValue());
    else if (value instanceof Double)
      setDouble(array, index, ((Double) value).doubleValue());
    else if (value instanceof Character)
      setChar(array, index, ((Character) value).charValue());
    else if (value instanceof Boolean)
      setBoolean(array, index, ((Boolean) value).booleanValue());
    else if (array == null)
      throw new NullPointerException();
    else
      throw new IllegalArgumentException();
  }

  /**
   * Sets an element of a boolean array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not a boolean
   *         array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setBoolean(Object array, int index, boolean value)
  {
    if (array instanceof boolean[])
      ((boolean[]) array)[index] = value;
    else if (array == null)
      throw new NullPointerException();
    else
      throw new IllegalArgumentException();
  }

  /**
   * Sets an element of a byte array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not a byte,
   *         short, int, long, float, or double array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setByte(Object array, int index, byte value)
  {
    if (array instanceof byte[])
      ((byte[]) array)[index] = value;
    else
      setShort(array, index, value);
  }

  /**
   * Sets an element of a char array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not a char,
   *         int, long, float, or double array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setChar(Object array, int index, char value)
  {
    if (array instanceof char[])
      ((char[]) array)[index] = value;
    else
      setInt(array, index, value);
  }

  /**
   * Sets an element of a short array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not a short,
   *         int, long, float, or double array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setShort(Object array, int index, short value)
  {
    if (array instanceof short[])
      ((short[]) array)[index] = value;
    else
      setInt(array, index, value);
  }

  /**
   * Sets an element of an int array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not an int,
   *         long, float, or double array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setInt(Object array, int index, int value)
  {
    if (array instanceof int[])
      ((int[]) array)[index] = value;
    else
      setLong(array, index, value);
  }

  /**
   * Sets an element of a long array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not a long,
   *         float, or double array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setLong(Object array, int index, long value)
  {
    if (array instanceof long[])
      ((long[]) array)[index] = value;
    else
      setFloat(array, index, value);
  }

  /**
   * Sets an element of a float array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not a float
   *         or double array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setFloat(Object array, int index, float value)
  {
    if (array instanceof float[])
      ((float[]) array)[index] = value;
    else
      setDouble(array, index, value);
  }

  /**
   * Sets an element of a double array.
   *
   * @param array the array to set a value of
   * @param index the array index to set the value to
   * @param value the value to set
   * @throws IllegalArgumentException if <code>array</code> is not a double
   *         array
   * @throws NullPointerException if <code>array</code> is null
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds
   * @see #set(Object, int, Object)
   */
  public static void setDouble(Object array, int index, double value)
  {
    if (array instanceof double[])
      ((double[]) array)[index] = value;
    else if (array == null)
      throw new NullPointerException();
    else
      throw new IllegalArgumentException();
  }

  /**
   * Dynamically and recursively create a multi-dimensioned array of objects.
   *
   * @param type guaranteed to be a valid object type
   * @param dimensions the dimensions of the array
   * @param index index of the current dimension to build
   * @return the new multi-dimensioned array
   * @throws NegativeArraySizeException if any entry of dimensions is negative
   * @throws OutOfMemoryError if memory allocation fails
   */
  // This would be faster if implemented natively, using the multianewarray
  // bytecode instead of this recursive call
  private static Object createMultiArray(Class type, int[] dimensions,
                                         int index)
  {
    if (index == dimensions.length - 1)
      return newInstance(type, dimensions[index]);

    Object toAdd = createMultiArray(type, dimensions, index + 1);
    Class thisType = toAdd.getClass();
    Object[] retval
      = (Object[]) VMArray.createObjectArray(thisType, dimensions[index]);
    if (dimensions[index] > 0)
      retval[0] = toAdd;
    int i = dimensions[index];
    while (--i > 0)
      retval[i] = createMultiArray(type, dimensions, index + 1);
    return retval;
  }

}
