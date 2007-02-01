/* java.lang.reflect.Array - manipulate arrays by reflection
   Copyright (C) 1998, 1999, 2001, 2003, 2005, 2007  Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;

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
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javalangreflect");
      }
  }

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
  public static native Object newInstance(Class<?> componentType, int length);

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
  public static native Object newInstance(Class<?> elementType, int[] dimensions);

  /**
   * Gets the array length.
   * @param array the array
   * @return the length of the array
   * @throws IllegalArgumentException if <code>array</code> is not an array
   * @throws NullPointerException if <code>array</code> is null
   */
  public static native int getLength(Object array);

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
  public static native Object get(Object array, int index);

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
  public static native boolean getBoolean(Object array, int index);
  
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
  public static native byte getByte(Object array, int index);

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
  public static native char getChar(Object array, int index);

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
  public static native short getShort(Object array, int index);

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
  public static native int getInt(Object array, int index);

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
  public static native long getLong(Object array, int index);

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
  public static native float getFloat(Object array, int index);

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
  public static native double getDouble(Object array, int index);

  private static native Class getElementType(Object array, int index);

  private static native void set(Object array, int index,
				  Object value, Class elType);

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
    Class elType = getElementType(array, index);
    if (! elType.isPrimitive())
      set(array, index, value, elType);
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
  public static native void setBoolean(Object array, int index, boolean value);

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
  public static native void setByte(Object array, int index, byte value);

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
  public static native void setChar(Object array, int index, char value);

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
  public static native void setShort(Object array, int index, short value);

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
  public static native void setInt(Object array, int index, int value);

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
  public static native void setLong(Object array, int index, long value);

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
  public static native void setFloat(Object array, int index, float value);

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
  public static native void setDouble(Object array, int index, double value);
}
