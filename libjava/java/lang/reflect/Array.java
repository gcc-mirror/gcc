// FileDescriptor.java - Open file or device

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang.reflect;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date january 12, 1999
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3.
 * Status:  Believe complete and correct.
 */

public final class Array
{
  Array () { }

  public static native Object newInstance(Class componentType, int length);
  public static native Object newInstance(Class elementType, int[] dimensions);
  public static native int getLength (Object array);

  public static native Object  get (Object array, int index);
  public static native char    getChar (Object array, int index);
  public static native byte    getByte (Object array, int index);
  public static native short   getShort (Object array, int index);
  public static native int     getInt (Object array, int index);
  public static native long    getLong (Object array, int index);
  public static native float   getFloat (Object array, int index);
  public static native double  getDouble (Object array, int index);
  public static native boolean getBoolean (Object array, int index);

  private static native Class getElementType (Object array, int index);

  private static native void set (Object array, int index,
				  Object value, Class elType);

  public static void set (Object array, int index, Object value)
  {
    Class elType = getElementType(array, index);
    if (! elType.isPrimitive())
      set(array, index, value, elType);
    else if (value instanceof Byte)
      setByte(array, index, ((Byte) value).byteValue());
    else if (value instanceof Short)
      setShort (array, index, ((Short) value).shortValue());
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

  public static native void setByte   (Object array, int index, byte value);
  public static native void setShort  (Object array, int index, short value);
  public static native void setInt    (Object array, int index, int value);
  public static native void setLong   (Object array, int index, long value);
  public static native void setFloat  (Object array, int index, float value);
  public static native void setDouble (Object array, int index, double value);
  public static native void setChar   (Object array, int index, char value);
  public static native void setBoolean(Object array, int index, boolean value);
}
