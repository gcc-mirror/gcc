/* Copyright (C) 1998, 1999, 2000, 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang.reflect;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date September 1998;  February 1999.
 */

public final class Field extends AccessibleObject implements Member
{
  private Class declaringClass;

  // This is filled in by getName.
  private String name;

  // Offset in bytes from the start of declaringClass's fields array.
  private int offset;

  // The Class (or primitive TYPE) of this field.
  private Class type;

  // This is instantiated by Class sometimes, but it uses C++ and
  // avoids the Java protection check.
  Field ()
  {
  }

  public boolean equals (Object fld)
  {
    if (! (fld instanceof Field))
      return false;
    Field f = (Field) fld;
    return declaringClass == f.declaringClass && offset == f.offset;
  }

  public Class getDeclaringClass ()
  {
    return declaringClass;
  }

  public native String getName ();

  public native Class getType ();

  public native int getModifiers ();

  public int hashCode()
  {
    return (declaringClass.hashCode() ^ offset);
  }

  public boolean getBoolean (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getBoolean(null, obj);
  }
  public char getChar (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getChar(null, obj);
  }

  public byte getByte (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getByte(null, obj);
  }

  public short getShort (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getShort(null, obj);
  }

  public int getInt (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getInt(null, obj);
  }

  public long getLong (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getLong(null, obj);
  }

  public float getFloat (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getFloat(null, obj);
  }

  public double getDouble (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return getDouble(null, obj);
  }

  public Object get (Object obj)
    throws IllegalArgumentException, IllegalAccessException
  {
    return get(null, obj);
  }

  private native boolean getBoolean (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native char getChar (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native byte getByte (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native short getShort (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native int getInt (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native long getLong (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native float getFloat (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native double getDouble (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  private native Object get (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  public void setByte (Object obj, byte b)
    throws IllegalArgumentException, IllegalAccessException
  {
    setByte(null, obj, b, true);
  }

  public void setShort (Object obj,  short s)
    throws IllegalArgumentException, IllegalAccessException
  {
    setShort(null, obj, s, true);
  }

  public void setInt (Object obj, int i)
    throws IllegalArgumentException, IllegalAccessException
  {
    setInt(null, obj, i, true);
  }

  public void setLong (Object obj, long l)
    throws IllegalArgumentException, IllegalAccessException
  {
    setLong(null, obj, l, true);
  }

  public void setFloat (Object obj, float f)
    throws IllegalArgumentException, IllegalAccessException
  {
    setFloat(null, obj, f, true);
  }

  public void setDouble (Object obj, double d)
    throws IllegalArgumentException, IllegalAccessException
  {
    setDouble(null, obj, d, true);
  }

  public void setChar (Object obj, char c)
    throws IllegalArgumentException, IllegalAccessException
  {
    setChar(null, obj, c, true);
  }

  public void setBoolean (Object obj, boolean b)
    throws IllegalArgumentException, IllegalAccessException
  {
    setBoolean(null, obj, b, true);
  }

  native void setByte (Class caller, Object obj, byte b, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setShort (Class caller, Object obj, short s, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setInt (Class caller, Object obj, int i, boolean checkFinal)  
    throws IllegalArgumentException, IllegalAccessException;

  native void setLong (Class caller, Object obj, long l, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setFloat (Class caller, Object obj, float f, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setDouble (Class caller, Object obj, double d,
			 boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setChar (Class caller, Object obj, char c, boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void setBoolean (Class caller, Object obj, boolean b,
			  boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  native void set (Class caller, Object obj, Object val, Class type, 
		   boolean checkFinal)
    throws IllegalArgumentException, IllegalAccessException;

  public void set (Object object, Object value)
    throws IllegalArgumentException, IllegalAccessException
  {
    set(null, object, value);
  }

  private void set (Class caller, Object object, Object value)
    throws IllegalArgumentException, IllegalAccessException
  {
    Class type = getType();
    if (! type.isPrimitive())
      set(caller, object, value, type, true);
    else if (value instanceof Byte)
      setByte(caller, object, ((Byte) value).byteValue(), true);
    else if (value instanceof Short)
      setShort (caller, object, ((Short) value).shortValue(), true);
    else if (value instanceof Integer)
      setInt(caller, object, ((Integer) value).intValue(), true);
    else if (value instanceof Long)
      setLong(caller, object, ((Long) value).longValue(), true);
    else if (value instanceof Float)
      setFloat(caller, object, ((Float) value).floatValue(), true);
    else if (value instanceof Double)
      setDouble(caller, object, ((Double) value).doubleValue(), true);
    else if (value instanceof Character)
      setChar(caller, object, ((Character) value).charValue(), true);
    else if (value instanceof Boolean)
      setBoolean(caller, object, ((Boolean) value).booleanValue(), true);
    else
      throw new IllegalArgumentException();
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer ();
    int mods = getModifiers();
    if (mods != 0)
      {
	Modifier.toString(mods, sbuf);
	sbuf.append(' ');
      }
    Method.appendClassName (sbuf, getType ());
    sbuf.append(' ');
    Method.appendClassName (sbuf, getDeclaringClass());
    sbuf.append('.');
    sbuf.append(getName());
    return sbuf.toString();
  }
}
