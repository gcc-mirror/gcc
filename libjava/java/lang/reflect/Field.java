/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang.reflect;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date September 1998;  February 1999.
 */
/* Status:  Mostly implemented.
 * However, access checks are not implemented.  See natField.cc for
 * _Jv_CheckFieldAccessibility as well as the missing getCaller.
 * Note that the idea is to have to compiler convert calls to
 * setXXX(...) and getXXX(...) to setXXX(CALLER, ...) and getXXX(CALLER, ...),
 * where CALLER is reference to the class that contains the calls to
 * setXXX or getXXX.  This is easy for the compiler, and replaces
 * expensive stack and table searching with a constant.
 */

public final class Field extends AccessibleObject implements Member
{
  private Class declaringClass;

  // This is filled in by getName.
  private String name;

  // Offset in bytes from the start of declaringClass's fields array.
  private int offset;

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

  // The idea is that the compiler will magically translate
  // fld.getShort(obj) to fld.getShort(THISCLASS, obj).
  // This makes checking assessiblity more efficient,
  // since we don't have to do any stack-walking.

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

  public native Object get (Class caller, Object obj)
    throws IllegalArgumentException, IllegalAccessException;

  public void setByte (Object obj, byte b)
    throws IllegalArgumentException, IllegalAccessException
  {
    setByte(null, obj, b);
  }

  public void setShort (Object obj,  short s)
    throws IllegalArgumentException, IllegalAccessException
  {
    setShort(null, obj, s);
  }

  public void setInt (Object obj, int i)
    throws IllegalArgumentException, IllegalAccessException
  {
    setInt(null, obj, i);
  }

  public void setLong (Object obj, long l)
    throws IllegalArgumentException, IllegalAccessException
  {
    setLong(null, obj, l);
  }

  public void setFloat (Object obj, float f)
    throws IllegalArgumentException, IllegalAccessException
  {
    setFloat(null, obj, f);
  }

  public void setDouble (Object obj, double d)
    throws IllegalArgumentException, IllegalAccessException
  {
    setDouble(null, obj, d);
  }

  public void setChar (Object obj, char c)
    throws IllegalArgumentException, IllegalAccessException
  {
    setChar(null, obj, c);
  }

  public void setBoolean (Object obj, boolean b)
    throws IllegalArgumentException, IllegalAccessException
  {
    setBoolean(null, obj, b);
  }

  public native void setByte (Class caller, Object obj, byte b)
    throws IllegalArgumentException, IllegalAccessException;

  public native void setShort (Class caller, Object obj, short s)
    throws IllegalArgumentException, IllegalAccessException;

  public native void setInt (Class caller, Object obj, int i)
    throws IllegalArgumentException, IllegalAccessException;

  public native void setLong (Class caller, Object obj, long l)
    throws IllegalArgumentException, IllegalAccessException;

  public native void setFloat (Class caller, Object obj, float f)
    throws IllegalArgumentException, IllegalAccessException;

  public native void setDouble (Class caller, Object obj, double d)
    throws IllegalArgumentException, IllegalAccessException;

  public native void setChar (Class caller, Object obj, char c)
    throws IllegalArgumentException, IllegalAccessException;

  public native void setBoolean (Class caller, Object obj, boolean b)
    throws IllegalArgumentException, IllegalAccessException;

  private native void set (Class caller, Object obj, Object val, Class type)
    throws IllegalArgumentException, IllegalAccessException;

  public void set (Object object, Object value)
    throws IllegalArgumentException, IllegalAccessException
  {
    set(null, object, value);
  }

  public void set (Class caller, Object object, Object value)
    throws IllegalArgumentException, IllegalAccessException
  {
    Class type = getType();
    if (! type.isPrimitive())
      set(caller, object, value, type);
    else if (value instanceof Byte)
      setByte(caller, object, ((Byte) value).byteValue());
    else if (value instanceof Short)
      setShort (caller, object, ((Short) value).shortValue());
    else if (value instanceof Integer)
      setInt(caller, object, ((Integer) value).intValue());
    else if (value instanceof Long)
      setLong(caller, object, ((Long) value).longValue());
    else if (value instanceof Float)
      setFloat(caller, object, ((Float) value).floatValue());
    else if (value instanceof Double)
      setDouble(caller, object, ((Double) value).doubleValue());
    else if (value instanceof Character)
      setChar(caller, object, ((Character) value).charValue());
    else if (value instanceof Boolean)
      setBoolean(caller, object, ((Boolean) value).booleanValue());
    else
      throw new IllegalArgumentException();
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer ();
    int mods = getModifiers();
    if (mods != 0)
      Modifier.toString(mods, sbuf);
    sbuf.append(getType());
    sbuf.append(' ');
    sbuf.append(getDeclaringClass());
    sbuf.append('.');
    sbuf.append(getName());
    return sbuf.toString();
  }
}
