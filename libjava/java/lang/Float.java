/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang;

/**
 * @author Andrew Haley <aph@cygnus.com>
 * @date September 25, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

public final class Float extends Number
{
  public static final float MAX_VALUE = 3.4028235e+38f;
  public static final float MIN_VALUE = 1.4e-45f;
  public static final float NEGATIVE_INFINITY = -1.0f/0.0f;
  public static final float POSITIVE_INFINITY = 1.0f/0.0f;
  public static final float NaN = 0.0f/0.0f;

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = float.class;

  private float value;

  public Float (float value)
  {
    this.value = value;
  }

  public Float (double value)
  {
    this.value = (float)value;
  }

  public Float (String s) throws NumberFormatException
  {
    this.value = valueOf (s).floatValue ();
  }

  public String toString ()
  {
    return toString (value);
  }

  public boolean equals (Object obj)
  {
    if (obj == null)
      return false;

    if (!(obj instanceof Float))
      return false;

    Float f = (Float) obj;

    return floatToIntBits (value) == floatToIntBits (f.floatValue ());
  }

  public int hashCode ()
  {
    return floatToIntBits (value);
  }

  public int intValue ()
  {
    return (int) value;
  }

  public long longValue ()
  {
    return (long) value;
  }

  public float floatValue ()
  {
    return (float) value;
  }

  public double doubleValue ()
  {
    return (double) value;
  }

  public byte byteValue ()
  {
    return (byte) value;
  }

  public short shortValue ()
  {
    return (short) value;
  }

  public static String toString (float v)
  {
    return Double.toString ((double) v, true);
  } 

  public static Float valueOf (String s) throws NullPointerException, 
    NumberFormatException
  {
    if (s == null)
      throw new NullPointerException ();

    return new Float (Double.valueOf (s).floatValue ());
  }

  public boolean isNaN ()
  {
    return isNaN (value);
  }

  public static boolean isNaN (float v)
  {
    int bits = floatToIntBits (v);
    int e = bits & 0x7f800000;
    int f = bits & 0x007fffff;

    return e == 0x7f800000 && f != 0;
  }

  public boolean isInfinite ()
  {
    return isInfinite (value);
  }

  public static boolean isInfinite (float v)
  {
    int bits = floatToIntBits (v);
    int f = bits & 0x7fffffff;

    return f == 0x7f800000;
  }

  public static native int floatToIntBits (float value);

  public static native float intBitsToFloat (int bits);

}

