/* Copyright (C) 1998, 1999  Red Hat, Inc.

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

public final class Double extends Number
{
  public static final double MIN_VALUE = 5e-324;
  public static final double MAX_VALUE = 1.7976931348623157e+308;
  public static final double NEGATIVE_INFINITY = -1.0d/0.0d;
  public static final double POSITIVE_INFINITY = 1.0d/0.0d;
  public static final double NaN = 0.0d/0.0d;

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = double.class;

  private double value;

  private native static double doubleValueOf (String s) 
       throws NumberFormatException;  

  public Double (double v)
  {
    value = v;
  }

  public Double (String s) throws NumberFormatException
  {
    value = valueOf (s).doubleValue ();
  }

  public String toString ()
  {
    return toString (value);
  }

  public boolean equals (Object obj)
  {
    if (obj == null)
      return false;

    if (!(obj instanceof Double))
      return false;

    Double d = (Double) obj;

    return doubleToLongBits (value) == doubleToLongBits (d.doubleValue ());
  }

  public int hashCode ()
  {
    long v = doubleToLongBits (value);
    return (int) (v ^ (v >>> 32));
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
    return value;
  }

  public byte byteValue ()
  {
    return (byte) value;
  }

  public short shortValue ()
  {
    return (short) value;
  }

  native static String toString (double v, boolean isFloat);

  public static String toString (double v)
  {
    return toString (v, false);
  }

  public static Double valueOf (String s) throws NullPointerException, 
    NumberFormatException
  {
    if (s == null)
      throw new NullPointerException ();

    return new Double (doubleValueOf (s));
  }

  public boolean isNaN ()
  {
    return isNaN (value);
  }

  public static boolean isNaN (double v)
  {
    long bits = doubleToLongBits (v);
    long e = bits & 0x7ff0000000000000L;
    long f = bits & 0x000fffffffffffffL;

    return e == 0x7ff0000000000000L && f != 0L;
  }

  public boolean isInfinite ()
  {
    return isInfinite (value);
  }

  public static boolean isInfinite (double v)
  {
    long bits = doubleToLongBits (v);
    long f = bits & 0x7fffffffffffffffL;

    return f == 0x7ff0000000000000L;
  }

  public static native long doubleToLongBits (double value);

  public static native double longBitsToDouble (long bits);
}

