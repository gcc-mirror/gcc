/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 17, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 *	    Includes JDK 1.2 methods.
 */

public final class Short extends Number implements Comparable
{
  short value;

  public final static short MIN_VALUE = -32768;
  public final static short MAX_VALUE = 32767;

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = short.class;

  public Short(short value)
  {
    this.value = value;
  }

  public Short(String str) 
    throws NumberFormatException
  {
    this.value = parseShort(str, 10);
  }

  public byte byteValue()
  {
    return (byte) value;
  }

  public short shortValue()
  {
    return value;
  }

  public int intValue()
  {
    return value;
  }

  public long longValue ()
  {
    return value;
  }

  public float floatValue ()
  {
    return (float) value;
  }

  public double doubleValue ()
  {
    return (double) value;
  }

  public static Short decode(String str)
    throws NumberFormatException
  {
    int i = (Integer.decode(str)).intValue();
    if (i < MIN_VALUE || i > MAX_VALUE)
      throw new NumberFormatException();
    return new Short((short) i);
  }

  public static short parseShort(String str, int radix)
    throws NumberFormatException
  {
    int i = Integer.parseInt(str, radix);
    if (i < MIN_VALUE || i > MAX_VALUE)
      throw new NumberFormatException();
    return (short) i;
  }

  public static short parseShort(String str)
    throws NumberFormatException
  {
    return parseShort(str, 10);
  }

  public static Short valueOf(String str, int radix)
    throws NumberFormatException
  {
    return new Short(parseShort(str, radix));
  }

  public static Short valueOf(String str)
    throws NumberFormatException
  {
    return valueOf(str, 10);
  }

  // Added in JDK 1.2
  public int compareTo(Short anotherShort)
  {
    return this.value - anotherShort.value;
  }

  // Added in JDK 1.2
  public int compareTo(Object o) throws ClassCastException
  {
    if (o instanceof Short)
      return this.value - ((Short) o).value;
    else
      throw new ClassCastException();
  }

  public boolean equals(Object obj)
  {
    return (obj != null && (obj instanceof Short)
	    && ((Short) obj).value == value);
  }

  // Verified that hashCode is returns plain value (see Short_1 test).
  public int hashCode()
  {
    return value;
  }

  public String toString()
  {
    return Integer.toString((int) value);
  }

  public static String toString(short value)
  {
    return Integer.toString((int) value);
  }
}
