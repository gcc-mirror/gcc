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

public final class Byte extends Number implements Comparable
{
  byte value;

  public final static byte MIN_VALUE = -128;
  public final static byte MAX_VALUE = 127;

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = byte.class;

  public Byte(byte value)
  {
    this.value = value;
  }

  public Byte(String str) 
    throws NumberFormatException
  {
    this.value = parseByte(str, 10);
  }

  public byte byteValue()
  {
    return value;
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

  public static Byte decode(String str)
    throws NumberFormatException
  {
    int i = (Integer.decode(str)).intValue();
    if (i < MIN_VALUE || i > MAX_VALUE)
      throw new NumberFormatException();
    return new Byte((byte) i);
  }

  public static byte parseByte(String str, int radix)
    throws NumberFormatException
  {
    int i = Integer.parseInt(str, radix);
    if (i < MIN_VALUE || i > MAX_VALUE)
      throw new NumberFormatException();
    return (byte) i;
  }

  public static byte parseByte(String str)
    throws NumberFormatException
  {
    return parseByte(str, 10);
  }

  public static Byte valueOf(String str, int radix)
    throws NumberFormatException
  {
    return new Byte(parseByte(str, radix));
  }

  public static Byte valueOf(String str)
    throws NumberFormatException
  {
    return valueOf(str, 10);
  }

  // Added in JDK 1.2
  public int compareTo(Byte anotherByte)
  {
    return this.value - anotherByte.value;
  }

  // Added in JDK 1.2
  public int compareTo(Object o) throws ClassCastException
  {
    if (o instanceof Byte)
      return this.value - ((Byte) o).value;
    else
      throw new ClassCastException();
  }

  public boolean equals(Object obj)
  {
    return obj != null && (obj instanceof Byte) && ((Byte)obj).value == value;
  }

  // Verified that hashCode is returns plain value (see Boolean_1 test).
  public int hashCode()
  {
    return value;
  }

  public String toString()
  {
    return Integer.toString((int) value);
  }

  public static String toString(byte value)
  {
    return Integer.toString((int) value);
  }
}
