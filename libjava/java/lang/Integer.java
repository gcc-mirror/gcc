/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date September 11, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public final class Integer extends Number implements Comparable
{
  public static final int MAX_VALUE = 0x7FFFFFFF;
  public static final int MIN_VALUE = 0x80000000;

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = int.class;

  /* The int value of the instance. */
  private int value;

  public Integer(int val)
  {
    value = val;
  }

  public Integer(String str) throws NumberFormatException
  {
    value = parseInt(str, 10);
  }

  public byte byteValue()
  {
    return (byte) value;
  }

  public double doubleValue()
  {
    return (double) value;
  }

  public float floatValue()
  {
    return (float) value;
  }

  public int intValue()
  {
    return value;
  }

  public long longValue()
  {
    return value;
  }

  public short shortValue()
  {
    return (short) value;
  }

  // Added in JDK 1.2
  public int compareTo(Integer anotherInteger)
  {
    if (this.value == anotherInteger.value)
      return 0;

    // Returns just -1 or 1 on inequality; doing math might overflow the int.
    if (this.value > anotherInteger.value)
      return 1;

    return -1;
  }

  // Added in JDK 1.2
  public int compareTo(Object o) throws ClassCastException
  {
    if (!(o instanceof Integer))
      throw new ClassCastException();

    return this.compareTo((Integer) o);
  }

  public static Integer decode(String str) throws NumberFormatException
  {
    boolean isNeg = false;
    int index = 0;
    int radix = 10;
    final int len;

    if (str == null || (len = str.length()) == 0)
      throw new NumberFormatException();

    // Negative numbers are always radix 10.
    if (str.charAt(0) == '-')
      {
        radix = 10;
        index++;
        isNeg = true;
      }
    else if (str.charAt(index) == '#')
      {
        radix = 16;
        index++;
      }
    else if (str.charAt(index) == '0')
      {
        // Check if str is just "0"
        if (len == 1)
          return new Integer(0);

        index++;
        if (str.charAt(index) == 'x')
          {
            radix = 16;
            index++;
          }
        else
          radix = 8;
      }

    if (index >= len)
      throw new NumberFormatException();

    return new Integer(parseInt(str, index, len, isNeg, radix));
  }

  public boolean equals(Object obj)
  {
    return (obj != null && (obj instanceof Integer)
            && ((Integer) obj).value == value);
  }

  public static Integer getInteger(String prop)
  {
    return getInteger(prop, null);
  }

  public static Integer getInteger(String prop, int defval)
  {
    Integer val = getInteger(prop, null);
    return val == null ? new Integer(defval) : val;
  }

  public static Integer getInteger(String prop, Integer defobj)
  {
    try
    {
      return decode(System.getProperty(prop));
    }
    catch (NumberFormatException ex)
    {
      return defobj;
    }
  }

  public int hashCode()
  {
    return value;
  }

  public static int parseInt(String str) throws NumberFormatException
  {
    return parseInt(str, 10);
  }

  public static int parseInt(String str, int radix) throws NumberFormatException
  {
    final int len;

    if (str == null || (len = str.length()) == 0 ||
        radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      throw new NumberFormatException();

    boolean isNeg = false;
    int index = 0;
    if (str.charAt(index) == '-')
      if (len > 1)
        {
          isNeg = true;
          index++;
        }
      else
        throw new NumberFormatException();

    return parseInt(str, index, len, isNeg, radix);
  }

  private static int parseInt(String str, int index, int len, boolean isNeg,
        			int radix) throws NumberFormatException
  {
    int val = 0;
    int digval;

    int max = MAX_VALUE / radix;
    // We can't directly write `max = (MAX_VALUE + 1) / radix'.
    // So instead we fake it.
    if (isNeg && MAX_VALUE % radix == radix - 1)
      ++max;

    for ( ; index < len; index++)
      {
	if (val < 0 || val > max)
	  throw new NumberFormatException();

        if ((digval = Character.digit(str.charAt(index), radix)) < 0)
          throw new NumberFormatException();

        // Throw an exception for overflow if result is negative.
	// However, we special-case the most negative value.
	val = val * radix + digval;
	if (val < 0 && (! isNeg || val != MIN_VALUE))
	  throw new NumberFormatException();
      }

    return isNeg ? -(val) : val;
  }

  public static String toBinaryString(int num)
  {
    return toUnsignedString(num, 1);
  }

  public static String toHexString(int num)
  {
    return toUnsignedString(num, 4);
  }

  public static String toOctalString(int num)
  {
    return toUnsignedString(num, 3);
  }

  private static String toUnsignedString(int num, int exp)
  {
    // Use an array large enough for a binary number.
    int radix = 1 << exp;
    int mask = radix - 1;
    char[] buffer = new char[32];
    int i = 32;
    do
      {
        buffer[--i] = Character.forDigit(num & mask, radix);
        num = num >>> exp;
      }
    while (num != 0);

    return String.valueOf(buffer, i, 32-i);
  }

  public String toString()
  {
    return toString(this.value);
  }

  public static String toString(int num)
  {
    // Use an arrary large enough for "-2147483648"; i.e. 11 chars.
    char[] buffer = new char[11];
    int i = 11;
    boolean isNeg;
    if (num < 0)
      {
        isNeg = true;
        num = -(num);
        if (num < 0)
          {
            // Must be MIN_VALUE, so handle this special case.
            buffer[--i] = '8';
            num = 214748364;
          }
      }
    else
      isNeg = false;

    do
      {
        buffer[--i] = (char) ((int) '0' + (num % 10));
        num /= 10;
      }
    while (num > 0);

    if (isNeg)
      buffer[--i] = '-';

    return String.valueOf(buffer, i, 11-i);
  }

  public static String toString(int num, int radix)
  {
    // Use optimized method for the typical case.
    if (radix == 10 ||
        radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      return toString(num);

    // For negative numbers, print out the absolute value w/ a leading '-'.
    // Use an array large enough for a binary number.
    char[] buffer = new char[33];
    int i = 33;
    boolean isNeg;
    if (num < 0)
      {
        isNeg = true;
        num = -(num);

        // When the value is MIN_VALUE, it overflows when made positive
        if (num < 0)
          {
            buffer[--i] = Character.forDigit(-(num + radix) % radix, radix);
            num = -(num / radix);
          }
      }
    else
      isNeg = false;

    do
      {
        buffer[--i] = Character.forDigit(num % radix, radix);
        num /= radix;
      }
    while (num > 0);

    if (isNeg)
      buffer[--i] = '-';

    return String.valueOf(buffer, i, 33-i);
  }

  public static Integer valueOf(String str) throws NumberFormatException
  {
    return new Integer(parseInt(str, 10));
  }

  public static Integer valueOf(String str, int radix)
  				throws NumberFormatException
  {
    return new Integer(parseInt(str, radix));
  }
}
