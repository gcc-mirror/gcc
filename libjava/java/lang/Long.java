/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.lang;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date September 18, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public final class Long extends Number implements Comparable
{
  public static final long MAX_VALUE = 0x7FFFFFFFFFFFFFFFL;
  public static final long MIN_VALUE = 0x8000000000000000L;

  // This initialization is seemingly circular, but it is accepted
  // by javac, and is handled specially by gcc.
  public static final Class TYPE = long.class;

  /* The long value of the instance. */
  private long value;

  public Long(long val)
  {
    value = val;
  }

  public Long(String str) throws NumberFormatException
  {
    value = parseLong(str, 10);
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
    return (int) value;
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
  public int compareTo(Long anotherLong)
  {
    if (this.value == anotherLong.value)
      return 0;

    // Returns just -1 or 1 on inequality; doing math might overflow the long.
    if (this.value > anotherLong.value)
      return 1;

    return -1;
  }

  // Added in JDK 1.2
  public int compareTo(Object o) throws ClassCastException
  {
    if (!(o instanceof Long))
      throw new ClassCastException();

    return this.compareTo((Long) o);
  }

  // Added in JDK 1.2
  public static Long decode(String str) throws NumberFormatException
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
          return new Long(0L);

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

    return new Long(parseLong(str, index, len, isNeg, radix));
  }

  public boolean equals(Object obj)
  {
    return (obj != null && (obj instanceof Long)
            && ((Long) obj).value == value);
  }

  public static Long getLong(String prop)
  {
    return getLong(prop, null);
  }

  public static Long getLong(String prop, long defval)
  {
    Long val = getLong(prop, null);
    return val == null ? new Long(defval) : val;
  }

  public static Long getLong(String prop, Long defobj)
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
    return (int)(this.longValue()^(this.longValue()>>>32));
  }

  public static long parseLong(String str) throws NumberFormatException
  {
    return parseLong(str, 10);
  }

  public static long parseLong(String str, int radix)
			throws NumberFormatException
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

    return parseLong(str, index, len, isNeg, radix);
  }

  private static long parseLong(String str, int index, int len, boolean isNeg,
        			int radix) throws NumberFormatException
  {
    long val = 0;
    int digval;

    long max = MAX_VALUE / radix;
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

  public static String toBinaryString(long num)
  {
    return toUnsignedString(num, 1);
  }

  public static String toHexString(long num)
  {
    return toUnsignedString(num, 4);
  }

  public static String toOctalString(long num)
  {
    return toUnsignedString(num, 3);
  }

  private static String toUnsignedString(long num, int exp)
  {
    // Use an array large enough for a binary number.
    int radix = 1 << exp;
    long mask = radix - 1;
    char[] buffer = new char[64];
    int i = 64;
    do
      {
        buffer[--i] = Character.forDigit((int) (num & mask), radix);
        num = num >>> exp;
      }
    while (num != 0);

    return String.valueOf(buffer, i, 64-i);
  }

  public String toString()
  {
    return toString(this.value);
  }

  public static String toString(long num)
  {
    // Use the Integer toString for efficiency if possible.
    if (num <= Integer.MAX_VALUE && num >= Integer.MIN_VALUE)
      return Integer.toString((int) num);

    // Use an arrary large enough for "-9223372036854775808"; i.e. 11 chars.
    char[] buffer = new char[20];
    int i = 20;
    boolean isNeg;
    if (num < 0)
      {
        isNeg = true;
        num = -(num);
        if (num < 0)
          {
            // Must be MIN_VALUE, so handle this special case.
            buffer[--i] = '8';
            num = 922337203685477580L;
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

    return String.valueOf(buffer, i, 20-i);
  }

  public static String toString(long num, int radix)
  {
    // Use optimized method for the typical case.
    if (radix == 10 ||
        radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      return toString(num);

    // Use the Integer toString for efficiency if possible.
    if (num <= Integer.MAX_VALUE && num >= Integer.MIN_VALUE)
      return Integer.toString((int) num, radix);

    // For negative numbers, print out the absolute value w/ a leading '-'.
    // Use an array large enough for a binary number.
    char[] buffer = new char[65];
    int i = 65;
    boolean isNeg;
    if (num < 0)
      {
        isNeg = true;
        num = -(num);

        // When the value is MIN_VALUE, it overflows when made positive
        if (num < 0)
          {
            buffer[--i] = Character.forDigit((int) (-(num + radix) % radix),
						radix);
            num = -(num / radix);
          }
      }
    else
      isNeg = false;

    do
      {
        buffer[--i] = Character.forDigit((int) (num % radix), radix);
        num /= radix;
      }
    while (num > 0);

    if (isNeg)
      buffer[--i] = '-';

    return String.valueOf(buffer, i, 65-i);
  }

  public static Long valueOf(String str) throws NumberFormatException
  {
    return new Long(parseLong(str, 10));
  }

  public static Long valueOf(String str, int radix)
  				throws NumberFormatException
  {
    return new Long(parseLong(str, radix));
  }
}
