/* Integer.java -- object wrapper for int
   Copyright (C) 1998, 1999, 2001, 2002, 2004, 2005
   Free Software Foundation, Inc.

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


package java.lang;

/**
 * Instances of class <code>Integer</code> represent primitive
 * <code>int</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to ints.
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Warren Levy
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @author Ian Rogers
 * @since 1.0
 * @status updated to 1.5
 */
public final class Integer extends Number implements Comparable<Integer>
{
  /**
   * Compatible with JDK 1.0.2+.
   */
  private static final long serialVersionUID = 1360826667806852920L;

  /**
   * The minimum value an <code>int</code> can represent is -2147483648 (or
   * -2<sup>31</sup>).
   */
  public static final int MIN_VALUE = 0x80000000;

  /**
   * The maximum value an <code>int</code> can represent is 2147483647 (or
   * 2<sup>31</sup> - 1).
   */
  public static final int MAX_VALUE = 0x7fffffff;

  /**
   * The primitive type <code>int</code> is represented by this
   * <code>Class</code> object.
   * @since 1.1
   */
  public static final Class<Integer> TYPE = (Class<Integer>) VMClassLoader.getPrimitiveClass('I');

  /**
   * The number of bits needed to represent an <code>int</code>.
   * @since 1.5
   */
  public static final int SIZE = 32;

  // This caches some Integer values, and is used by boxing
  // conversions via valueOf().  We must cache at least -128..127;
  // these constants control how much we actually cache.
  private static final int MIN_CACHE = -128;
  private static final int MAX_CACHE = 127;
  private static final Integer[] intCache = new Integer[MAX_CACHE - MIN_CACHE + 1];
  static
  {
    for (int i=MIN_CACHE; i <= MAX_CACHE; i++)
      intCache[i - MIN_CACHE] = new Integer(i);
  }

  /**
   * The immutable value of this Integer.
   *
   * @serial the wrapped int
   */
  private final int value;

  /**
   * Create an <code>Integer</code> object representing the value of the
   * <code>int</code> argument.
   *
   * @param value the value to use
   */
  public Integer(int value)
  {
    this.value = value;
  }

  /**
   * Create an <code>Integer</code> object representing the value of the
   * argument after conversion to an <code>int</code>.
   *
   * @param s the string to convert
   * @throws NumberFormatException if the String does not contain an int
   * @see #valueOf(String)
   */
  public Integer(String s)
  {
    value = parseInt(s, 10, false);
  }

  /**
   * Return the size of a string large enough to hold the given number
   *
   * @param num the number we want the string length for (must be positive)
   * @param radix the radix (base) that will be used for the string
   * @return a size sufficient for a string of num
   */
  private static int stringSize(int num, int radix) {
    int exp;
    if (radix < 4)
      {
        exp = 1;
      }
    else if (radix < 8)
      {
        exp = 2;
      }
    else if (radix < 16)
      {
        exp = 3;
      }
    else if (radix < 32)
      {
        exp = 4;
      }
    else
      {
        exp = 5;
      }
    int size=0;
    do
      {
        num >>>= exp;
        size++;
      }
    while(num != 0);
    return size;
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> using
   * the specified radix (base). If the radix exceeds
   * <code>Character.MIN_RADIX</code> or <code>Character.MAX_RADIX</code>, 10
   * is used instead. If the result is negative, the leading character is
   * '-' ('\\u002D'). The remaining characters come from
   * <code>Character.forDigit(digit, radix)</code> ('0'-'9','a'-'z').
   *
   * @param num the <code>int</code> to convert to <code>String</code>
   * @param radix the radix (base) to use in the conversion
   * @return the <code>String</code> representation of the argument
   */
  public static String toString(int num, int radix)
  {
    if (radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      radix = 10;

    // Is the value negative?
    boolean isNeg = num < 0;

    // Is the string a single character?
    if (!isNeg && num < radix)
      return new String(digits, num, 1, true);

    // Compute string size and allocate buffer
    // account for a leading '-' if the value is negative
    int size;
    int i;
    char[] buffer;
    if (isNeg)
      {
        num = -num;

        // When the value is MIN_VALUE, it overflows when made positive
        if (num < 0)
          {
            i = size = stringSize(MAX_VALUE, radix) + 2;
            buffer = new char[size];
            buffer[--i] = digits[(int) (-(num + radix) % radix)];
            num = -(num / radix);
          }
        else
          {
            i = size = stringSize(num, radix) + 1;
            buffer = new char[size];
          }
      }
    else
      {
        i = size = stringSize(num, radix);
        buffer = new char[size];
      }

    do
      {
        buffer[--i] = digits[num % radix];
        num /= radix;
      }
    while (num > 0);

    if (isNeg)
      buffer[--i] = '-';

    // Package constructor avoids an array copy.
    return new String(buffer, i, size - i, true);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> assuming it is
   * unsigned in base 16.
   *
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument
   */
  public static String toHexString(int i)
  {
    return toUnsignedString(i, 4);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> assuming it is
   * unsigned in base 8.
   *
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument
   */
  public static String toOctalString(int i)
  {
    return toUnsignedString(i, 3);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> assuming it is
   * unsigned in base 2.
   *
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument
   */
  public static String toBinaryString(int i)
  {
    return toUnsignedString(i, 1);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> and assumes
   * a radix of 10.
   *
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument
   * @see #toString(int, int)
   */
  public static String toString(int i)
  {
    // This is tricky: in libgcj, String.valueOf(int) is a fast native
    // implementation.  In Classpath it just calls back to
    // Integer.toString(int, int).
    return String.valueOf(i);
  }

  /**
   * Converts the specified <code>String</code> into an <code>int</code>
   * using the specified radix (base). The string must not be <code>null</code>
   * or empty. It may begin with an optional '-', which will negate the answer,
   * provided that there are also valid digits. Each digit is parsed as if by
   * <code>Character.digit(d, radix)</code>, and must be in the range
   * <code>0</code> to <code>radix - 1</code>. Finally, the result must be
   * within <code>MIN_VALUE</code> to <code>MAX_VALUE</code>, inclusive.
   * Unlike Double.parseDouble, you may not have a leading '+'.
   *
   * @param str the <code>String</code> to convert
   * @param radix the radix (base) to use in the conversion
   * @return the <code>String</code> argument converted to <code>int</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as an
   *         <code>int</code>
   */
  public static int parseInt(String str, int radix)
  {
    return parseInt(str, radix, false);
  }

  /**
   * Converts the specified <code>String</code> into an <code>int</code>.
   * This function assumes a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the <code>int</code> value of <code>s</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as an
   *         <code>int</code>
   * @see #parseInt(String, int)
   */
  public static int parseInt(String s)
  {
    return parseInt(s, 10, false);
  }

  /**
   * Creates a new <code>Integer</code> object using the <code>String</code>
   * and specified radix (base).
   *
   * @param s the <code>String</code> to convert
   * @param radix the radix (base) to convert with
   * @return the new <code>Integer</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as an
   *         <code>int</code>
   * @see #parseInt(String, int)
   */
  public static Integer valueOf(String s, int radix)
  {
    return valueOf(parseInt(s, radix, false));
  }

  /**
   * Creates a new <code>Integer</code> object using the <code>String</code>,
   * assuming a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the new <code>Integer</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as an
   *         <code>int</code>
   * @see #Integer(String)
   * @see #parseInt(String)
   */
  public static Integer valueOf(String s)
  {
    return valueOf(parseInt(s, 10, false));
  }

  /**
   * Returns an <code>Integer</code> object wrapping the value.
   * In contrast to the <code>Integer</code> constructor, this method
   * will cache some values.  It is used by boxing conversion.
   *
   * @param val the value to wrap
   * @return the <code>Integer</code>
   */
  public static Integer valueOf(int val)
  {
    if (val < MIN_CACHE || val > MAX_CACHE)
      return new Integer(val);
    else
      return intCache[val - MIN_CACHE];
  }

  /**
   * Return the value of this <code>Integer</code> as a <code>byte</code>.
   *
   * @return the byte value
   */
  public byte byteValue()
  {
    return (byte) value;
  }

  /**
   * Return the value of this <code>Integer</code> as a <code>short</code>.
   *
   * @return the short value
   */
  public short shortValue()
  {
    return (short) value;
  }

  /**
   * Return the value of this <code>Integer</code>.
   * @return the int value
   */
  public int intValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Integer</code> as a <code>long</code>.
   *
   * @return the long value
   */
  public long longValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Integer</code> as a <code>float</code>.
   *
   * @return the float value
   */
  public float floatValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Integer</code> as a <code>double</code>.
   *
   * @return the double value
   */
  public double doubleValue()
  {
    return value;
  }

  /**
   * Converts the <code>Integer</code> value to a <code>String</code> and
   * assumes a radix of 10.
   *
   * @return the <code>String</code> representation
   */
  public String toString()
  {
    return String.valueOf(value);
  }

  /**
   * Return a hashcode representing this Object. <code>Integer</code>'s hash
   * code is simply its value.
   *
   * @return this Object's hash code
   */
  public int hashCode()
  {
    return value;
  }

  /**
   * Returns <code>true</code> if <code>obj</code> is an instance of
   * <code>Integer</code> and represents the same int value.
   *
   * @param obj the object to compare
   * @return whether these Objects are semantically equal
   */
  public boolean equals(Object obj)
  {
    return obj instanceof Integer && value == ((Integer) obj).value;
  }

  /**
   * Get the specified system property as an <code>Integer</code>. The
   * <code>decode()</code> method will be used to interpret the value of
   * the property.
   *
   * @param nm the name of the system property
   * @return the system property as an <code>Integer</code>, or null if the
   *         property is not found or cannot be decoded
   * @throws SecurityException if accessing the system property is forbidden
   * @see System#getProperty(String)
   * @see #decode(String)
   */
  public static Integer getInteger(String nm)
  {
    return getInteger(nm, null);
  }

  /**
   * Get the specified system property as an <code>Integer</code>, or use a
   * default <code>int</code> value if the property is not found or is not
   * decodable. The <code>decode()</code> method will be used to interpret
   * the value of the property.
   *
   * @param nm the name of the system property
   * @param val the default value
   * @return the value of the system property, or the default
   * @throws SecurityException if accessing the system property is forbidden
   * @see System#getProperty(String)
   * @see #decode(String)
   */
  public static Integer getInteger(String nm, int val)
  {
    Integer result = getInteger(nm, null);
    return result == null ? valueOf(val) : result;
  }

  /**
   * Get the specified system property as an <code>Integer</code>, or use a
   * default <code>Integer</code> value if the property is not found or is
   * not decodable. The <code>decode()</code> method will be used to
   * interpret the value of the property.
   *
   * @param nm the name of the system property
   * @param def the default value
   * @return the value of the system property, or the default
   * @throws SecurityException if accessing the system property is forbidden
   * @see System#getProperty(String)
   * @see #decode(String)
   */
  public static Integer getInteger(String nm, Integer def)
  {
    if (nm == null || "".equals(nm))
      return def;
    nm = System.getProperty(nm);
    if (nm == null)
      return def;
    try
      {
        return decode(nm);
      }
    catch (NumberFormatException e)
      {
        return def;
      }
  }

  /**
   * Convert the specified <code>String</code> into an <code>Integer</code>.
   * The <code>String</code> may represent decimal, hexadecimal, or
   * octal numbers.
   *
   * <p>The extended BNF grammar is as follows:<br>
   * <pre>
   * <em>DecodableString</em>:
   *      ( [ <code>-</code> ] <em>DecimalNumber</em> )
   *    | ( [ <code>-</code> ] ( <code>0x</code> | <code>0X</code>
   *              | <code>#</code> ) <em>HexDigit</em> { <em>HexDigit</em> } )
   *    | ( [ <code>-</code> ] <code>0</code> { <em>OctalDigit</em> } )
   * <em>DecimalNumber</em>:
   *        <em>DecimalDigit except '0'</em> { <em>DecimalDigit</em> }
   * <em>DecimalDigit</em>:
   *        <em>Character.digit(d, 10) has value 0 to 9</em>
   * <em>OctalDigit</em>:
   *        <em>Character.digit(d, 8) has value 0 to 7</em>
   * <em>DecimalDigit</em>:
   *        <em>Character.digit(d, 16) has value 0 to 15</em>
   * </pre>
   * Finally, the value must be in the range <code>MIN_VALUE</code> to
   * <code>MAX_VALUE</code>, or an exception is thrown.
   *
   * @param str the <code>String</code> to interpret
   * @return the value of the String as an <code>Integer</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>int</code>
   * @throws NullPointerException if <code>s</code> is null
   * @since 1.2
   */
  public static Integer decode(String str)
  {
    return valueOf(parseInt(str, 10, true));
  }

  /**
   * Compare two Integers numerically by comparing their <code>int</code>
   * values. The result is positive if the first is greater, negative if the
   * second is greater, and 0 if the two are equal.
   *
   * @param i the Integer to compare
   * @return the comparison
   * @since 1.2
   */
  public int compareTo(Integer i)
  {
    if (value == i.value)
      return 0;
    // Returns just -1 or 1 on inequality; doing math might overflow.
    return value > i.value ? 1 : -1;
  }

  /**
   * Compares two unboxed int values.
   * The result is positive if the first is greater, negative if the second
   * is greater, and 0 if the two are equal.
   *
   * @param x First value to compare.
   * @param y Second value to compare.
   *
   * @return positive int if the first value is greater, negative if the second
   * is greater, and 0 if the two are equal.
   * @since 1.7
   */
  public static int compare(int x, int y)
  {
    return Integer.valueOf(x).compareTo(Integer.valueOf(y));
  }

  /**
   * Return the number of bits set in x.
   * @param x value to examine
   * @since 1.5
   */
  public static int bitCount(int x)
  {
    // Successively collapse alternating bit groups into a sum.
    x = ((x >> 1) & 0x55555555) + (x & 0x55555555);
    x = ((x >> 2) & 0x33333333) + (x & 0x33333333);
    x = ((x >> 4) & 0x0f0f0f0f) + (x & 0x0f0f0f0f);
    x = ((x >> 8) & 0x00ff00ff) + (x & 0x00ff00ff);
    return ((x >> 16) & 0x0000ffff) + (x & 0x0000ffff);
  }

  /**
   * Rotate x to the left by distance bits.
   * @param x the value to rotate
   * @param distance the number of bits by which to rotate
   * @since 1.5
   */
  public static int rotateLeft(int x, int distance)
  {
    // This trick works because the shift operators implicitly mask
    // the shift count.
    return (x << distance) | (x >>> - distance);
  }

  /**
   * Rotate x to the right by distance bits.
   * @param x the value to rotate
   * @param distance the number of bits by which to rotate
   * @since 1.5
   */
  public static int rotateRight(int x, int distance)
  {
    // This trick works because the shift operators implicitly mask
    // the shift count.
    return (x << - distance) | (x >>> distance);
  }

  /**
   * Find the highest set bit in value, and return a new value
   * with only that bit set.
   * @param value the value to examine
   * @since 1.5
   */
  public static int highestOneBit(int value)
  {
    value |= value >>> 1;
    value |= value >>> 2;
    value |= value >>> 4;
    value |= value >>> 8;
    value |= value >>> 16;
    return value ^ (value >>> 1);
  }

  /**
   * Return the number of leading zeros in value.
   * @param value the value to examine
   * @since 1.5
   */
  public static int numberOfLeadingZeros(int value)
  {
    value |= value >>> 1;
    value |= value >>> 2;
    value |= value >>> 4;
    value |= value >>> 8;
    value |= value >>> 16;
    return bitCount(~value);
  }

  /**
   * Find the lowest set bit in value, and return a new value
   * with only that bit set.
   * @param value the value to examine
   * @since 1.5
   */
  public static int lowestOneBit(int value)
  {
    // Classic assembly trick.
    return value & - value;
  }

  /**
   * Find the number of trailing zeros in value.
   * @param value the value to examine
   * @since 1.5
   */
  public static int numberOfTrailingZeros(int value)
  {
    return bitCount((value & -value) - 1);
  }

  /**
   * Return 1 if x is positive, -1 if it is negative, and 0 if it is
   * zero.
   * @param x the value to examine
   * @since 1.5
   */
  public static int signum(int x)
  {
    return (x >> 31) | (-x >>> 31);

    // The LHS propagates the sign bit through every bit in the word;
    // if X < 0, every bit is set to 1, else 0.  if X > 0, the RHS
    // negates x and shifts the resulting 1 in the sign bit to the
    // LSB, leaving every other bit 0.

    // Hacker's Delight, Section 2-7
  }

  /**
   * Reverse the bytes in val.
   * @since 1.5
   */
  public static int reverseBytes(int val)
  {
    return (  ((val >> 24) & 0xff)
            | ((val >> 8) & 0xff00)
            | ((val << 8) & 0xff0000)
            | ((val << 24) & 0xff000000));
  }

  /**
   * Reverse the bits in val.
   * @since 1.5
   */
  public static int reverse(int val)
  {
    // Successively swap alternating bit groups.
    val = ((val >> 1) & 0x55555555) + ((val << 1) & ~0x55555555);
    val = ((val >> 2) & 0x33333333) + ((val << 2) & ~0x33333333);
    val = ((val >> 4) & 0x0f0f0f0f) + ((val << 4) & ~0x0f0f0f0f);
    val = ((val >> 8) & 0x00ff00ff) + ((val << 8) & ~0x00ff00ff);
    return ((val >> 16) & 0x0000ffff) + ((val << 16) & ~0x0000ffff);
  }

  /**
   * Helper for converting unsigned numbers to String.
   *
   * @param num the number
   * @param exp log2(digit) (ie. 1, 3, or 4 for binary, oct, hex)
   */
  // Package visible for use by Long.
  static String toUnsignedString(int num, int exp)
  {
    // Compute string length
    int size = 1;
    int copy = num >>> exp;
    while (copy != 0)
      {
        size++;
        copy >>>= exp;
      }
    // Quick path for single character strings
    if (size == 1)
      return new String(digits, num, 1, true);

    // Encode into buffer
    int mask = (1 << exp) - 1;
    char[] buffer = new char[size];
    int i = size;
    do
      {
        buffer[--i] = digits[num & mask];
        num >>>= exp;
      }
    while (num != 0);

    // Package constructor avoids an array copy.
    return new String(buffer, i, size - i, true);
  }

  /**
   * Helper for parsing ints, used by Integer, Short, and Byte.
   *
   * @param str the string to parse
   * @param radix the radix to use, must be 10 if decode is true
   * @param decode if called from decode
   * @return the parsed int value
   * @throws NumberFormatException if there is an error
   * @throws NullPointerException if decode is true and str if null
   * @see #parseInt(String, int)
   * @see #decode(String)
   * @see Byte#parseByte(String, int)
   * @see Short#parseShort(String, int)
   */
  static int parseInt(String str, int radix, boolean decode)
  {
    if (! decode && str == null)
      throw new NumberFormatException();
    int index = 0;
    int len = str.length();
    boolean isNeg = false;
    if (len == 0)
      throw new NumberFormatException("string length is null");
    int ch = str.charAt(index);
    if (ch == '-')
      {
        if (len == 1)
          throw new NumberFormatException("pure '-'");
        isNeg = true;
        ch = str.charAt(++index);
      }
    else if (ch == '+')
      {
        if (len == 1)
          throw new NumberFormatException("pure '+'");
        ch = str.charAt(++index);
      }
    if (decode)
      {
        if (ch == '0')
          {
            if (++index == len)
              return 0;
            if ((str.charAt(index) & ~('x' ^ 'X')) == 'X')
              {
                radix = 16;
                index++;
              }
            else
              radix = 8;
          }
        else if (ch == '#')
          {
            radix = 16;
            index++;
          }
      }
    if (index == len)
      throw new NumberFormatException("non terminated number: " + str);

    int max = MAX_VALUE / radix;
    // We can't directly write `max = (MAX_VALUE + 1) / radix'.
    // So instead we fake it.
    if (isNeg && MAX_VALUE % radix == radix - 1)
      ++max;

    int val = 0;
    while (index < len)
      {
        if (val < 0 || val > max)
          throw new NumberFormatException("number overflow (pos=" + index + ") : " + str);

        ch = Character.digit(str.charAt(index++), radix);
        val = val * radix + ch;
        if (ch < 0 || (val < 0 && (! isNeg || val != MIN_VALUE)))
          throw new NumberFormatException("invalid character at position " + index + " in " + str);
      }
    return isNeg ? -val : val;
  }
}
