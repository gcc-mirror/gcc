/* Integer.java -- object wrapper for int
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.0
 * @status updated to 1.4
 */
public final class Integer extends Number implements Comparable
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
  public static final Class TYPE = VMClassLoader.getPrimitiveClass('I');

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

    // For negative numbers, print out the absolute value w/ a leading '-'.
    // Use an array large enough for a binary number.
    char[] buffer = new char[33];
    int i = 33;
    boolean isNeg = false;
    if (num < 0)
      {
        isNeg = true;
        num = -num;

        // When the value is MIN_VALUE, it overflows when made positive
        if (num < 0)
	  {
	    buffer[--i] = digits[(int) (-(num + radix) % radix)];
	    num = -(num / radix);
	  }
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
    return new String(buffer, i, 33 - i, true);
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
   * @param s the <code>String</code> to convert
   * @param radix the radix (base) to use in the conversion
   * @return the <code>String</code> argument converted to </code>int</code>
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
    return new Integer(parseInt(s, radix, false));
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
    return new Integer(parseInt(s, 10, false));
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
    return result == null ? new Integer(val) : result;
  }

  /**
   * Get the specified system property as an <code>Integer</code>, or use a
   * default <code>Integer</code> value if the property is not found or is
   * not decodable. The <code>decode()</code> method will be used to
   * interpret the value of the property.
   *
   * @param nm the name of the system property
   * @param val the default value
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
   * @param s the <code>String</code> to interpret
   * @return the value of the String as an <code>Integer</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>int</code>
   * @throws NullPointerException if <code>s</code> is null
   * @since 1.2
   */
  public static Integer decode(String str)
  {
    return new Integer(parseInt(str, 10, true));
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
   * Behaves like <code>compareTo(Integer)</code> unless the Object
   * is not an <code>Integer</code>.
   *
   * @param o the object to compare
   * @return the comparison
   * @throws ClassCastException if the argument is not an <code>Integer</code>
   * @see #compareTo(Integer)
   * @see Comparable
   * @since 1.2
   */
  public int compareTo(Object o)
  {
    return compareTo((Integer) o);
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
    // Use an array large enough for a binary number.
    int mask = (1 << exp) - 1;
    char[] buffer = new char[32];
    int i = 32;
    do
      {
        buffer[--i] = digits[num & mask];
        num >>>= exp;
      }
    while (num != 0);

    // Package constructor avoids an array copy.
    return new String(buffer, i, 32 - i, true);
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
   * @see Byte#parseInt(String, int)
   * @see Short#parseInt(String, int)
   */
  static int parseInt(String str, int radix, boolean decode)
  {
    if (! decode && str == null)
      throw new NumberFormatException();
    int index = 0;
    int len = str.length();
    boolean isNeg = false;
    if (len == 0)
      throw new NumberFormatException();
    int ch = str.charAt(index);
    if (ch == '-')
      {
        if (len == 1)
          throw new NumberFormatException();
        isNeg = true;
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
      throw new NumberFormatException();

    int max = MAX_VALUE / radix;
    // We can't directly write `max = (MAX_VALUE + 1) / radix'.
    // So instead we fake it.
    if (isNeg && MAX_VALUE % radix == radix - 1)
      ++max;

    int val = 0;
    while (index < len)
      {
	if (val < 0 || val > max)
	  throw new NumberFormatException();

        ch = Character.digit(str.charAt(index++), radix);
        val = val * radix + ch;
        if (ch < 0 || (val < 0 && (! isNeg || val != MIN_VALUE)))
          throw new NumberFormatException();
      }
    return isNeg ? -val : val;
  }
}
