/* java.lang.Long
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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
 * Instances of class <code>Double</code> represent primitive
 * <code>double</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to longs.
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Warren Levy
 * @since JDK 1.0
 */
public final class Long extends Number implements Comparable
{
  // compatible with JDK 1.0.2+
  static final long serialVersionUID = 4290774380558885855L;

  /**
   * The minimum value a <code>long</code> can represent is
   * -9223372036854775808.
   */
  public static final long MIN_VALUE = 0x8000000000000000L;

  /**
   * The maximum value a <code>long</code> can represent is
   * 9223372036854775807.
   */
  public static final long MAX_VALUE = 0x7fffffffffffffffL;

  /**
   * The primitive type <code>long</code> is represented by this 
   * <code>Class</code> object.
   */
  public static final Class TYPE = VMClassLoader.getPrimitiveClass ('J');

  /**
   * The immutable value of this Long.
   */
  private final long value;

  /**
   * Create a <code>Long</code> object representing the value of the 
   * <code>long</code> argument.
   *
   * @param value the value to use
   */
  public Long(long value)
  {
    this.value = value;
  }

  /**
   * Create a <code>Long</code> object representing the value of the 
   * argument after conversion to a <code>long</code>.
   *
   * @param s the string to convert.
   */
  public Long(String s) throws NumberFormatException
  {
    value = parseLong(s, 10);
  }

  /**
   * If the <code>Object</code> is not <code>null</code>, is an
   * <code>instanceof</code> <code>Long</code>, and represents
   * the same primitive <code>long</code> value return 
   * <code>true</code>.  Otherwise <code>false</code> is returned.
   */
  public boolean equals(Object obj)
  {
    return obj instanceof Long && ((Long)obj).value == value;
  }

  /**
   * Return a hashcode representing this Object.
   *
   * <code>Long</code>'s hash code is calculated by simply returning its
   * value.
   *
   * @return this Object's hash code.
   */
  public int hashCode()
  {
    return (int)(value^(value>>>32));
  }

  /**
   * Get the specified system property as a <code>Long</code>.
   *
   * A method similar to <code>Integer</code>'s <code>decode()</code> will be
   * used to interpret the value of the property.
   * 
   * @param nm the name of the system property
   * @return the system property as an <code>Long</code>, or
   *         <code>null</code> if the property is not found or cannot be
   *         decoded as a <code>Long</code>.
   * @see java.lang.System#getProperty(java.lang.String)
   * @see java.lang.Integer#decode(int)
   */
  public static Long getLong(String nm)
  {
    return getLong(nm, null);
  }

  /**
   * Get the specified system property as an <code>Long</code>, or use a
   * default <code>long</code> value if the property is not found or is not
   * decodable.
   * 
   * A method similar to <code>Integer</code>'s <code>decode()</code> will be
   * used to interpret the value of the property.
   * 
   * @param nm the name of the system property
   * @param val the default value to use if the property is not found or not
   *        a number.
   * @return the system property as a <code>Long</code>, or the default
   *         value if the property is not found or cannot be decoded as a
   *         <code>Long</code>.
   * @see java.lang.System#getProperty(java.lang.String)
   * @see java.lang.Integer#decode(int)
   * @see #getLong(java.lang.String,java.lang.Long)
   */
  public static Long getLong(String nm, long val)
  {
    Long result = getLong(nm, null);
    return (result == null) ? new Long(val) : result;
  }

  /**
   * Get the specified system property as an <code>Long</code>, or use a
   * default <code>Long</code> value if the property is not found or is
   * not decodable.
   * 
   * The <code>decode()</code> method will be used to interpret the value of
   * the property.
   *
   * @param nm the name of the system property
   * @param val the default value to use if the property is not found or not
   *        a number.
   * @return the system property as an <code>Long</code>, or the default
   *         value if the property is not found or cannot be decoded as an
   *         <code>Long</code>.
   * @see java.lang.System#getProperty(java.lang.String)
   * @see java.lang.Integer#decode(int)
   * @see #getLong(java.lang.String,long)
   */
  public static Long getLong(String nm, Long def)
  {
    nm = System.getProperty(nm);
    if (nm == null || "".equals(nm))
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

  private static String toUnsignedString(long num, int exp)
  {
    // Use an array large enough for a binary number.
    int radix = 1 << exp;
    int mask = radix - 1;
    char[] buffer = new char[64];
    int i = 64;
    do
      {
        buffer[--i] = Character.forDigit((int) num & mask, radix);
        num = num >>> exp;
      }
    while (num != 0);

    return String.valueOf(buffer, i, 64-i);
  }

  /**
   * Converts the <code>long</code> to a <code>String</code> assuming it is
   * unsigned in base 16.
   * @param i the <code>long</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */
  public static String toHexString(long i)
  {
    return toUnsignedString(i, 4);
  }

  /**
   * Converts the <code>long</code> to a <code>String</code> assuming it is
   * unsigned in base 8.
   * @param i the <code>long</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */
  public static String toOctalString(long i)
  {
    return toUnsignedString(i, 3);
  }

  /**
   * Converts the <code>long</code> to a <code>String</code> assuming it is
   * unsigned in base 2.
   * @param i the <code>long</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */
  public static String toBinaryString(long i) {
    return toUnsignedString(i, 1);
  }

  /**
   * Converts the <code>long</code> to a <code>String</code> and assumes
   * a radix of 10.
   * @param num the <code>long</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */    
  public static String toString(long num)
  {
    // Use the Integer toString for efficiency if possible.
    if (num <= Integer.MAX_VALUE && num >= Integer.MIN_VALUE)
      return Integer.toString((int) num);

    // Use an array large enough for "-9223372036854775808"; i.e. 20 chars.
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

  /**
   * Converts the <code>Long</code> value to a <code>String</code> and
   * assumes a radix of 10.
   * @return the <code>String</code> representation of this <code>Long</code>.
   */    
  public String toString()
  {
    return toString(value);
  }
  
  /**
   * Converts the <code>long</code> to a <code>String</code> using
   * the specified radix (base).
   * @param num the <code>long</code> to convert to <code>String</code>.
   * @param radix the radix (base) to use in the conversion.
   * @return the <code>String</code> representation of the argument.
   */
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
    
  /**
   * Creates a new <code>Long</code> object using the <code>String</code>,
   * assuming a radix of 10.
   * @param s the <code>String</code> to convert.
   * @return the new <code>Long</code>.
   * @see #Long(java.lang.String)
   * @see #parseLong(java.lang.String)
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>long</code>.
   */
  public static Long valueOf(String s) throws NumberFormatException
  {
    return new Long(parseLong(s));
  }

  /**
   * Creates a new <code>Long</code> object using the <code>String</code>
   * and specified radix (base).
   * @param s the <code>String</code> to convert.
   * @param radix the radix (base) to convert with.
   * @return the new <code>Long</code>.
   * @see #parseLong(java.lang.String,int)
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>long</code>.
   */
  public static Long valueOf(String s, int radix) throws NumberFormatException
  {
    return new Long(parseLong(s, radix));
  }

  /**
   * Converts the specified <code>String</code> into a <code>long</code>.
   * This function assumes a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the <code>long</code> value of the <code>String</code>
   *         argument.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>long</code>.
   */
  public static long parseLong(String s) throws NumberFormatException
  {
    return parseLong(s, 10);
  }

  /**
   * Converts the specified <code>String</code> into a <code>long</code>
   * using the specified radix (base).
   *
   * @param s the <code>String</code> to convert
   * @param radix the radix (base) to use in the conversion
   * @return the <code>String</code> argument converted to </code>long</code>.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>long</code>.    
   */
  public static long parseLong(String str, int radix)
    throws NumberFormatException
  {
    final int len;

    if ((len = str.length()) == 0 || radix < Character.MIN_RADIX 
         || radix > Character.MAX_RADIX)
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

  public static Long decode(String str) throws NumberFormatException
  {
    boolean isNeg = false;
    int index = 0;
    int radix = 10;
    final int len;

    if ((len = str.length()) == 0)
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

  /** Return the value of this <code>Long</code> as an <code>short</code>.
   ** @return the value of this <code>Long</code> as an <code>short</code>.
   **/
  public byte byteValue()
  {
    return (byte) value;
  }

  /** Return the value of this <code>Long</code> as an <code>short</code>.
   ** @return the value of this <code>Long</code> as an <code>short</code>.
   **/
  public short shortValue()
  {
    return (short) value;
  }

  /** Return the value of this <code>Long</code> as an <code>int</code>.
   ** @return the value of this <code>Long</code> as an <code>int</code>.
   **/
  public int intValue()
  {
    return (int) value;
  }

  /** Return the value of this <code>Long</code> as a <code>long</code>.
   ** @return the value of this <code>Long</code> as a <code>long</code>.
   **/
  public long longValue()
  {
    return value;
  }

  /** Return the value of this <code>Long</code> as a <code>float</code>.
   ** @return the value of this <code>Long</code> as a <code>float</code>.
   **/
  public float floatValue()
  {
    return value;
  }

  /** Return the value of this <code>Long</code> as a <code>double</code>.
   ** @return the value of this <code>Long</code> as a <code>double</code>.
   **/
  public double doubleValue()
  {
    return value;
  }

  /**
   * Compare two Longs numerically by comparing their
   * <code>long</code> values.
   * @return a positive value if this <code>Long</code> is greater
   * in value than the argument <code>Long</code>; a negative value
   * if this <code>Long</code> is smaller in value than the argument
   * <code>Long</code>; and <code>0</code>, zero, if this
   * <code>Long</code> is equal in value to the argument
   * <code>Long</code>.  
   *
   * @since 1.2
   */
  public int compareTo(Long l)
  {
    if (this.value == l.value)
      return 0;

    // Returns just -1 or 1 on inequality; doing math might overflow the long.
    if (this.value > l.value)
      return 1;

    return -1;
  }
    
  /**
   * Behaves like <code>compareTo(java.lang.Long)</code> unless the Object
   * is not a <code>Long</code>.  Then it throws a 
   * <code>ClassCastException</code>.
   * @exception ClassCastException if the argument is not a
   * <code>Long</code>.
   *
   * @since 1.2
   */
  public int compareTo(Object o)
  {
    return compareTo((Long)o);
  }
}
