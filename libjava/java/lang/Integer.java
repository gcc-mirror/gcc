/* java.lang.Integer
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
 * Instances of class <code>Integer</code> represent primitive
 * <code>int</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to ints.
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Warren Levy
 * @since JDK 1.0
 */
public final class Integer extends Number implements Comparable
{
  // compatible with JDK 1.0.2+
  private static final long serialVersionUID = 1360826667806852920L;

  /**
   * The minimum value an <code>int</code> can represent is -2147483648.
   */
  public static final int MIN_VALUE = 0x80000000;

  /**
   * The maximum value an <code>int</code> can represent is 2147483647.
   */
  public static final int MAX_VALUE = 0x7fffffff;

  /**
   * The primitive type <code>int</code> is represented by this 
   * <code>Class</code> object.
   */
  public static final Class TYPE = VMClassLoader.getPrimitiveClass ('I');

  /**
   * The immutable value of this Integer.
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
   * @param s the string to convert.
   */
  public Integer(String s) throws NumberFormatException
  {
    value = parseInt(s, 10);
  }

  /**
   * Return a hashcode representing this Object.
   *
   * <code>Integer</code>'s hash code is calculated by simply returning its
   * value.
   *
   * @return this Object's hash code.
   */
  public int hashCode()
  {
    return value;
  }

  /**
   * If the <code>Object</code> is not <code>null</code>, is an
   * <code>instanceof</code> <code>Integer</code>, and represents
   * the same primitive <code>int</code> value return 
   * <code>true</code>.  Otherwise <code>false</code> is returned.
   */
  public boolean equals(Object obj)
  {
    return obj instanceof Integer && value == ((Integer)obj).value;
  }

  /**
   * Get the specified system property as an <code>Integer</code>.
   *
   * The <code>decode()</code> method will be used to interpret the value of
   * the property.
   * @param nm the name of the system property
   * @return the system property as an <code>Integer</code>, or
   *         <code>null</code> if the property is not found or cannot be
   *         decoded as an <code>Integer</code>.
   * @see java.lang.System#getProperty(java.lang.String)
   * @see #decode(int)
   */
  public static Integer getInteger(String nm)
  {
    return getInteger(nm, null);
  }

  /**
   * Get the specified system property as an <code>Integer</code>, or use a
   * default <code>int</code> value if the property is not found or is not
   * decodable.
   * 
   * The <code>decode()</code> method will be used to interpret the value of
   * the property.
   *
   * @param nm the name of the system property
   * @param val the default value to use if the property is not found or not
   *        a number.
   * @return the system property as an <code>Integer</code>, or the default
   *         value if the property is not found or cannot be decoded as an
   *         <code>Integer</code>.
   * @see java.lang.System#getProperty(java.lang.String)
   * @see #decode(int)
   * @see #getInteger(java.lang.String,java.lang.Integer)
   */
  public static Integer getInteger(String nm, int val)
  {
    Integer result = getInteger(nm, null);
    return (result == null) ? new Integer(val) : result;
  }

  /**
   * Get the specified system property as an <code>Integer</code>, or use a
   * default <code>Integer</code> value if the property is not found or is
   * not decodable.
   * 
   * The <code>decode()</code> method will be used to interpret the value of
   * the property.
   *
   * @param nm the name of the system property
   * @param val the default value to use if the property is not found or not
   *        a number.
   * @return the system property as an <code>Integer</code>, or the default
   *         value if the property is not found or cannot be decoded as an
   *         <code>Integer</code>.
   * @see java.lang.System#getProperty(java.lang.String)
   * @see #decode(int)
   * @see #getInteger(java.lang.String,int)
   */
  public static Integer getInteger(String nm, Integer def)
  {
    if (nm == null || "".equals(nm))
      return def;
    nm = System.getProperty(nm);
    if (nm == null) return def;
    try
      {
	return decode(nm);
      }
    catch (NumberFormatException e)
      {
	return def;
      }
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

  /**
   * Converts the <code>int</code> to a <code>String</code> assuming it is
   * unsigned in base 16.
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */
  public static String toHexString(int i)
  {
    return toUnsignedString(i, 4);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> assuming it is
   * unsigned in base 8.
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */
  public static String toOctalString(int i)
  {
    return toUnsignedString(i, 3);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> assuming it is
   * unsigned in base 2.
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */
  public static String toBinaryString(int i)
  {
    return toUnsignedString(i, 1);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> and assumes
   * a radix of 10.
   * @param i the <code>int</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument.
   */
  public static String toString(int i)
  {
    // This is tricky: in libgcj, String.valueOf(int) is a fast native
    // implementation.  In Classpath it just calls back to
    // Integer.toString(int,int).
    return String.valueOf (i);
  }

  /**
   * Converts the <code>Integer</code> value to a <code>String</code> and
   * assumes a radix of 10.
   * @return the <code>String</code> representation of this <code>Integer</code>.
   */    
  public String toString()
  {
    return toString (value);
  }

  /**
   * Converts the <code>int</code> to a <code>String</code> using
   * the specified radix (base).
   * @param i the <code>int</code> to convert to <code>String</code>.
   * @param radix the radix (base) to use in the conversion.
   * @return the <code>String</code> representation of the argument.
   */
  public static String toString(int num, int radix)
  {
    if (radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      radix = 10;

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

  /**
   * Creates a new <code>Integer</code> object using the <code>String</code>,
   * assuming a radix of 10.
   * @param s the <code>String</code> to convert.
   * @return the new <code>Integer</code>.
   * @see #Integer(java.lang.String)
   * @see #parseInt(java.lang.String)
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as an <code>int</code>.
   */
  public static Integer valueOf(String s) throws NumberFormatException
  {
    return new Integer(parseInt(s));
  }

  /**
   * Creates a new <code>Integer</code> object using the <code>String</code>
   * and specified radix (base).
   * @param s the <code>String</code> to convert.
   * @param radix the radix (base) to convert with.
   * @return the new <code>Integer</code>.
   * @see #parseInt(java.lang.String,int)
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as an <code>int</code>.
   */
  public static Integer valueOf(String s, int radix)
    throws NumberFormatException
  {
    return new Integer(parseInt(s, radix));
  }

  /**
   * Converts the specified <code>String</code> into an <code>int</code>.
   * This function assumes a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the <code>int</code> value of the <code>String</code>
   *         argument.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as an <code>int</code>.
   */
  public static int parseInt(String s) throws NumberFormatException
  {
    return parseInt(s, 10);
  }

  /**
   * Converts the specified <code>String</code> into an <code>int</code>
   * using the specified radix (base).
   *
   * @param s the <code>String</code> to convert
   * @param radix the radix (base) to use in the conversion
   * @return the <code>String</code> argument converted to </code>int</code>.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as a <code>int</code>.    
   */
  public static int parseInt(String str, int radix)
    throws NumberFormatException
  {
    final int len;

    if (str == null)
      throw new NumberFormatException ();

    if ((len = str.length()) == 0 ||
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
			      int radix)
    throws NumberFormatException
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

  /**
   * Convert the specified <code>String</code> into an <code>Integer</code>.
   * The <code>String</code> may represent decimal, hexadecimal, or 
   * octal numbers.
   *
   * The <code>String</code> argument is interpreted based on the leading
   * characters.  Depending on what the String begins with (after an optional
   * minus sign), the base will be interpreted differently:
   *
   * <table border=1>
   * <tr><th>Leading<br>Characters</th><th>Base</th></tr>
   * <tr><td>#</td><td>16</td></tr>
   * <tr><td>0x</td><td>16</td></tr>
   * <tr><td>0X</td><td>16</td></tr>
   * <tr><td>0</td><td>8</td></tr>
   * <tr><td>Anything<br>Else</td><td>10</td></tr>
   * </table>
   *
   * If the String starts with a minus sign the result is negated.
   *
   * @param str the <code>String</code> to interpret.
   * @return the value of the String as an <code>Integer</code>.
   * @exception NumberFormatException thrown if the <code>String</code> 
   * cannot be parsed as an <code>int</code>.    
   */
  public static Integer decode(String str) throws NumberFormatException
  {
    boolean isNeg = false;
    int index = 0;
    int radix = 10;
    final int len;

    if ((len = str.length()) == 0)
      throw new NumberFormatException("empty string");

    if (str.charAt(index) == '-')
      {
        // The minus sign should be followed by at least one more char
        if (len > 1)
          {
            isNeg = true;
            index++;
          }
        else
          throw new NumberFormatException();
      }

    if (str.charAt(index) == '#')
      {
        radix = 16;
        index++;
      }
    else if (str.charAt(index) == '0')
      {
        index++;

        // Check if str is just "0" or "-0"
        if (len == index)
          return new Integer(0);

        if (str.charAt(index) == 'x' || str.charAt(index) == 'X')
          {
            radix = 16;
            index++;
          }
        else
          radix = 8;
      }

    if (index >= len)
      throw new NumberFormatException("empty value");

    return new Integer(parseInt(str, index, len, isNeg, radix));
  }

  /** Return the value of this <code>Integer</code> as a <code>byte</code>.
   ** @return the value of this <code>Integer</code> as a <code>byte</code>.
   **/
  public byte byteValue()
  {
    return (byte) value;
  }

  /** Return the value of this <code>Integer</code> as a <code>short</code>.
   ** @return the value of this <code>Integer</code> as a <code>short</code>.
   **/
  public short shortValue()
  {
    return (short) value;
  }

  /** Return the value of this <code>Integer</code> as an <code>int</code>.
   ** @return the value of this <code>Integer</code> as an <code>int</code>.
   **/
  public int intValue()
  {
    return value;
  }

  /** Return the value of this <code>Integer</code> as a <code>long</code>.
   ** @return the value of this <code>Integer</code> as a <code>long</code>.
   **/
  public long longValue()
  {
    return value;
  }

  /** Return the value of this <code>Integer</code> as a <code>float</code>.
   ** @return the value of this <code>Integer</code> as a <code>float</code>.
   **/
  public float floatValue()
  {
    return value;
  }

  /** Return the value of this <code>Integer</code> as a <code>double</code>.
   ** @return the value of this <code>Integer</code> as a <code>double</code>.
   **/
  public double doubleValue()
  {
    return value;
  }

  /**
   * Compare two Integers numerically by comparing their
   * <code>int</code> values.
   * @return a positive value if this <code>Integer</code> is greater
   * in value than the argument <code>Integer</code>; a negative value
   * if this <code>Integer</code> is smaller in value than the argument
   * <code>Integer</code>; and <code>0</code>, zero, if this
   * <code>Integer</code> is equal in value to the argument
   * <code>Integer</code>.  
   *
   * @since 1.2
   */
  public int compareTo(Integer i)
  {
    if (this.value == i.value)
      return 0;

    // Returns just -1 or 1 on inequality; doing math might overflow.
    if (this.value > i.value)
      return 1;

    return -1;
  }

  /**
   * Behaves like <code>compareTo(java.lang.Integer)</code> unless the Object
   * is not a <code>Integer</code>.  Then it throws a 
   * <code>ClassCastException</code>.
   * @exception ClassCastException if the argument is not a
   * <code>Integer</code>.
   *
   * @since 1.2
   */
  public int compareTo(Object o)
  {
    return compareTo((Integer)o);
  }
}
