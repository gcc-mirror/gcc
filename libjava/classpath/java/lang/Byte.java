/* Byte.java -- object wrapper for byte
   Copyright (C) 1998, 2001, 2002, 2005  Free Software Foundation, Inc.

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
 * Instances of class <code>Byte</code> represent primitive <code>byte</code>
 * values.
 *
 * Additionally, this class provides various helper functions and variables
 * useful to bytes.
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Per Bothner
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.1
 * @status updated to 1.5
 */
public final class Byte extends Number implements Comparable
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -7183698231559129828L;

  /**
   * The minimum value a <code>byte</code> can represent is -128 (or
   * -2<sup>7</sup>).
   */
  public static final byte MIN_VALUE = -128;

  /**
   * The maximum value a <code>byte</code> can represent is 127 (or
   * 2<sup>7</sup> - 1).
   */
  public static final byte MAX_VALUE = 127;

  /**
   * The primitive type <code>byte</code> is represented by this
   * <code>Class</code> object.
   */
  public static final Class TYPE = VMClassLoader.getPrimitiveClass('B');

  /**
   * The number of bits needed to represent a <code>byte</code>.
   * @since 1.5
   */
  public static final int SIZE = 8;

  // This caches Byte values, and is used by boxing conversions via
  // valueOf().  We're required to cache all possible values here.
  private static Byte[] byteCache = new Byte[MAX_VALUE - MIN_VALUE + 1];

  /**
   * The immutable value of this Byte.
   *
   * @serial the wrapped byte
   */
  private final byte value;

  /**
   * Create a <code>Byte</code> object representing the value of the
   * <code>byte</code> argument.
   *
   * @param value the value to use
   */
  public Byte(byte value)
  {
    this.value = value;
  }

  /**
   * Create a <code>Byte</code> object representing the value specified
   * by the <code>String</code> argument
   *
   * @param s the string to convert
   * @throws NumberFormatException if the String does not contain a byte
   * @see #valueOf(String)
   */
  public Byte(String s)
  {
    value = parseByte(s, 10);
  }

  /**
   * Converts the <code>byte</code> to a <code>String</code> and assumes
   * a radix of 10.
   *
   * @param b the <code>byte</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument
   */
  public static String toString(byte b)
  {
    return String.valueOf(b);
  }

  /**
   * Converts the specified <code>String</code> into a <code>byte</code>.
   * This function assumes a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the <code>byte</code> value of <code>s</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>byte</code>
   * @see #parseByte(String)
   */
  public static byte parseByte(String s)
  {
    return parseByte(s, 10);
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
   * @return the <code>String</code> argument converted to <code>byte</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>byte</code>
   */
  public static byte parseByte(String s, int radix)
  {
    int i = Integer.parseInt(s, radix, false);
    if ((byte) i != i)
      throw new NumberFormatException();
    return (byte) i;
  }

  /**
   * Creates a new <code>Byte</code> object using the <code>String</code>
   * and specified radix (base).
   *
   * @param s the <code>String</code> to convert
   * @param radix the radix (base) to convert with
   * @return the new <code>Byte</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>byte</code>
   * @see #parseByte(String, int)
   */
  public static Byte valueOf(String s, int radix)
  {
    return new Byte(parseByte(s, radix));
  }

  /**
   * Creates a new <code>Byte</code> object using the <code>String</code>,
   * assuming a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the new <code>Byte</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>byte</code>
   * @see #Byte(String)
   * @see #parseByte(String)
   */
  public static Byte valueOf(String s)
  {
    return new Byte(parseByte(s, 10));
  }

  /**
   * Returns a <code>Byte</code> object wrapping the value.
   * In contrast to the <code>Byte</code> constructor, this method
   * will cache some values.  It is used by boxing conversion.
   *
   * @param val the value to wrap
   * @return the <code>Byte</code>
   * 
   * @since 1.5
   */
  public static Byte valueOf(byte val)
  {
    synchronized (byteCache)
      {
    if (byteCache[val - MIN_VALUE] == null)
      byteCache[val - MIN_VALUE] = new Byte(val);
    return byteCache[val - MIN_VALUE];
      }
  }

 /**
   * Convert the specified <code>String</code> into a <code>Byte</code>.
   * The <code>String</code> may represent decimal, hexadecimal, or
   * octal numbers.
   *
   * <p>The extended BNF grammar is as follows:<br>
   * <pre>
   * <em>DecodableString</em>:
   *      ( [ <code>-</code> ] <em>DecimalNumber</em> )
   *    | ( [ <code>-</code> ] ( <code>0x</code> | <code>0X</code>
   *              | <code>#</code> ) { <em>HexDigit</em> }+ )
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
   * @return the value of the String as a <code>Byte</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>byte</code>
   * @throws NullPointerException if <code>s</code> is null
   * @see Integer#decode(String)
   */
  public static Byte decode(String s)
  {
    int i = Integer.parseInt(s, 10, true);
    if ((byte) i != i)
      throw new NumberFormatException();
    return new Byte((byte) i);
  }

  /**
   * Return the value of this <code>Byte</code>.
   *
   * @return the byte value
   */
  public byte byteValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Byte</code> as a <code>short</code>.
   *
   * @return the short value
   */
  public short shortValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Byte</code> as an <code>int</code>.
   *
   * @return the int value
   */
  public int intValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Byte</code> as a <code>long</code>.
   *
   * @return the long value
   */
  public long longValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Byte</code> as a <code>float</code>.
   *
   * @return the float value
   */
  public float floatValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Byte</code> as a <code>double</code>.
   *
   * @return the double value
   */
  public double doubleValue()
  {
    return value;
  }

  /**
   * Converts the <code>Byte</code> value to a <code>String</code> and
   * assumes a radix of 10.
   *
   * @return the <code>String</code> representation of this <code>Byte</code>
   * @see Integer#toString()
   */
  public String toString()
  {
    return String.valueOf(value);
  }

  /**
   * Return a hashcode representing this Object. <code>Byte</code>'s hash
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
   * <code>Byte</code> and represents the same byte value.
   *
   * @param obj the object to compare
   * @return whether these Objects are semantically equal
   */
  public boolean equals(Object obj)
  {
    return obj instanceof Byte && value == ((Byte) obj).value;
  }

  /**
   * Compare two Bytes numerically by comparing their <code>byte</code> values.
   * The result is positive if the first is greater, negative if the second
   * is greater, and 0 if the two are equal.
   *
   * @param b the Byte to compare
   * @return the comparison
   * @since 1.2
   */
  public int compareTo(Byte b)
  {
    return value - b.value;
  }

  /**
   * Behaves like <code>compareTo(Byte)</code> unless the Object
   * is not a <code>Byte</code>.
   *
   * @param o the object to compare
   * @return the comparison
   * @throws ClassCastException if the argument is not a <code>Byte</code>
   * @see #compareTo(Byte)
   * @see Comparable
   * @since 1.2
   */
  public int compareTo(Object o)
  {
    return compareTo((Byte) o);
  }
}
