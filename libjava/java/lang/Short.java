/* Short.java -- object wrapper for short
   Copyright (C) 1998, 2001, 2002 Free Software Foundation, Inc.

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
 * Instances of class <code>Short</code> represent primitive
 * <code>short</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to shorts.
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.1
 * @status updated to 1.4
 */
public final class Short extends Number implements Comparable
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 7515723908773894738L;

  /**
   * The minimum value a <code>short</code> can represent is -32768 (or
   * -2<sup>15</sup).
   */
  public static final short MIN_VALUE = -32768;

  /**
   * The minimum value a <code>short</code> can represent is 32767 (or
   * 2<sup>15</sup).
   */
  public static final short MAX_VALUE = 32767;

  /**
   * The primitive type <code>short</code> is represented by this
   * <code>Class</code> object.
   */
  public static final Class TYPE = VMClassLoader.getPrimitiveClass('S');

  /**
   * The immutable value of this Short.
   *
   * @serial the wrapped short
   */
  private final short value;

  /**
   * Create a <code>Short</code> object representing the value of the
   * <code>short</code> argument.
   *
   * @param value the value to use
   */
  public Short(short value)
  {
    this.value = value;
  }

  /**
   * Create a <code>Short</code> object representing the value of the
   * argument after conversion to a <code>short</code>.
   *
   * @param s the string to convert
   * @throws NumberFormatException if the String cannot be parsed
   */
  public Short(String s)
  {
    value = parseShort(s, 10);
  }

  /**
   * Converts the <code>short</code> to a <code>String</code> and assumes
   * a radix of 10.
   *
   * @param s the <code>short</code> to convert to <code>String</code>
   * @return the <code>String</code> representation of the argument
   */
  public static String toString(short s)
  {
    return String.valueOf(s);
  }

  /**
   * Converts the specified <code>String</code> into a <code>short</code>.
   * This function assumes a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the <code>short</code> value of <code>s</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>short</code>
   */
  public static short parseShort(String s)
  {
    return parseShort(s, 10);
  }

  /**
   * Converts the specified <code>String</code> into a <code>short</code>
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
   * @return the <code>String</code> argument converted to </code>short</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>short</code>
   */
  public static short parseShort(String s, int radix)
  {
    int i = Integer.parseInt(s, radix, false);
    if ((short) i != i)
      throw new NumberFormatException();
    return (short) i;
  }

  /**
   * Creates a new <code>Short</code> object using the <code>String</code>
   * and specified radix (base).
   *
   * @param s the <code>String</code> to convert
   * @param radix the radix (base) to convert with
   * @return the new <code>Short</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>short</code>
   * @see #parseShort(String, int)
   */
  public static Short valueOf(String s, int radix)
  {
    return new Short(parseShort(s, radix));
  }

  /**
   * Creates a new <code>Short</code> object using the <code>String</code>,
   * assuming a radix of 10.
   *
   * @param s the <code>String</code> to convert
   * @return the new <code>Short</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>short</code>
   * @see #Short(String)
   * @see #parseShort(String)
   */
  public static Short valueOf(String s)
  {
    return new Short(parseShort(s, 10));
  }

  /**
   * Convert the specified <code>String</code> into a <code>Short</code>.
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
   * @return the value of the String as a <code>Short</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>short</code>
   * @throws NullPointerException if <code>s</code> is null
   * @see Integer#decode(String)
   */
  public static Short decode(String s)
  {
    int i = Integer.parseInt(s, 10, true);
    if ((short) i != i)
      throw new NumberFormatException();
    return new Short((short) i);
  }

  /**
   * Return the value of this <code>Short</code> as a <code>byte</code>.
   *
   * @return the byte value
   */
  public byte byteValue()
  {
    return (byte) value;
  }

  /**
   * Return the value of this <code>Short</code>.
   *
   * @return the short value
   */
  public short shortValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Short</code> as an <code>int</code>.
   *
   * @return the int value
   */
  public int intValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Short</code> as a <code>long</code>.
   *
   * @return the long value
   */
  public long longValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Short</code> as a <code>float</code>.
   *
   * @return the float value
   */
  public float floatValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Short</code> as a <code>double</code>.
   *
   * @return the double value
   */
  public double doubleValue()
  {
    return value;
  }

  /**
   * Converts the <code>Short</code> value to a <code>String</code> and
   * assumes a radix of 10.
   *
   * @return the <code>String</code> representation of this <code>Short</code>
   */
  public String toString()
  {
    return String.valueOf(value);
  }

  /**
   * Return a hashcode representing this Object. <code>Short</code>'s hash
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
   * <code>Short</code> and represents the same short value.
   *
   * @param obj the object to compare
   * @return whether these Objects are semantically equal
   */
  public boolean equals(Object obj)
  {
    return obj instanceof Short && value == ((Short) obj).value;
  }

  /**
   * Compare two Shorts numerically by comparing their <code>short</code>
   * values. The result is positive if the first is greater, negative if the
   * second is greater, and 0 if the two are equal.
   *
   * @param s the Short to compare
   * @return the comparison
   * @since 1.2
   */
  public int compareTo(Short s)
  {
    return value - s.value;
  }

  /**
   * Behaves like <code>compareTo(Short)</code> unless the Object
   * is not a <code>Short</code>.
   *
   * @param o the object to compare
   * @return the comparison
   * @throws ClassCastException if the argument is not a <code>Short</code>
   * @see #compareTo(Short)
   * @see Comparable
   * @since 1.2
   */
  public int compareTo(Object o)
  {
    return compareTo((Short)o);
  }
}
