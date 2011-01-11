/* Float.java -- object wrapper for float
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
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

import gnu.java.lang.CPStringBuilder;

/**
 * Instances of class <code>Float</code> represent primitive
 * <code>float</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to floats.
 *
 * @author Paul Fisher
 * @author Andrew Haley (aph@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.0
 * @status partly updated to 1.5
 */
public final class Float extends Number implements Comparable<Float>
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -2671257302660747028L;

  /**
   * The maximum positive value a <code>double</code> may represent
   * is 3.4028235e+38f.
   */
  public static final float MAX_VALUE = 3.4028235e+38f;

  /**
   * The minimum positive value a <code>float</code> may represent
   * is 1.4e-45.
   */
  public static final float MIN_VALUE = 1.4e-45f;

  /**
   * The value of a float representation -1.0/0.0, negative infinity.
   */
  public static final float NEGATIVE_INFINITY = -1.0f / 0.0f;

  /**
   * The value of a float representation 1.0/0.0, positive infinity.
   */
  public static final float POSITIVE_INFINITY = 1.0f / 0.0f;

  /**
   * All IEEE 754 values of NaN have the same value in Java.
   */
  public static final float NaN = 0.0f / 0.0f;

  /**
   * The primitive type <code>float</code> is represented by this
   * <code>Class</code> object.
   * @since 1.1
   */
  public static final Class<Float> TYPE = (Class<Float>) VMClassLoader.getPrimitiveClass('F');

  /**
   * The number of bits needed to represent a <code>float</code>.
   * @since 1.5
   */
  public static final int SIZE = 32;

  /**
   * Cache representation of 0
   */
  private static final Float ZERO = new Float(0.0f);

  /**
   * Cache representation of 1
   */
  private static final Float ONE = new Float(1.0f);

  /**
   * The immutable value of this Float.
   *
   * @serial the wrapped float
   */
  private final float value;

  /**
   * Create a <code>Float</code> from the primitive <code>float</code>
   * specified.
   *
   * @param value the <code>float</code> argument
   */
  public Float(float value)
  {
    this.value = value;
  }

  /**
   * Create a <code>Float</code> from the primitive <code>double</code>
   * specified.
   *
   * @param value the <code>double</code> argument
   */
  public Float(double value)
  {
    this.value = (float) value;
  }

  /**
   * Create a <code>Float</code> from the specified <code>String</code>.
   * This method calls <code>Float.parseFloat()</code>.
   *
   * @param s the <code>String</code> to convert
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>float</code>
   * @throws NullPointerException if <code>s</code> is null
   * @see #parseFloat(String)
   */
  public Float(String s)
  {
    value = parseFloat(s);
  }

  /**
   * Convert the <code>float</code> to a <code>String</code>.
   * Floating-point string representation is fairly complex: here is a
   * rundown of the possible values.  "<code>[-]</code>" indicates that a
   * negative sign will be printed if the value (or exponent) is negative.
   * "<code>&lt;number&gt;</code>" means a string of digits ('0' to '9').
   * "<code>&lt;digit&gt;</code>" means a single digit ('0' to '9').<br>
   *
   * <table border=1>
   * <tr><th>Value of Float</th><th>String Representation</th></tr>
   * <tr><td>[+-] 0</td> <td><code>[-]0.0</code></td></tr>
   * <tr><td>Between [+-] 10<sup>-3</sup> and 10<sup>7</sup>, exclusive</td>
   *     <td><code>[-]number.number</code></td></tr>
   * <tr><td>Other numeric value</td>
   *     <td><code>[-]&lt;digit&gt;.&lt;number&gt;
   *          E[-]&lt;number&gt;</code></td></tr>
   * <tr><td>[+-] infinity</td> <td><code>[-]Infinity</code></td></tr>
   * <tr><td>NaN</td> <td><code>NaN</code></td></tr>
   * </table>
   *
   * Yes, negative zero <em>is</em> a possible value.  Note that there is
   * <em>always</em> a <code>.</code> and at least one digit printed after
   * it: even if the number is 3, it will be printed as <code>3.0</code>.
   * After the ".", all digits will be printed except trailing zeros. The
   * result is rounded to the shortest decimal number which will parse back
   * to the same float.
   *
   * <p>To create other output formats, use {@link java.text.NumberFormat}.
   *
   * @XXX specify where we are not in accord with the spec.
   *
   * @param f the <code>float</code> to convert
   * @return the <code>String</code> representing the <code>float</code>
   */
  public static String toString(float f)
  {
    return VMFloat.toString(f);
  }

  /**
   * Convert a float value to a hexadecimal string.  This converts as
   * follows:
   * <ul>
   * <li> A NaN value is converted to the string "NaN".
   * <li> Positive infinity is converted to the string "Infinity".
   * <li> Negative infinity is converted to the string "-Infinity".
   * <li> For all other values, the first character of the result is '-'
   * if the value is negative.  This is followed by '0x1.' if the
   * value is normal, and '0x0.' if the value is denormal.  This is
   * then followed by a (lower-case) hexadecimal representation of the
   * mantissa, with leading zeros as required for denormal values.
   * The next character is a 'p', and this is followed by a decimal
   * representation of the unbiased exponent.
   * </ul>
   * @param f the float value
   * @return the hexadecimal string representation
   * @since 1.5
   */
  public static String toHexString(float f)
  {
    if (isNaN(f))
      return "NaN";
    if (isInfinite(f))
      return f < 0 ? "-Infinity" : "Infinity";

    int bits = floatToIntBits(f);
    CPStringBuilder result = new CPStringBuilder();

    if (bits < 0)
      result.append('-');
    result.append("0x");

    final int mantissaBits = 23;
    final int exponentBits = 8;
    int mantMask = (1 << mantissaBits) - 1;
    int mantissa = bits & mantMask;
    int expMask = (1 << exponentBits) - 1;
    int exponent = (bits >>> mantissaBits) & expMask;

    result.append(exponent == 0 ? '0' : '1');
    result.append('.');
    // For Float only, we have to adjust the mantissa.
    mantissa <<= 1;
    result.append(Integer.toHexString(mantissa));
    if (exponent == 0 && mantissa != 0)
      {
        // Treat denormal specially by inserting '0's to make
        // the length come out right.  The constants here are
        // to account for things like the '0x'.
        int offset = 4 + ((bits < 0) ? 1 : 0);
        // The silly +3 is here to keep the code the same between
        // the Float and Double cases.  In Float the value is
        // not a multiple of 4.
        int desiredLength = offset + (mantissaBits + 3) / 4;
        while (result.length() < desiredLength)
          result.insert(offset, '0');
      }
    result.append('p');
    if (exponent == 0 && mantissa == 0)
      {
        // Zero, so do nothing special.
      }
    else
      {
        // Apply bias.
        boolean denormal = exponent == 0;
        exponent -= (1 << (exponentBits - 1)) - 1;
        // Handle denormal.
        if (denormal)
          ++exponent;
      }

    result.append(Integer.toString(exponent));
    return result.toString();
  }

  /**
   * Creates a new <code>Float</code> object using the <code>String</code>.
   *
   * @param s the <code>String</code> to convert
   * @return the new <code>Float</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>float</code>
   * @throws NullPointerException if <code>s</code> is null
   * @see #parseFloat(String)
   */
  public static Float valueOf(String s)
  {
    return valueOf(parseFloat(s));
  }

  /**
   * Returns a <code>Float</code> object wrapping the value.
   * In contrast to the <code>Float</code> constructor, this method
   * may cache some values.  It is used by boxing conversion.
   *
   * @param val the value to wrap
   * @return the <code>Float</code>
   * @since 1.5
   */
  public static Float valueOf(float val)
  {
    if ((val == 0.0) && (floatToRawIntBits(val) == 0))
      return ZERO;
    else if (val == 1.0)
      return ONE;
    else
      return new Float(val);
  }

  /**
   * Parse the specified <code>String</code> as a <code>float</code>. The
   * extended BNF grammar is as follows:<br>
   * <pre>
   * <em>DecodableString</em>:
   *      ( [ <code>-</code> | <code>+</code> ] <code>NaN</code> )
   *    | ( [ <code>-</code> | <code>+</code> ] <code>Infinity</code> )
   *    | ( [ <code>-</code> | <code>+</code> ] <em>FloatingPoint</em>
   *              [ <code>f</code> | <code>F</code> | <code>d</code>
   *                | <code>D</code>] )
   * <em>FloatingPoint</em>:
   *      ( { <em>Digit</em> }+ [ <code>.</code> { <em>Digit</em> } ]
   *              [ <em>Exponent</em> ] )
   *    | ( <code>.</code> { <em>Digit</em> }+ [ <em>Exponent</em> ] )
   * <em>Exponent</em>:
   *      ( ( <code>e</code> | <code>E</code> )
   *              [ <code>-</code> | <code>+</code> ] { <em>Digit</em> }+ )
   * <em>Digit</em>: <em><code>'0'</code> through <code>'9'</code></em>
   * </pre>
   *
   * <p>NaN and infinity are special cases, to allow parsing of the output
   * of toString.  Otherwise, the result is determined by calculating
   * <em>n * 10<sup>exponent</sup></em> to infinite precision, then rounding
   * to the nearest float. Remember that many numbers cannot be precisely
   * represented in floating point. In case of overflow, infinity is used,
   * and in case of underflow, signed zero is used. Unlike Integer.parseInt,
   * this does not accept Unicode digits outside the ASCII range.
   *
   * <p>If an unexpected character is found in the <code>String</code>, a
   * <code>NumberFormatException</code> will be thrown.  Leading and trailing
   * 'whitespace' is ignored via <code>String.trim()</code>, but spaces
   * internal to the actual number are not allowed.
   *
   * <p>To parse numbers according to another format, consider using
   * {@link java.text.NumberFormat}.
   *
   * @XXX specify where/how we are not in accord with the spec.
   *
   * @param str the <code>String</code> to convert
   * @return the <code>float</code> value of <code>s</code>
   * @throws NumberFormatException if <code>str</code> cannot be parsed as a
   *         <code>float</code>
   * @throws NullPointerException if <code>str</code> is null
   * @see #MIN_VALUE
   * @see #MAX_VALUE
   * @see #POSITIVE_INFINITY
   * @see #NEGATIVE_INFINITY
   * @since 1.2
   */
  public static float parseFloat(String str)
  {
    return VMFloat.parseFloat(str);
  }

  /**
   * Return <code>true</code> if the <code>float</code> has the same
   * value as <code>NaN</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>float</code> to compare
   * @return whether the argument is <code>NaN</code>
   */
  public static boolean isNaN(float v)
  {
    // This works since NaN != NaN is the only reflexive inequality
    // comparison which returns true.
    return v != v;
  }

  /**
   * Return <code>true</code> if the <code>float</code> has a value
   * equal to either <code>NEGATIVE_INFINITY</code> or
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>float</code> to compare
   * @return whether the argument is (-/+) infinity
   */
  public static boolean isInfinite(float v)
  {
    return v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY;
  }

  /**
   * Return <code>true</code> if the value of this <code>Float</code>
   * is the same as <code>NaN</code>, otherwise return <code>false</code>.
   *
   * @return whether this <code>Float</code> is <code>NaN</code>
   */
  public boolean isNaN()
  {
    return isNaN(value);
  }

  /**
   * Return <code>true</code> if the value of this <code>Float</code>
   * is the same as <code>NEGATIVE_INFINITY</code> or
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @return whether this <code>Float</code> is (-/+) infinity
   */
  public boolean isInfinite()
  {
    return isInfinite(value);
  }

  /**
   * Convert the <code>float</code> value of this <code>Float</code>
   * to a <code>String</code>.  This method calls
   * <code>Float.toString(float)</code> to do its dirty work.
   *
   * @return the <code>String</code> representation
   * @see #toString(float)
   */
  public String toString()
  {
    return toString(value);
  }

  /**
   * Return the value of this <code>Float</code> as a <code>byte</code>.
   *
   * @return the byte value
   * @since 1.1
   */
  public byte byteValue()
  {
    return (byte) value;
  }

  /**
   * Return the value of this <code>Float</code> as a <code>short</code>.
   *
   * @return the short value
   * @since 1.1
   */
  public short shortValue()
  {
    return (short) value;
  }

  /**
   * Return the value of this <code>Integer</code> as an <code>int</code>.
   *
   * @return the int value
   */
  public int intValue()
  {
    return (int) value;
  }

  /**
   * Return the value of this <code>Integer</code> as a <code>long</code>.
   *
   * @return the long value
   */
  public long longValue()
  {
    return (long) value;
  }

  /**
   * Return the value of this <code>Float</code>.
   *
   * @return the float value
   */
  public float floatValue()
  {
    return value;
  }

  /**
   * Return the value of this <code>Float</code> as a <code>double</code>
   *
   * @return the double value
   */
  public double doubleValue()
  {
    return value;
  }

  /**
   * Return a hashcode representing this Object. <code>Float</code>'s hash
   * code is calculated by calling <code>floatToIntBits(floatValue())</code>.
   *
   * @return this Object's hash code
   * @see #floatToIntBits(float)
   */
  public int hashCode()
  {
    return floatToIntBits(value);
  }

  /**
   * Returns <code>true</code> if <code>obj</code> is an instance of
   * <code>Float</code> and represents the same float value. Unlike comparing
   * two floats with <code>==</code>, this treats two instances of
   * <code>Float.NaN</code> as equal, but treats <code>0.0</code> and
   * <code>-0.0</code> as unequal.
   *
   * <p>Note that <code>f1.equals(f2)</code> is identical to
   * <code>floatToIntBits(f1.floatValue()) ==
   *    floatToIntBits(f2.floatValue())</code>.
   *
   * @param obj the object to compare
   * @return whether the objects are semantically equal
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof Float)
      {
        float f = ((Float) obj).value;
        return (floatToRawIntBits(value) == floatToRawIntBits(f)) ||
          (isNaN(value) && isNaN(f));
      }
    return false;
  }

  /**
   * Convert the float to the IEEE 754 floating-point "single format" bit
   * layout. Bit 31 (the most significant) is the sign bit, bits 30-23
   * (masked by 0x7f800000) represent the exponent, and bits 22-0
   * (masked by 0x007fffff) are the mantissa. This function collapses all
   * versions of NaN to 0x7fc00000. The result of this function can be used
   * as the argument to <code>Float.intBitsToFloat(int)</code> to obtain the
   * original <code>float</code> value.
   *
   * @param value the <code>float</code> to convert
   * @return the bits of the <code>float</code>
   * @see #intBitsToFloat(int)
   */
  public static int floatToIntBits(float value)
  {
    if (isNaN(value))
      return 0x7fc00000;
    else
      return VMFloat.floatToRawIntBits(value);
  }

  /**
   * Convert the float to the IEEE 754 floating-point "single format" bit
   * layout. Bit 31 (the most significant) is the sign bit, bits 30-23
   * (masked by 0x7f800000) represent the exponent, and bits 22-0
   * (masked by 0x007fffff) are the mantissa. This function leaves NaN alone,
   * rather than collapsing to a canonical value. The result of this function
   * can be used as the argument to <code>Float.intBitsToFloat(int)</code> to
   * obtain the original <code>float</code> value.
   *
   * @param value the <code>float</code> to convert
   * @return the bits of the <code>float</code>
   * @see #intBitsToFloat(int)
   */
  public static int floatToRawIntBits(float value)
  {
    return VMFloat.floatToRawIntBits(value);
  }

  /**
   * Convert the argument in IEEE 754 floating-point "single format" bit
   * layout to the corresponding float. Bit 31 (the most significant) is the
   * sign bit, bits 30-23 (masked by 0x7f800000) represent the exponent, and
   * bits 22-0 (masked by 0x007fffff) are the mantissa. This function leaves
   * NaN alone, so that you can recover the bit pattern with
   * <code>Float.floatToRawIntBits(float)</code>.
   *
   * @param bits the bits to convert
   * @return the <code>float</code> represented by the bits
   * @see #floatToIntBits(float)
   * @see #floatToRawIntBits(float)
   */
  public static float intBitsToFloat(int bits)
  {
    return VMFloat.intBitsToFloat(bits);
  }

  /**
   * Compare two Floats numerically by comparing their <code>float</code>
   * values. The result is positive if the first is greater, negative if the
   * second is greater, and 0 if the two are equal. However, this special
   * cases NaN and signed zero as follows: NaN is considered greater than
   * all other floats, including <code>POSITIVE_INFINITY</code>, and positive
   * zero is considered greater than negative zero.
   *
   * @param f the Float to compare
   * @return the comparison
   * @since 1.2
   */
  public int compareTo(Float f)
  {
    return compare(value, f.value);
  }

  /**
   * Behaves like <code>new Float(x).compareTo(new Float(y))</code>; in
   * other words this compares two floats, special casing NaN and zero,
   * without the overhead of objects.
   *
   * @param x the first float to compare
   * @param y the second float to compare
   * @return the comparison
   * @since 1.4
   */
  public static int compare(float x, float y)
  {
      // handle the easy cases:
      if (x < y)
          return -1;
      if (x > y)
          return 1;

      // handle equality respecting that 0.0 != -0.0 (hence not using x == y):
      int ix = floatToRawIntBits(x);
      int iy = floatToRawIntBits(y);
      if (ix == iy)
          return 0;

      // handle NaNs:
      if (x != x)
          return (y != y) ? 0 : 1;
      else if (y != y)
          return -1;

      // handle +/- 0.0
      return (ix < iy) ? -1 : 1;
  }
}
