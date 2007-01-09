/* Double.java -- object wrapper for double
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


/**
 * Instances of class <code>Double</code> represent primitive
 * <code>double</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to doubles.
 *
 * @author Paul Fisher
 * @author Andrew Haley (aph@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.0
 * @status partly updated to 1.5
 */
public final class Double extends Number implements Comparable<Double>
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -9172774392245257468L;

  /**
   * The maximum positive value a <code>double</code> may represent
   * is 1.7976931348623157e+308.
   */
  public static final double MAX_VALUE = 1.7976931348623157e+308;

  /**
   * The minimum positive value a <code>double</code> may represent
   * is 5e-324.
   */
  public static final double MIN_VALUE = 5e-324;

  /**
   * The value of a double representation -1.0/0.0, negative
   * infinity.
   */
  public static final double NEGATIVE_INFINITY = -1.0 / 0.0;

  /**
   * The value of a double representing 1.0/0.0, positive infinity.
   */
  public static final double POSITIVE_INFINITY = 1.0 / 0.0;

  /**
   * All IEEE 754 values of NaN have the same value in Java.
   */
  public static final double NaN = 0.0 / 0.0;

  /**
   * The number of bits needed to represent a <code>double</code>.
   * @since 1.5
   */
  public static final int SIZE = 64;

 /**
   * The primitive type <code>double</code> is represented by this
   * <code>Class</code> object.
   * @since 1.1
   */
  public static final Class<Double> TYPE = (Class<Double>) VMClassLoader.getPrimitiveClass('D');

  /**
   * The immutable value of this Double.
   *
   * @serial the wrapped double
   */
  private final double value;

  /**
   * Create a <code>Double</code> from the primitive <code>double</code>
   * specified.
   *
   * @param value the <code>double</code> argument
   */
  public Double(double value)
  {
    this.value = value;
  }

  /**
   * Create a <code>Double</code> from the specified <code>String</code>.
   * This method calls <code>Double.parseDouble()</code>.
   *
   * @param s the <code>String</code> to convert
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>double</code>
   * @throws NullPointerException if <code>s</code> is null
   * @see #parseDouble(String)
   */
  public Double(String s)
  {
    value = parseDouble(s);
  }

  /**
   * Convert the <code>double</code> to a <code>String</code>.
   * Floating-point string representation is fairly complex: here is a
   * rundown of the possible values.  "<code>[-]</code>" indicates that a
   * negative sign will be printed if the value (or exponent) is negative.
   * "<code>&lt;number&gt;</code>" means a string of digits ('0' to '9').
   * "<code>&lt;digit&gt;</code>" means a single digit ('0' to '9').<br>
   *
   * <table border=1>
   * <tr><th>Value of Double</th><th>String Representation</th></tr>
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
   * to the same double.
   *
   * <p>To create other output formats, use {@link java.text.NumberFormat}.
   *
   * @XXX specify where we are not in accord with the spec.
   *
   * @param d the <code>double</code> to convert
   * @return the <code>String</code> representing the <code>double</code>
   */
  public static String toString(double d)
  {
    return VMDouble.toString(d, false);
  }

  /**
   * Convert a double value to a hexadecimal string.  This converts as
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
   * @param d the double value
   * @return the hexadecimal string representation
   * @since 1.5
   */
  public static String toHexString(double d)
  {
    if (isNaN(d))
      return "NaN";
    if (isInfinite(d))
      return d < 0 ? "-Infinity" : "Infinity";

    long bits = doubleToLongBits(d);
    StringBuilder result = new StringBuilder();
    
    if (bits < 0)
      result.append('-');
    result.append("0x");

    final int mantissaBits = 52;
    final int exponentBits = 11;
    long mantMask = (1L << mantissaBits) - 1;
    long mantissa = bits & mantMask;
    long expMask = (1L << exponentBits) - 1;
    long exponent = (bits >>> mantissaBits) & expMask;

    result.append(exponent == 0 ? '0' : '1');
    result.append('.');
    result.append(Long.toHexString(mantissa));
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

    result.append(Long.toString(exponent));
    return result.toString();
  }

  /**
   * Returns a <code>Double</code> object wrapping the value.
   * In contrast to the <code>Double</code> constructor, this method
   * may cache some values.  It is used by boxing conversion.
   *
   * @param val the value to wrap
   * @return the <code>Double</code>
   * @since 1.5
   */
  public static Double valueOf(double val)
  {
    // We don't actually cache, but we could.
    return new Double(val);
  }

 /**
   * Create a new <code>Double</code> object using the <code>String</code>.
   *
   * @param s the <code>String</code> to convert
   * @return the new <code>Double</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>double</code>
   * @throws NullPointerException if <code>s</code> is null.
   * @see #parseDouble(String)
   */
  public static Double valueOf(String s)
  {
    return new Double(parseDouble(s));
  }

  /**
   * Parse the specified <code>String</code> as a <code>double</code>. The
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
   * to the nearest double. Remember that many numbers cannot be precisely
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
   * @return the <code>double</code> value of <code>s</code>
   * @throws NumberFormatException if <code>s</code> cannot be parsed as a
   *         <code>double</code>
   * @throws NullPointerException if <code>s</code> is null
   * @see #MIN_VALUE
   * @see #MAX_VALUE
   * @see #POSITIVE_INFINITY
   * @see #NEGATIVE_INFINITY
   * @since 1.2
   */
  public static double parseDouble(String str)
  {
    return VMDouble.parseDouble(str);
  }

  /**
   * Return <code>true</code> if the <code>double</code> has the same
   * value as <code>NaN</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>double</code> to compare
   * @return whether the argument is <code>NaN</code>.
   */
  public static boolean isNaN(double v)
  {
    // This works since NaN != NaN is the only reflexive inequality
    // comparison which returns true.
    return v != v;
  }

  /**
   * Return <code>true</code> if the <code>double</code> has a value
   * equal to either <code>NEGATIVE_INFINITY</code> or
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>double</code> to compare
   * @return whether the argument is (-/+) infinity.
   */
  public static boolean isInfinite(double v)
  {
    return v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY;
  }

  /**
   * Return <code>true</code> if the value of this <code>Double</code>
   * is the same as <code>NaN</code>, otherwise return <code>false</code>.
   *
   * @return whether this <code>Double</code> is <code>NaN</code>
   */
  public boolean isNaN()
  {
    return isNaN(value);
  }

  /**
   * Return <code>true</code> if the value of this <code>Double</code>
   * is the same as <code>NEGATIVE_INFINITY</code> or
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @return whether this <code>Double</code> is (-/+) infinity
   */
  public boolean isInfinite()
  {
    return isInfinite(value);
  }

  /**
   * Convert the <code>double</code> value of this <code>Double</code>
   * to a <code>String</code>.  This method calls
   * <code>Double.toString(double)</code> to do its dirty work.
   *
   * @return the <code>String</code> representation
   * @see #toString(double)
   */
  public String toString()
  {
    return toString(value);
  }

  /**
   * Return the value of this <code>Double</code> as a <code>byte</code>.
   *
   * @return the byte value
   * @since 1.1
   */
  public byte byteValue()
  {
    return (byte) value;
  }

  /**
   * Return the value of this <code>Double</code> as a <code>short</code>.
   *
   * @return the short value
   * @since 1.1
   */
  public short shortValue()
  {
    return (short) value;
  }

  /**
   * Return the value of this <code>Double</code> as an <code>int</code>.
   *
   * @return the int value
   */
  public int intValue()
  {
    return (int) value;
  }

  /**
   * Return the value of this <code>Double</code> as a <code>long</code>.
   *
   * @return the long value
   */
  public long longValue()
  {
    return (long) value;
  }

  /**
   * Return the value of this <code>Double</code> as a <code>float</code>.
   *
   * @return the float value
   */
  public float floatValue()
  {
    return (float) value;
  }

  /**
   * Return the value of this <code>Double</code>.
   *
   * @return the double value
   */
  public double doubleValue()
  {
    return value;
  }

  /**
   * Return a hashcode representing this Object. <code>Double</code>'s hash
   * code is calculated by:<br>
   * <code>long v = Double.doubleToLongBits(doubleValue());<br>
   *    int hash = (int)(v^(v&gt;&gt;32))</code>.
   *
   * @return this Object's hash code
   * @see #doubleToLongBits(double)
   */
  public int hashCode()
  {
    long v = doubleToLongBits(value);
    return (int) (v ^ (v >>> 32));
  }

  /**
   * Returns <code>true</code> if <code>obj</code> is an instance of
   * <code>Double</code> and represents the same double value. Unlike comparing
   * two doubles with <code>==</code>, this treats two instances of
   * <code>Double.NaN</code> as equal, but treats <code>0.0</code> and
   * <code>-0.0</code> as unequal.
   *
   * <p>Note that <code>d1.equals(d2)</code> is identical to
   * <code>doubleToLongBits(d1.doubleValue()) ==
   *    doubleToLongBits(d2.doubleValue())</code>.
   *
   * @param obj the object to compare
   * @return whether the objects are semantically equal
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof Double))
      return false;

    double d = ((Double) obj).value;

    // Avoid call to native method. However, some implementations, like gcj,
    // are better off using floatToIntBits(value) == floatToIntBits(f).
    // Check common case first, then check NaN and 0.
    if (value == d)
      return (value != 0) || (1 / value == 1 / d);
    return isNaN(value) && isNaN(d);
  }

  /**
   * Convert the double to the IEEE 754 floating-point "double format" bit
   * layout. Bit 63 (the most significant) is the sign bit, bits 62-52
   * (masked by 0x7ff0000000000000L) represent the exponent, and bits 51-0
   * (masked by 0x000fffffffffffffL) are the mantissa. This function
   * collapses all versions of NaN to 0x7ff8000000000000L. The result of this
   * function can be used as the argument to
   * <code>Double.longBitsToDouble(long)</code> to obtain the original
   * <code>double</code> value.
   *
   * @param value the <code>double</code> to convert
   * @return the bits of the <code>double</code>
   * @see #longBitsToDouble(long)
   */
  public static long doubleToLongBits(double value)
  {
    return VMDouble.doubleToLongBits(value);
  }

  /**
   * Convert the double to the IEEE 754 floating-point "double format" bit
   * layout. Bit 63 (the most significant) is the sign bit, bits 62-52
   * (masked by 0x7ff0000000000000L) represent the exponent, and bits 51-0
   * (masked by 0x000fffffffffffffL) are the mantissa. This function
   * leaves NaN alone, rather than collapsing to a canonical value. The
   * result of this function can be used as the argument to
   * <code>Double.longBitsToDouble(long)</code> to obtain the original
   * <code>double</code> value.
   *
   * @param value the <code>double</code> to convert
   * @return the bits of the <code>double</code>
   * @see #longBitsToDouble(long)
   */
  public static long doubleToRawLongBits(double value)
  {
    return VMDouble.doubleToRawLongBits(value);
  }

  /**
   * Convert the argument in IEEE 754 floating-point "double format" bit
   * layout to the corresponding float. Bit 63 (the most significant) is the
   * sign bit, bits 62-52 (masked by 0x7ff0000000000000L) represent the
   * exponent, and bits 51-0 (masked by 0x000fffffffffffffL) are the mantissa.
   * This function leaves NaN alone, so that you can recover the bit pattern
   * with <code>Double.doubleToRawLongBits(double)</code>.
   *
   * @param bits the bits to convert
   * @return the <code>double</code> represented by the bits
   * @see #doubleToLongBits(double)
   * @see #doubleToRawLongBits(double)
   */
  public static double longBitsToDouble(long bits)
  {
    return VMDouble.longBitsToDouble(bits);
  }

  /**
   * Compare two Doubles numerically by comparing their <code>double</code>
   * values. The result is positive if the first is greater, negative if the
   * second is greater, and 0 if the two are equal. However, this special
   * cases NaN and signed zero as follows: NaN is considered greater than
   * all other doubles, including <code>POSITIVE_INFINITY</code>, and positive
   * zero is considered greater than negative zero.
   *
   * @param d the Double to compare
   * @return the comparison
   * @since 1.2
   */
  public int compareTo(Double d)
  {
    return compare(value, d.value);
  }

  /**
   * Behaves like <code>new Double(x).compareTo(new Double(y))</code>; in
   * other words this compares two doubles, special casing NaN and zero,
   * without the overhead of objects.
   *
   * @param x the first double to compare
   * @param y the second double to compare
   * @return the comparison
   * @since 1.4
   */
  public static int compare(double x, double y)
  {
    if (isNaN(x))
      return isNaN(y) ? 0 : 1;
    if (isNaN(y))
      return -1;
    // recall that 0.0 == -0.0, so we convert to infinites and try again
    if (x == 0 && y == 0)
      return (int) (1 / x - 1 / y);
    if (x == y)
      return 0;

    return x > y ? 1 : -1;
  }
}
