/* java.lang.Float
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

/**
 * Instances of class <code>Float</code> represent primitive
 * <code>float</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to floats.
 *
 * @author Paul Fisher
 * @author Andrew Haley <aph@cygnus.com>
 * @since JDK 1.0
 */
public final class Float extends Number implements Comparable
{
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
  public static final float NEGATIVE_INFINITY = -1.0f/0.0f;

  /**
   * The value of a float representation 1.0/0.0, positive infinity.
   */
  public static final float POSITIVE_INFINITY = 1.0f/0.0f;

  /**
   * All IEEE 754 values of NaN have the same value in Java.
   */
  public static final float NaN = 0.0f/0.0f;

  /**
   * The primitive type <code>float</code> is represented by this 
   * <code>Class</code> object.
   */
  public static final Class TYPE = VMClassLoader.getPrimitiveClass('F');

  /**
   * The immutable value of this Float.
   */
  private final float value;

  private static final long serialVersionUID = -2671257302660747028L;

  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
	System.loadLibrary ("javalang");
      }
  }

  /**
   * Create a <code>float</code> from the primitive <code>Float</code>
   * specified.
   *
   * @param value the <code>Float</code> argument
   */
  public Float (float value)
  {
    this.value = value;
  }

  /**
   * Create a <code>Float</code> from the primitive <code>double</code>
   * specified.
   *
   * @param value the <code>double</code> argument
   */
  public Float (double value)
  {
    this.value = (float)value;
  }

  /**
   * Create a <code>Float</code> from the specified <code>String</code>.
   *
   * This method calls <code>Float.parseFloat()</code>.
   *
   * @exception NumberFormatException when the <code>String</code> cannot
   *            be parsed into a <code>Float</code>.
   * @param s the <code>String</code> to convert
   * @see #parseFloat(java.lang.String)
   */
  public Float (String s) throws NumberFormatException
  {
    this.value = parseFloat (s);
  }

  /**
   * Parse the specified <code>String</code> as a <code>float</code>.
   *
   * The number is really read as <em>n * 10<sup>exponent</sup></em>.  The
   * first number is <em>n</em>, and if there is an "<code>E</code>"
   * ("<code>e</code>" is also acceptable), then the integer after that is
   * the exponent.
   * <P>
   * Here are the possible forms the number can take:
   * <BR>
   * <TABLE BORDER=1>
   *     <TR><TH>Form</TH><TH>Examples</TH></TR>
   *     <TR><TD><CODE>[+-]&lt;number&gt;[.]</CODE></TD><TD>345., -10, 12</TD></TR>
   *     <TR><TD><CODE>[+-]&lt;number&gt;.&lt;number&gt;</CODE></TD><TD>40.2, 80.00, -12.30</TD></TR>
   *     <TR><TD><CODE>[+-]&lt;number&gt;[.]E[+-]&lt;number&gt;</CODE></TD><TD>80E12, -12e+7, 4.E-123</TD></TR>
   *     <TR><TD><CODE>[+-]&lt;number&gt;.&lt;number&gt;E[+-]&lt;number&gt;</CODE></TD><TD>6.02e-22, -40.2E+6, 12.3e9</TD></TR>
   * </TABLE>
   *
   * "<code>[+-]</code>" means either a plus or minus sign may go there, or
   * neither, in which case + is assumed.
   * <BR>
   * "<code>[.]</code>" means a dot may be placed here, but is optional.
   * <BR>
   * "<code>&lt;number&gt;</code>" means a string of digits (0-9), basically
   * an integer.  "<code>&lt;number&gt;.&lt;number&gt;</code>" is basically
   * a real number, a floating-point value.
   * <P>
   * Remember that a <code>float</code> has a limited range.  If the
   * number you specify is greater than <code>Float.MAX_VALUE</code> or less
   * than <code>-Float.MAX_VALUE</code>, it will be set at
   * <code>Float.POSITIVE_INFINITY</code> or
   * <code>Float.NEGATIVE_INFINITY</code>, respectively.
   * <P>
   *
   * Note also that <code>float</code> does not have perfect precision.  Many
   * numbers cannot be precisely represented.  The number you specify
   * will be rounded to the nearest representable value.
   * <code>Float.MIN_VALUE</code> is the margin of error for <code>float</code>
   * values.
   * <P>
   * If an unexpected character is found in the <code>String</code>, a
   * <code>NumberFormatException</code> will be thrown.  Spaces are not
   * allowed and will cause this exception to be thrown.
   *
   * @XXX specify where/how we are not in accord with the spec.
   *
   * @param str the <code>String</code> to convert
   * @return the value of the <code>String</code> as a <code>float</code>.
   * @exception NumberFormatException when the string cannot be parsed to a
   *            <code>float</code>.
   * @since JDK 1.2
   * @see #MIN_VALUE
   * @see #MAX_VALUE
   * @see #POSITIVE_INFINITY
   * @see #NEGATIVE_INFINITY
   */
  public static float parseFloat (String s) throws NumberFormatException
  {
    // The spec says that parseFloat() should work like
    // Double.valueOf().  This is equivalent, in our implementation,
    // but more efficient.
    return (float) Double.parseDouble (s);
  }

  /**
   * Convert the <code>float</code> value of this <code>Float</code>
   * to a <code>String</code>.  This method calls
   * <code>Float.toString(float)</code> to do its dirty work.
   *
   * @return the <code>String</code> representation of this <code>Float</code>.
   * @see #toString(float)
   */
  public String toString ()
  {
    return toString (value);
  }

  /**
   * If the <code>Object</code> is not <code>null</code>, is an
   * <code>instanceof</code> <code>Float</code>, and represents
   * the same primitive <code>float</code> value return 
   * <code>true</code>.  Otherwise <code>false</code> is returned.
   * <p>
   * Note that there are two differences between <code>==</code> and
   * <code>equals()</code>. <code>0.0f == -0.0f</code> returns <code>true</code>
   * but <code>new Float(0.0f).equals(new Float(-0.0f))</code> returns
   * <code>false</code>. And <code>Float.NaN == Float.NaN</code> returns
   * <code>false</code>, but
   * <code>new Float(Float.NaN).equals(new Float(Float.NaN))</code> returns
   * <code>true</code>.
   *
   * @param obj the object to compare to
   * @return whether the objects are semantically equal.
   */
  public boolean equals (Object obj)
  {
    if (!(obj instanceof Float))
      return false;

    float f = ((Float) obj).value;

    // GCJ LOCAL: this implementation is probably faster than
    // Classpath's, especially once we inline floatToIntBits.
    return floatToIntBits (value) == floatToIntBits (f);
    // END GCJ LOCAL
  }

  /**
   * Return a hashcode representing this Object.
   * <code>Float</code>'s hash code is calculated by calling the
   * <code>floatToIntBits()</code> function.
   * @return this Object's hash code.
   * @see java.lang.Float.floatToIntBits(float)
   */
  public int hashCode ()
  {
    return floatToIntBits (value);
  }

  /**
   * Return the value of this <code>Double</code> when cast to an 
   * <code>int</code>.
   */
  public int intValue ()
  {
    return (int) value;
  }

  /**
   * Return the value of this <code>Double</code> when cast to a
   * <code>long</code>.
   */
  public long longValue ()
  {
    return (long) value;
  }

  /**
   * Return the value of this <code>Double</code> when cast to a
   * <code>float</code>.
   */
  public float floatValue ()
  {
    return (float) value;
  }

  /**
   * Return the primitive <code>double</code> value represented by this
   * <code>Double</code>.
   */
  public double doubleValue ()
  {
    return (double) value;
  }

  /**
   * Convert the <code>float</code> to a <code>String</code>.
   * <P>
   *
   * Floating-point string representation is fairly complex: here is a
   * rundown of the possible values.  "<CODE>[-]</CODE>" indicates that a
   * negative sign will be printed if the value (or exponent) is negative.
   * "<CODE>&lt;number&gt;</CODE>" means a string of digits (0-9).
   * "<CODE>&lt;digit&gt;</CODE>" means a single digit (0-9).
   * <P>
   *
   * <TABLE BORDER=1>
   * <TR><TH>Value of Float</TH><TH>String Representation</TH></TR>
   * <TR>
   *     <TD>[+-] 0</TD>
   *     <TD>[<CODE>-</CODE>]<CODE>0.0</CODE></TD>
   * </TR>
   * <TR>
   *     <TD>Between [+-] 10<SUP>-3</SUP> and 10<SUP>7</SUP></TD>
   *     <TD><CODE>[-]number.number</CODE></TD>
   * </TR>
   * <TR>
   *     <TD>Other numeric value</TD>
   *     <TD><CODE>[-]&lt;digit&gt;.&lt;number&gt;E[-]&lt;number&gt;</CODE></TD>
   * </TR>
   * <TR>
   *     <TD>[+-] infinity</TD>
   *     <TD><CODE>[-]Infinity</CODE></TD>
   * </TR>
   * <TR>
   *     <TD>NaN</TD>
   *     <TD><CODE>NaN</CODE></TD>
   * </TR>
   * </TABLE>
   *
   * Yes, negative zero <EM>is</EM> a possible value.  Note that there is
   * <EM>always</EM> a <CODE>.</CODE> and at least one digit printed after
   * it: even if the number is 3, it will be printed as <CODE>3.0</CODE>.
   * After the ".", all digits will be printed except trailing zeros.  No
   * truncation or rounding is done by this function.
   *
   * @XXX specify where we are not in accord with the spec.
   *
   * @param f the <code>float</code> to convert
   * @return the <code>String</code> representing the <code>float</code>.
   */
  public static String toString (float f)
  {
    return Double.toString ((double) f, true);
  }

  /**
   * Return the result of calling <code>new Float(java.lang.String)</code>.
   *
   * @param s the <code>String</code> to convert to a <code>Float</code>.
   * @return a new <code>Float</code> representing the <code>String</code>'s
   *         numeric value.
   *
   * @exception NumberFormatException thrown if <code>String</code> cannot
   * be parsed as a <code>double</code>.
   * @see #Float(java.lang.String)
   * @see #parseFloat(java.lang.String)
   */
  public static Float valueOf (String s) throws NumberFormatException
  {
    return new Float (s);
  }

  /**
   * Return <code>true</code> if the value of this <code>Float</code>
   * is the same as <code>NaN</code>, otherwise return <code>false</code>.
   * @return whether this <code>Float</code> is <code>NaN</code>.
   */
  public boolean isNaN ()
  {
    return isNaN (value);
  }

  /**
   * Return <code>true</code> if the <code>float</code> has the same
   * value as <code>NaN</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>float</code> to compare
   * @return whether the argument is <code>NaN</code>.
   */
  public static boolean isNaN (float v)
  {
    // This works since NaN != NaN is the only reflexive inequality
    // comparison which returns true.
    return v != v;
  }

  /**
   * Return <code>true</code> if the value of this <code>Float</code>
   * is the same as <code>NEGATIVE_INFINITY</code> or 
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @return whether this <code>Float</code> is (-/+) infinity.
   */
  public boolean isInfinite ()
  {
    return isInfinite (value);
  }

  /**
   * Return <code>true</code> if the <code>float</code> has a value 
   * equal to either <code>NEGATIVE_INFINITY</code> or 
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>float</code> to compare
   * @return whether the argument is (-/+) infinity.
   */
  public static boolean isInfinite (float v)
  {
    return (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY);
  }

  /**
   * Return the int bits of the specified <code>float</code>.
   * The result of this function can be used as the argument to
   * <code>Float.intBitsToFloat(long)</code> to obtain the
   * original <code>float</code> value.
   *
   * @param value the <code>float</code> to convert
   * @return the bits of the <code>float</code>.
   */
  public static native int floatToIntBits (float value);

  /**
   * Return the int bits of the specified <code>float</code>.
   * The result of this function can be used as the argument to
   * <code>Float.intBitsToFloat(long)</code> to obtain the
   * original <code>float</code> value.  The difference between
   * this function and <code>floatToIntBits</code> is that this
   * function does not collapse NaN values.
   *
   * @param value the <code>float</code> to convert
   * @return the bits of the <code>float</code>.
   */
  public static native int floatToRawIntBits (float value);

  /**
   * Return the <code>float</code> represented by the long
   * bits specified.
   *
   * @param bits the long bits representing a <code>double</code>
   * @return the <code>float</code> represented by the bits.
   */
  public static native float intBitsToFloat (int bits);

  /**
   * Returns 0 if the <code>float</code> value of the argument is 
   * equal to the value of this <code>Float</code>.  Returns a number
   * less than zero if the value of this <code>Float</code> is less 
   * than the <code>Float</code> value of the argument, and returns a 
   * number greater than zero if the value of this <code>Float</code> 
   * is greater than the <code>float</code> value of the argument.
   * <br>
   * <code>Float.NaN</code> is greater than any number other than itself, 
   * even <code>Float.POSITIVE_INFINITY</code>.
   * <br>
   * <code>0.0</code> is greater than <code>-0.0</code>.
   *
   * @param f the Float to compare to.
   * @return  0 if the <code>Float</code>s are the same, &lt; 0 if this
   *          <code>Float</code> is less than the <code>Float</code> in
   *          in question, or &gt; 0 if it is greater.
   *
   * @since 1.2
   */
  public int compareTo (Float f)
  {
    return compare (value, f.value);
  }

  /**
   * Returns 0 if the first argument is equal to the second argument.
   * Returns a number less than zero if the first argument is less than the
   * second argument, and returns a number greater than zero if the first
   * argument is greater than the second argument.
   * <br>
   * <code>Float.NaN</code> is greater than any number other than itself, 
   * even <code>Float.POSITIVE_INFINITY</code>.
   * <br>
   * <code>0.0</code> is greater than <code>-0.0</code>.
   *
   * @param x the first float to compare.
   * @param y the second float to compare.
   * @return  0 if the arguments are the same, &lt; 0 if the
   *          first argument is less than the second argument in
   *          in question, or &gt; 0 if it is greater.
   * @since 1.4
   */
  public static int compare (float x, float y)
  {
    if (isNaN (x))
      return isNaN (y) ? 0 : 1;
    if (isNaN (y))
      return -1;
    // recall that 0.0 == -0.0, so we convert to infinities and try again
    if (x == 0 && y == 0)
      return (int) (1 / x - 1 / y);
    if (x == y)
      return 0;

    return x > y ? 1 : -1;
  }

  /**
   * Compares the specified <code>Object</code> to this <code>Float</code>
   * if and only if the <code>Object</code> is an instanceof 
   * <code>Float</code>.
   * Otherwise it throws a <code>ClassCastException</code>
   *
   * @param o the Object to compare to.
   * @return  0 if the <code>Float</code>s are the same, &lt; 0 if this
   *          <code>Float</code> is less than the <code>Float</code> in
   *          in question, or &gt; 0 if it is greater.
   * @throws ClassCastException if the argument is not a <code>Float</code>
   *
   * @since 1.2
   */
  public int compareTo (Object o)
  {
    return compareTo ((Float) o);
  }
}
