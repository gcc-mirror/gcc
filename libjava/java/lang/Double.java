/* Double.java -- object wrapper for double primitive
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
 * Instances of class <code>Double</code> represent primitive
 * <code>double</code> values.
 *
 * Additionally, this class provides various helper functions and variables
 * related to doubles.
 *
 * @author Paul Fisher
 * @author Andrew Haley <aph@cygnus.com>
 * @since JDK 1.0
 */
public final class Double extends Number implements Comparable
{
  /**
   * The minimum positive value a <code>double</code> may represent
   * is 5e-324.
   */
  public static final double MIN_VALUE = 5e-324;

  /**
   * The maximum positive value a <code>double</code> may represent
   * is 1.7976931348623157e+308.
   */
  public static final double MAX_VALUE = 1.7976931348623157e+308;

  /**
   * The value of a double representation -1.0/0.0, negative
   * infinity.  
   */
  public static final double NEGATIVE_INFINITY = -1.0d/0.0d;

  /**
   * The value of a double representing 1.0/0.0, positive infinity.
   */
  public static final double POSITIVE_INFINITY = 1.0d/0.0d;

  /**
   * All IEEE 754 values of NaN have the same value in Java.
   */
  public static final double NaN = 0.0d/0.0d;

  /**
   * The primitive type <code>double</code> is represented by this
   * <code>Class</code> object.
   */
  public static final Class TYPE = VMClassLoader.getPrimitiveClass('D');

  /**
   * The immutable value of this Double.
   */
  private final double value;

  private static final long serialVersionUID = -9172774392245257468L;

  /**
   * Load native routines necessary for this class.  
   */
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
	System.loadLibrary ("javalang");
	initIDs ();
      }
  }

  /**
   * Create a <code>Double</code> from the primitive <code>double</code>
   * specified.
   *
   * @param value the <code>double</code> argument
   */
  public Double (double value)
  {
    this.value = value;
  }

  /**
   * Create a <code>Double</code> from the specified
   * <code>String</code>.
   *
   * This method calls <code>Double.parseDouble()</code>.
   *
   * @exception NumberFormatException when the <code>String</code> cannot
   *            be parsed into a <code>Float</code>.
   * @param s the <code>String</code> to convert
   * @see #parseDouble(java.lang.String) 
   */
  public Double (String s) throws NumberFormatException
  {
    value = parseDouble (s);
  }

  /**
   * Convert the <code>double</code> value of this <code>Double</code>
   * to a <code>String</code>.  This method calls
   * <code>Double.toString(double)</code> to do its dirty work.
   *
   * @return the <code>String</code> representation of this <code>Double</code>.
   * @see #toString(double)
   */
  public String toString ()
  {
    return toString (value);
  }

  /**
   * If the <code>Object</code> is not <code>null</code>, is an
   * <code>instanceof</code> <code>Double</code>, and represents
   * the same primitive <code>double</code> value return 
   * <code>true</code>.  Otherwise <code>false</code> is returned.
   * <p>
   * Note that there are two differences between <code>==</code> and
   * <code>equals()</code>. <code>0.0d == -0.0d</code> returns <code>true</code>
   * but <code>new Double(0.0d).equals(new Double(-0.0d))</code> returns
   * <code>false</code>. And <code>Double.NaN == Double.NaN</code> returns
   * <code>false</code>, but
   * <code>new Double(Double.NaN).equals(new Double(Double.NaN))</code> returns
   * <code>true</code>.
   *
   * @param obj the object to compare to
   * @return whether the objects are semantically equal.
   */
  public boolean equals (Object obj)
  {
    if (!(obj instanceof Double))
      return false;

    double d = ((Double) obj).value;

    // GCJ LOCAL: this implementation is probably faster than
    // Classpath's, especially once we inline doubleToLongBits.
    return doubleToLongBits (value) == doubleToLongBits (d);
    // END GCJ LOCAL
  }

  /**
   * The hashcode is the value of the expression: <br>
   * <br>
   * <code>(int)(v^(v>>>32))</code><br>
   * <br>
   * where v is defined by: <br>
   * <code>long v = Double.doubleToLongBits(this.longValue());</code><br>
   */
  public int hashCode ()
  {
    long v = doubleToLongBits (value);
    return (int) (v ^ (v >>> 32));
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
    return value;
  }

  /**
   * Return the result of calling <code>new Double(java.lang.String)</code>.
   *
   * @param s the <code>String</code> to convert to a <code>Double</code>.
   * @return a new <code>Double</code> representing the <code>String</code>'s
   *         numeric value.
   *
   * @exception NullPointerException thrown if <code>String</code> is 
   * <code>null</code>.
   * @exception NumberFormatException thrown if <code>String</code> cannot
   * be parsed as a <code>double</code>.
   * @see #Double(java.lang.String)
   * @see #parseDouble(java.lang.String)
   */
  public static Double valueOf (String s) throws NumberFormatException
  {
    return new Double (s);
  }

  /**
   * Return <code>true</code> if the value of this <code>Double</code>
   * is the same as <code>NaN</code>, otherwise return <code>false</code>.
   * @return whether this <code>Double</code> is <code>NaN</code>.
   */
  public boolean isNaN ()
  {
    return isNaN (value);
  }

  /**
   * Return <code>true</code> if the <code>double</code> has the same
   * value as <code>NaN</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>double</code> to compare
   * @return whether the argument is <code>NaN</code>.
   */
  public static boolean isNaN (double v)
  {
    // This works since NaN != NaN is the only reflexive inequality
    // comparison which returns true.
    return v != v;
  }

  /**
   * Return <code>true</code> if the value of this <code>Double</code>
   * is the same as <code>NEGATIVE_INFINITY</code> or 
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @return whether this <code>Double</code> is (-/+) infinity.
   */
  public boolean isInfinite ()
  {
    return isInfinite (value);
  }

  /**
   * Return <code>true</code> if the <code>double</code> has a value 
   * equal to either <code>NEGATIVE_INFINITY</code> or 
   * <code>POSITIVE_INFINITY</code>, otherwise return <code>false</code>.
   *
   * @param v the <code>double</code> to compare
   * @return whether the argument is (-/+) infinity.
   */
  public static boolean isInfinite (double v)
  {
    return (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY);
  }

  /**
   * Returns 0 if the <code>double</code> value of the argument is 
   * equal to the value of this <code>Double</code>.  Returns a number
   * less than zero if the value of this <code>Double</code> is less 
   * than the <code>double</code> value of the argument, and returns a 
   * number greater than zero if the value of this <code>Double</code> 
   * is greater than the <code>double</code> value of the argument.
   * <br>
   * <code>Double.NaN</code> is greater than any number other than itself, 
   * even <code>Double.POSITIVE_INFINITY</code>.
   * <br>
   * <code>0.0d</code> is greater than <code>-0.0d</code>.
   *
   * @param d the Double to compare to.
   * @return  0 if the <code>Double</code>s are the same, &lt; 0 if this
   *          <code>Double</code> is less than the <code>Double</code> in
   *          in question, or &gt; 0 if it is greater.
   * @since 1.2
   */
  public int compareTo (Double d)
  {
    return compare (value, d.value);
  }

  /**
   * Returns 0 if the first argument is equal to the second argument.
   * Returns a number less than zero if the first argument is less than the
   * second argument, and returns a number greater than zero if the first
   * argument is greater than the second argument.
   * <br>
   * <code>Double.NaN</code> is greater than any number other than itself, 
   * even <code>Double.POSITIVE_INFINITY</code>.
   * <br>
   * <code>0.0d</code> is greater than <code>-0.0d</code>.
   *
   * @param x the first double to compare.
   * @param y the second double to compare.
   * @return  0 if the arguments are the same, &lt; 0 if the
   *          first argument is less than the second argument in
   *          in question, or &gt; 0 if it is greater.
   * @since 1.4
   */
  public static int compare (double x, double y)
  {
    if (isNaN (x))
      return isNaN (y) ? 0 : 1;
    if (isNaN (y))
      return -1;
    // recall that 0.0 == -0.0, so we convert to infinites and try again
    if (x == 0 && y == 0)
      return (int) (1 / x - 1 / y);
    if (x == y)
      return 0;

    return x > y ? 1 : -1;
  }

  /**
   * Compares the specified <code>Object</code> to this <code>Double</code>
   * if and only if the <code>Object</code> is an instanceof 
   * <code>Double</code>.
   *
   * @param o the Object to compare to.
   * @return  0 if the <code>Double</code>s are the same, &lt; 0 if this
   *          <code>Double</code> is less than the <code>Double</code> in
   *          in question, or &gt; 0 if it is greater.
   * @throws ClassCastException if the argument is not a <code>Double</code>
   */
  public int compareTo (Object o)
  {
    return compareTo ((Double) o);
  }

  /**
   * Convert the <code>double</code> to a <code>String</code>.
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
   *
   * @XXX specify where we are not in accord with the spec.
   *
   * @param d the <code>double</code> to convert
   * @return the <code>String</code> representing the <code>double</code>.
   */
  public static String toString (double d)
  {
    return toString (d, false);
  }

  static native String toString (double d, boolean isFloat);

  /**
   * Return the long bits of the specified <code>double</code>.
   * The result of this function can be used as the argument to
   * <code>Double.longBitsToDouble(long)</code> to obtain the
   * original <code>double</code> value.
   *
   * @param value the <code>double</code> to convert
   * @return the bits of the <code>double</code>.
   */
  public static native long doubleToLongBits (double value);

  /**
   * Return the long bits of the specified <code>double</code>.
   * The result of this function can be used as the argument to
   * <code>Double.longBitsToDouble(long)</code> to obtain the
   * original <code>double</code> value.  This method differs from 
   * <code>doubleToLongBits</code> in that it does not collapse
   * NaN values.
   *
   * @param value the <code>double</code> to convert
   * @return the bits of the <code>double</code>.
   */
  public static native long doubleToRawLongBits (double value);

  /**
   * Return the <code>double</code> represented by the long
   * bits specified.
   *
   * @param bits the long bits representing a <code>double</code>
   * @return the <code>double</code> represented by the bits.
   */
  public static native double longBitsToDouble (long bits);

  /**
   * Parse the specified <code>String</code> as a <code>double</code>.
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
   *
   * Remember that a <code>double</code> has a limited range.  If the
   * number you specify is greater than <code>Double.MAX_VALUE</code> or less
   * than <code>-Double.MAX_VALUE</code>, it will be set at
   * <code>Double.POSITIVE_INFINITY</code> or
   * <code>Double.NEGATIVE_INFINITY</code>, respectively.
   * <P>
   * Note also that <code>double</code> does not have perfect precision.  Many
   * numbers cannot be precisely represented.  The number you specify
   * will be rounded to the nearest representable value.
   * <code>Double.MIN_VALUE</code> is the margin of error for
   * <code>double</code> values.
   * <P>
   * If an unexpected character is found in the <code>String</code>, a
   * <code>NumberFormatException</code> will be thrown.  Spaces are not
   * allowed, and will cause the same exception.
   *
   * @XXX specify where/how we are not in accord with the spec.
   *
   * @param str the <code>String</code> to convert
   * @return the value of the <code>String</code> as a <code>double</code>.
   * @exception NumberFormatException when the string cannot be parsed to a
   *            <code>double</code>.
   * @exception NullPointerException when the string is null.
   * @see #MIN_VALUE
   * @see #MAX_VALUE
   * @see #POSITIVE_INFINITY
   * @see #NEGATIVE_INFINITY
   * @since 1.2
   */
  public static native double parseDouble (String s)
    throws NumberFormatException;

  /**
   * Initialize JNI cache.  This method is called only by the 
   * static initializer when using JNI.
   */
  private static native void initIDs ();
}
