/* java.lang.Math -- common mathematical functions, native allowed (VMMath)
   Copyright (C) 1998, 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;

import java.util.Random;

/**
 * Helper class containing useful mathematical functions and constants.
 * <P>
 *
 * Note that angles are specified in radians.  Conversion functions are
 * provided for your convenience.
 *
 * @author Paul Fisher
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.0
 */
public final class Math
{

  // FIXME - This is here because we need to load the "javalang" system
  // library somewhere late in the bootstrap cycle. We cannot do this
  // from VMSystem or VMRuntime since those are used to actually load
  // the library. This is mainly here because historically Math was
  // late enough in the bootstrap cycle to start using System after it
  // was initialized (called from the java.util classes).
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javalang");
      }
  }

  /**
   * Math is non-instantiable
   */
  private Math()
  {
  }

  /**
   * A random number generator, initialized on first use.
   */
  private static Random rand;

  /**
   * The most accurate approximation to the mathematical constant <em>e</em>:
   * <code>2.718281828459045</code>. Used in natural log and exp.
   *
   * @see #log(double)
   * @see #exp(double)
   */
  public static final double E = 2.718281828459045;

  /**
   * The most accurate approximation to the mathematical constant <em>pi</em>:
   * <code>3.141592653589793</code>. This is the ratio of a circle's diameter
   * to its circumference.
   */
  public static final double PI = 3.141592653589793;

  /**
   * Take the absolute value of the argument.
   * (Absolute value means make it positive.)
   * <P>
   *
   * Note that the the largest negative value (Integer.MIN_VALUE) cannot
   * be made positive.  In this case, because of the rules of negation in
   * a computer, MIN_VALUE is what will be returned.
   * This is a <em>negative</em> value.  You have been warned.
   *
   * @param i the number to take the absolute value of
   * @return the absolute value
   * @see Integer#MIN_VALUE
   */
  public static int abs(int i)
  {
    return (i < 0) ? -i : i;
  }

  /**
   * Take the absolute value of the argument.
   * (Absolute value means make it positive.)
   * <P>
   *
   * Note that the the largest negative value (Long.MIN_VALUE) cannot
   * be made positive.  In this case, because of the rules of negation in
   * a computer, MIN_VALUE is what will be returned.
   * This is a <em>negative</em> value.  You have been warned.
   *
   * @param l the number to take the absolute value of
   * @return the absolute value
   * @see Long#MIN_VALUE
   */
  public static long abs(long l)
  {
    return (l < 0) ? -l : l;
  }

  /**
   * Take the absolute value of the argument.
   * (Absolute value means make it positive.)
   * <P>
   *
   * This is equivalent, but faster than, calling
   * <code>Float.intBitsToFloat(0x7fffffff & Float.floatToIntBits(a))</code>.
   *
   * @param f the number to take the absolute value of
   * @return the absolute value
   */
  public static float abs(float f)
  {
    return (f <= 0) ? 0 - f : f;
  }

  /**
   * Take the absolute value of the argument.
   * (Absolute value means make it positive.)
   *
   * This is equivalent, but faster than, calling
   * <code>Double.longBitsToDouble(Double.doubleToLongBits(a)
   *       &lt;&lt; 1) &gt;&gt;&gt; 1);</code>.
   *
   * @param d the number to take the absolute value of
   * @return the absolute value
   */
  public static double abs(double d)
  {
    return (d <= 0) ? 0 - d : d;
  }

  /**
   * Return whichever argument is smaller.
   *
   * @param a the first number
   * @param b a second number
   * @return the smaller of the two numbers
   */
  public static int min(int a, int b)
  {
    return (a < b) ? a : b;
  }

  /**
   * Return whichever argument is smaller.
   *
   * @param a the first number
   * @param b a second number
   * @return the smaller of the two numbers
   */
  public static long min(long a, long b)
  {
    return (a < b) ? a : b;
  }

  /**
   * Return whichever argument is smaller. If either argument is NaN, the
   * result is NaN, and when comparing 0 and -0, -0 is always smaller.
   *
   * @param a the first number
   * @param b a second number
   * @return the smaller of the two numbers
   */
  public static float min(float a, float b)
  {
    // this check for NaN, from JLS 15.21.1, saves a method call
    if (a != a)
      return a;
    // no need to check if b is NaN; < will work correctly
    // recall that -0.0 == 0.0, but [+-]0.0 - [+-]0.0 behaves special
    if (a == 0 && b == 0)
      return -(-a - b);
    return (a < b) ? a : b;
  }

  /**
   * Return whichever argument is smaller. If either argument is NaN, the
   * result is NaN, and when comparing 0 and -0, -0 is always smaller.
   *
   * @param a the first number
   * @param b a second number
   * @return the smaller of the two numbers
   */
  public static double min(double a, double b)
  {
    // this check for NaN, from JLS 15.21.1, saves a method call
    if (a != a)
      return a;
    // no need to check if b is NaN; < will work correctly
    // recall that -0.0 == 0.0, but [+-]0.0 - [+-]0.0 behaves special
    if (a == 0 && b == 0)
      return -(-a - b);
    return (a < b) ? a : b;
  }

  /**
   * Return whichever argument is larger.
   *
   * @param a the first number
   * @param b a second number
   * @return the larger of the two numbers
   */
  public static int max(int a, int b)
  {
    return (a > b) ? a : b;
  }

  /**
   * Return whichever argument is larger.
   *
   * @param a the first number
   * @param b a second number
   * @return the larger of the two numbers
   */
  public static long max(long a, long b)
  {
    return (a > b) ? a : b;
  }

  /**
   * Return whichever argument is larger. If either argument is NaN, the
   * result is NaN, and when comparing 0 and -0, 0 is always larger.
   *
   * @param a the first number
   * @param b a second number
   * @return the larger of the two numbers
   */
  public static float max(float a, float b)
  {
    // this check for NaN, from JLS 15.21.1, saves a method call
    if (a != a)
      return a;
    // no need to check if b is NaN; > will work correctly
    // recall that -0.0 == 0.0, but [+-]0.0 - [+-]0.0 behaves special
    if (a == 0 && b == 0)
      return a - -b;
    return (a > b) ? a : b;
  }

  /**
   * Return whichever argument is larger. If either argument is NaN, the
   * result is NaN, and when comparing 0 and -0, 0 is always larger.
   *
   * @param a the first number
   * @param b a second number
   * @return the larger of the two numbers
   */
  public static double max(double a, double b)
  {
    // this check for NaN, from JLS 15.21.1, saves a method call
    if (a != a)
      return a;
    // no need to check if b is NaN; > will work correctly
    // recall that -0.0 == 0.0, but [+-]0.0 - [+-]0.0 behaves special
    if (a == 0 && b == 0)
      return a - -b;
    return (a > b) ? a : b;
  }

  /**
   * The trigonometric function <em>sin</em>. The sine of NaN or infinity is
   * NaN, and the sine of 0 retains its sign. This is accurate within 1 ulp,
   * and is semi-monotonic.
   *
   * @param a the angle (in radians)
   * @return sin(a)
   */
  public static double sin(double a)
  {
    return VMMath.sin(a);
  }

  /**
   * The trigonometric function <em>cos</em>. The cosine of NaN or infinity is
   * NaN. This is accurate within 1 ulp, and is semi-monotonic.
   *
   * @param a the angle (in radians)
   * @return cos(a)
   */
  public static double cos(double a)
  {
    return VMMath.cos(a);
  }

  /**
   * The trigonometric function <em>tan</em>. The tangent of NaN or infinity
   * is NaN, and the tangent of 0 retains its sign. This is accurate within 1
   * ulp, and is semi-monotonic.
   *
   * @param a the angle (in radians)
   * @return tan(a)
   */
  public static double tan(double a)
  {
    return VMMath.tan(a);
  }

  /**
   * The trigonometric function <em>arcsin</em>. The range of angles returned
   * is -pi/2 to pi/2 radians (-90 to 90 degrees). If the argument is NaN or
   * its absolute value is beyond 1, the result is NaN; and the arcsine of
   * 0 retains its sign. This is accurate within 1 ulp, and is semi-monotonic.
   *
   * @param a the sin to turn back into an angle
   * @return arcsin(a)
   */
  public static double asin(double a)
  {
    return VMMath.asin(a);
  }

  /**
   * The trigonometric function <em>arccos</em>. The range of angles returned
   * is 0 to pi radians (0 to 180 degrees). If the argument is NaN or
   * its absolute value is beyond 1, the result is NaN. This is accurate
   * within 1 ulp, and is semi-monotonic.
   *
   * @param a the cos to turn back into an angle
   * @return arccos(a)
   */
  public static double acos(double a)
  {
    return VMMath.acos(a);
  }

  /**
   * The trigonometric function <em>arcsin</em>. The range of angles returned
   * is -pi/2 to pi/2 radians (-90 to 90 degrees). If the argument is NaN, the
   * result is NaN; and the arctangent of 0 retains its sign. This is accurate
   * within 1 ulp, and is semi-monotonic.
   *
   * @param a the tan to turn back into an angle
   * @return arcsin(a)
   * @see #atan2(double, double)
   */
  public static double atan(double a)
  {
    return VMMath.atan(a);
  }

  /**
   * A special version of the trigonometric function <em>arctan</em>, for
   * converting rectangular coordinates <em>(x, y)</em> to polar
   * <em>(r, theta)</em>. This computes the arctangent of x/y in the range
   * of -pi to pi radians (-180 to 180 degrees). Special cases:<ul>
   * <li>If either argument is NaN, the result is NaN.</li>
   * <li>If the first argument is positive zero and the second argument is
   * positive, or the first argument is positive and finite and the second
   * argument is positive infinity, then the result is positive zero.</li>
   * <li>If the first argument is negative zero and the second argument is
   * positive, or the first argument is negative and finite and the second
   * argument is positive infinity, then the result is negative zero.</li>
   * <li>If the first argument is positive zero and the second argument is
   * negative, or the first argument is positive and finite and the second
   * argument is negative infinity, then the result is the double value
   * closest to pi.</li>
   * <li>If the first argument is negative zero and the second argument is
   * negative, or the first argument is negative and finite and the second
   * argument is negative infinity, then the result is the double value
   * closest to -pi.</li>
   * <li>If the first argument is positive and the second argument is
   * positive zero or negative zero, or the first argument is positive
   * infinity and the second argument is finite, then the result is the
   * double value closest to pi/2.</li>
   * <li>If the first argument is negative and the second argument is
   * positive zero or negative zero, or the first argument is negative
   * infinity and the second argument is finite, then the result is the
   * double value closest to -pi/2.</li>
   * <li>If both arguments are positive infinity, then the result is the
   * double value closest to pi/4.</li>
   * <li>If the first argument is positive infinity and the second argument
   * is negative infinity, then the result is the double value closest to
   * 3*pi/4.</li>
   * <li>If the first argument is negative infinity and the second argument
   * is positive infinity, then the result is the double value closest to
   * -pi/4.</li>
   * <li>If both arguments are negative infinity, then the result is the
   * double value closest to -3*pi/4.</li>
   *
   * </ul><p>This is accurate within 2 ulps, and is semi-monotonic. To get r,
   * use sqrt(x*x+y*y).
   *
   * @param y the y position
   * @param x the x position
   * @return <em>theta</em> in the conversion of (x, y) to (r, theta)
   * @see #atan(double)
   */
  public static double atan2(double y, double x)
  {
    return VMMath.atan2(y,x);
  }

  /**
   * Take <em>e</em><sup>a</sup>.  The opposite of <code>log()</code>. If the
   * argument is NaN, the result is NaN; if the argument is positive infinity,
   * the result is positive infinity; and if the argument is negative
   * infinity, the result is positive zero. This is accurate within 1 ulp,
   * and is semi-monotonic.
   *
   * @param a the number to raise to the power
   * @return the number raised to the power of <em>e</em>
   * @see #log(double)
   * @see #pow(double, double)
   */
  public static double exp(double a)
  {
    return VMMath.exp(a);
  }

  /**
   * Take ln(a) (the natural log).  The opposite of <code>exp()</code>. If the
   * argument is NaN or negative, the result is NaN; if the argument is
   * positive infinity, the result is positive infinity; and if the argument
   * is either zero, the result is negative infinity. This is accurate within
   * 1 ulp, and is semi-monotonic.
   *
   * <p>Note that the way to get log<sub>b</sub>(a) is to do this:
   * <code>ln(a) / ln(b)</code>.
   *
   * @param a the number to take the natural log of
   * @return the natural log of <code>a</code>
   * @see #exp(double)
   */
  public static double log(double a)
  {
    return VMMath.log(a);
  }

  /**
   * Take a square root. If the argument is NaN or negative, the result is
   * NaN; if the argument is positive infinity, the result is positive
   * infinity; and if the result is either zero, the result is the same.
   * This is accurate within the limits of doubles.
   *
   * <p>For a cube root, use <code>cbrt</code>.  For other roots, use
   * <code>pow(a, 1 / rootNumber)</code>.</p>
   *
   * @param a the numeric argument
   * @return the square root of the argument
   * @see #cbrt(double)
   * @see #pow(double, double)
   */
  public static double sqrt(double a)
  {
    return VMMath.sqrt(a);
  }

  /**
   * Raise a number to a power. Special cases:<ul>
   * <li>If the second argument is positive or negative zero, then the result
   * is 1.0.</li>
   * <li>If the second argument is 1.0, then the result is the same as the
   * first argument.</li>
   * <li>If the second argument is NaN, then the result is NaN.</li>
   * <li>If the first argument is NaN and the second argument is nonzero,
   * then the result is NaN.</li>
   * <li>If the absolute value of the first argument is greater than 1 and
   * the second argument is positive infinity, or the absolute value of the
   * first argument is less than 1 and the second argument is negative
   * infinity, then the result is positive infinity.</li>
   * <li>If the absolute value of the first argument is greater than 1 and
   * the second argument is negative infinity, or the absolute value of the
   * first argument is less than 1 and the second argument is positive
   * infinity, then the result is positive zero.</li>
   * <li>If the absolute value of the first argument equals 1 and the second
   * argument is infinite, then the result is NaN.</li>
   * <li>If the first argument is positive zero and the second argument is
   * greater than zero, or the first argument is positive infinity and the
   * second argument is less than zero, then the result is positive zero.</li>
   * <li>If the first argument is positive zero and the second argument is
   * less than zero, or the first argument is positive infinity and the
   * second argument is greater than zero, then the result is positive
   * infinity.</li>
   * <li>If the first argument is negative zero and the second argument is
   * greater than zero but not a finite odd integer, or the first argument is
   * negative infinity and the second argument is less than zero but not a
   * finite odd integer, then the result is positive zero.</li>
   * <li>If the first argument is negative zero and the second argument is a
   * positive finite odd integer, or the first argument is negative infinity
   * and the second argument is a negative finite odd integer, then the result
   * is negative zero.</li>
   * <li>If the first argument is negative zero and the second argument is
   * less than zero but not a finite odd integer, or the first argument is
   * negative infinity and the second argument is greater than zero but not a
   * finite odd integer, then the result is positive infinity.</li>
   * <li>If the first argument is negative zero and the second argument is a
   * negative finite odd integer, or the first argument is negative infinity
   * and the second argument is a positive finite odd integer, then the result
   * is negative infinity.</li>
   * <li>If the first argument is less than zero and the second argument is a
   * finite even integer, then the result is equal to the result of raising
   * the absolute value of the first argument to the power of the second
   * argument.</li>
   * <li>If the first argument is less than zero and the second argument is a
   * finite odd integer, then the result is equal to the negative of the
   * result of raising the absolute value of the first argument to the power
   * of the second argument.</li>
   * <li>If the first argument is finite and less than zero and the second
   * argument is finite and not an integer, then the result is NaN.</li>
   * <li>If both arguments are integers, then the result is exactly equal to
   * the mathematical result of raising the first argument to the power of
   * the second argument if that result can in fact be represented exactly as
   * a double value.</li>
   *
   * </ul><p>(In the foregoing descriptions, a floating-point value is
   * considered to be an integer if and only if it is a fixed point of the
   * method {@link #ceil(double)} or, equivalently, a fixed point of the
   * method {@link #floor(double)}. A value is a fixed point of a one-argument
   * method if and only if the result of applying the method to the value is
   * equal to the value.) This is accurate within 1 ulp, and is semi-monotonic.
   *
   * @param a the number to raise
   * @param b the power to raise it to
   * @return a<sup>b</sup>
   */
  public static double pow(double a, double b)
  {
    return VMMath.pow(a,b);
  }

  /**
   * Get the IEEE 754 floating point remainder on two numbers. This is the
   * value of <code>x - y * <em>n</em></code>, where <em>n</em> is the closest
   * double to <code>x / y</code> (ties go to the even n); for a zero
   * remainder, the sign is that of <code>x</code>. If either argument is NaN,
   * the first argument is infinite, or the second argument is zero, the result
   * is NaN; if x is finite but y is infinite, the result is x. This is
   * accurate within the limits of doubles.
   *
   * @param x the dividend (the top half)
   * @param y the divisor (the bottom half)
   * @return the IEEE 754-defined floating point remainder of x/y
   * @see #rint(double)
   */
  public static double IEEEremainder(double x, double y)
  {
    return VMMath.IEEEremainder(x,y);
  }

  /**
   * Take the nearest integer that is that is greater than or equal to the
   * argument. If the argument is NaN, infinite, or zero, the result is the
   * same; if the argument is between -1 and 0, the result is negative zero.
   * Note that <code>Math.ceil(x) == -Math.floor(-x)</code>.
   *
   * @param a the value to act upon
   * @return the nearest integer &gt;= <code>a</code>
   */
  public static double ceil(double a)
  {
    return VMMath.ceil(a);
  }

  /**
   * Take the nearest integer that is that is less than or equal to the
   * argument. If the argument is NaN, infinite, or zero, the result is the
   * same. Note that <code>Math.ceil(x) == -Math.floor(-x)</code>.
   *
   * @param a the value to act upon
   * @return the nearest integer &lt;= <code>a</code>
   */
  public static double floor(double a)
  {
    return VMMath.floor(a);
  }

  /**
   * Take the nearest integer to the argument.  If it is exactly between
   * two integers, the even integer is taken. If the argument is NaN,
   * infinite, or zero, the result is the same.
   *
   * @param a the value to act upon
   * @return the nearest integer to <code>a</code>
   */
  public static double rint(double a)
  {
    return VMMath.rint(a);
  }

  /**
   * Take the nearest integer to the argument.  This is equivalent to
   * <code>(int) Math.floor(a + 0.5f)</code>. If the argument is NaN, the result
   * is 0; otherwise if the argument is outside the range of int, the result
   * will be Integer.MIN_VALUE or Integer.MAX_VALUE, as appropriate.
   *
   * @param a the argument to round
   * @return the nearest integer to the argument
   * @see Integer#MIN_VALUE
   * @see Integer#MAX_VALUE
   */
  public static int round(float a)
  {
    // this check for NaN, from JLS 15.21.1, saves a method call
    if (a != a)
      return 0;
    return (int) floor(a + 0.5f);
  }

  /**
   * Take the nearest long to the argument.  This is equivalent to
   * <code>(long) Math.floor(a + 0.5)</code>. If the argument is NaN, the
   * result is 0; otherwise if the argument is outside the range of long, the
   * result will be Long.MIN_VALUE or Long.MAX_VALUE, as appropriate.
   *
   * @param a the argument to round
   * @return the nearest long to the argument
   * @see Long#MIN_VALUE
   * @see Long#MAX_VALUE
   */
  public static long round(double a)
  {
    // this check for NaN, from JLS 15.21.1, saves a method call
    if (a != a)
      return 0;
    return (long) floor(a + 0.5d);
  }

  /**
   * Get a random number.  This behaves like Random.nextDouble(), seeded by
   * System.currentTimeMillis() when first called. In other words, the number
   * is from a pseudorandom sequence, and lies in the range [+0.0, 1.0).
   * This random sequence is only used by this method, and is threadsafe,
   * although you may want your own random number generator if it is shared
   * among threads.
   *
   * @return a random number
   * @see Random#nextDouble()
   * @see System#currentTimeMillis()
   */
  public static synchronized double random()
  {
    if (rand == null)
      rand = new Random();
    return rand.nextDouble();
  }

  /**
   * Convert from degrees to radians. The formula for this is
   * radians = degrees * (pi/180); however it is not always exact given the
   * limitations of floating point numbers.
   *
   * @param degrees an angle in degrees
   * @return the angle in radians
   * @since 1.2
   */
  public static double toRadians(double degrees)
  {
    return (degrees * PI) / 180;
  }

  /**
   * Convert from radians to degrees. The formula for this is
   * degrees = radians * (180/pi); however it is not always exact given the
   * limitations of floating point numbers.
   *
   * @param rads an angle in radians
   * @return the angle in degrees
   * @since 1.2
   */
  public static double toDegrees(double rads)
  {
    return (rads * 180) / PI;
  }

  /**
   * <p>
   * Take a cube root. If the argument is <code>NaN</code>, an infinity or
   * zero, then the original value is returned.  The returned result is
   * within 1 ulp of the exact result.  For a finite value, <code>x</code>,
   * the cube root of <code>-x</code> is equal to the negation of the cube root
   * of <code>x</code>. 
   * </p>
   * <p>
   * For a square root, use <code>sqrt</code>.  For other roots, use
   * <code>pow(a, 1 / rootNumber)</code>.
   * </p>
   *
   * @param a the numeric argument
   * @return the cube root of the argument
   * @see #sqrt(double)
   * @see #pow(double, double)
   * @since 1.5
   */
  public static double cbrt(double a)
  {
    return VMMath.cbrt(a);
  }

  /**
   * <p>
   * Returns the hyperbolic cosine of the given value.  For a value,
   * <code>x</code>, the hyperbolic cosine is <code>(e<sup>x</sup> + 
   * e<sup>-x</sup>)/2</code>
   * with <code>e</code> being <a href="#E">Euler's number</a>.  The returned
   * result is within 2.5 ulps of the exact result.
   * </p>
   * <p>
   * If the supplied value is <code>NaN</code>, then the original value is
   * returned.  For either infinity, positive infinity is returned.
   * The hyperbolic cosine of zero is 1.0.
   * </p>
   * 
   * @param a the numeric argument
   * @return the hyperbolic cosine of <code>a</code>.
   * @since 1.5
   */
  public static double cosh(double a)
  {
    return VMMath.cosh(a);
  }

  /**
   * <p>
   * Returns <code>e<sup>a</sup> - 1.  For values close to 0, the
   * result of <code>expm1(a) + 1</code> tend to be much closer to the
   * exact result than simply <code>exp(x)</code>.  The result is within
   * 1 ulp of the exact result, and results are semi-monotonic.  For finite
   * inputs, the returned value is greater than or equal to -1.0.  Once
   * a result enters within half a ulp of this limit, the limit is returned.
   * </p>   
   * <p>
   * For <code>NaN</code>, positive infinity and zero, the original value
   * is returned.  Negative infinity returns a result of -1.0 (the limit).
   * </p>
   * 
   * @param a the numeric argument
   * @return <code>e<sup>a</sup> - 1</code>
   * @since 1.5
   */
  public static double expm1(double a)
  {
    return VMMath.expm1(a);
  }

  /**
   * <p>
   * Returns the hypotenuse, <code>a<sup>2</sup> + b<sup>2</sup></code>,
   * without intermediate overflow or underflow.  The returned result is
   * within 1 ulp of the exact result.  If one parameter is held constant,
   * then the result in the other parameter is semi-monotonic.
   * </p>
   * <p>
   * If either of the arguments is an infinity, then the returned result
   * is positive infinity.  Otherwise, if either argument is <code>NaN</code>,
   * then <code>NaN</code> is returned.
   * </p>
   * 
   * @param a the first parameter.
   * @param b the second parameter.
   * @return the hypotenuse matching the supplied parameters.
   * @since 1.5
   */
  public static double hypot(double a, double b)
  {
    return VMMath.hypot(a,b);
  }

  /**
   * <p>
   * Returns the base 10 logarithm of the supplied value.  The returned
   * result is within 1 ulp of the exact result, and the results are
   * semi-monotonic.
   * </p>
   * <p>
   * Arguments of either <code>NaN</code> or less than zero return
   * <code>NaN</code>.  An argument of positive infinity returns positive
   * infinity.  Negative infinity is returned if either positive or negative
   * zero is supplied.  Where the argument is the result of
   * <code>10<sup>n</sup</code>, then <code>n</code> is returned.
   * </p>
   *
   * @param a the numeric argument.
   * @return the base 10 logarithm of <code>a</code>.
   * @since 1.5
   */
  public static double log10(double a)
  {
    return VMMath.log10(a);
  }

  /**
   * <p>
   * Returns the natural logarithm resulting from the sum of the argument,
   * <code>a</code> and 1.  For values close to 0, the
   * result of <code>log1p(a)</code> tend to be much closer to the
   * exact result than simply <code>log(1.0+a)</code>.  The returned
   * result is within 1 ulp of the exact result, and the results are
   * semi-monotonic.
   * </p>
   * <p>
   * Arguments of either <code>NaN</code> or less than -1 return
   * <code>NaN</code>.  An argument of positive infinity or zero
   * returns the original argument.  Negative infinity is returned from an
   * argument of -1.
   * </p>
   *
   * @param a the numeric argument.
   * @return the natural logarithm of <code>a</code> + 1.
   * @since 1.5
   */
  public static double log1p(double a)
  {
    return VMMath.log1p(a);
  }

  /**
   * <p>
   * Returns the sign of the argument as follows:
   * </p>
   * <ul>
   * <li>If <code>a</code> is greater than zero, the result is 1.0.</li>
   * <li>If <code>a</code> is less than zero, the result is -1.0.</li>
   * <li>If <code>a</code> is <code>NaN</code>, the result is <code>NaN</code>.
   * <li>If <code>a</code> is positive or negative zero, the result is the
   * same.</li>
   * </ul>
   * 
   * @param a the numeric argument.
   * @return the sign of the argument.
   * @since 1.5.
   */
  public static double signum(double a)
  {
    if (Double.isNaN(a))
      return Double.NaN;
    if (a > 0)
      return 1.0;
    if (a < 0)
      return -1.0;
    return a;
  }

  /**
   * <p>
   * Returns the sign of the argument as follows:
   * </p>
   * <ul>
   * <li>If <code>a</code> is greater than zero, the result is 1.0f.</li>
   * <li>If <code>a</code> is less than zero, the result is -1.0f.</li>
   * <li>If <code>a</code> is <code>NaN</code>, the result is <code>NaN</code>.
   * <li>If <code>a</code> is positive or negative zero, the result is the
   * same.</li>
   * </ul>
   * 
   * @param a the numeric argument.
   * @return the sign of the argument.
   * @since 1.5.
   */
  public static float signum(float a)
  {
    if (Float.isNaN(a))
      return Float.NaN;
    if (a > 0)
      return 1.0f;
    if (a < 0)
      return -1.0f;
    return a;
  }

  /**
   * <p>
   * Returns the hyperbolic sine of the given value.  For a value,
   * <code>x</code>, the hyperbolic sine is <code>(e<sup>x</sup> - 
   * e<sup>-x</sup>)/2</code>
   * with <code>e</code> being <a href="#E">Euler's number</a>.  The returned
   * result is within 2.5 ulps of the exact result.
   * </p>
   * <p>
   * If the supplied value is <code>NaN</code>, an infinity or a zero, then the
   * original value is returned.
   * </p>
   * 
   * @param a the numeric argument
   * @return the hyperbolic sine of <code>a</code>.
   * @since 1.5
   */
  public static double sinh(double a)
  {
    return VMMath.sinh(a);
  }

  /**
   * <p>
   * Returns the hyperbolic tangent of the given value.  For a value,
   * <code>x</code>, the hyperbolic tangent is <code>(e<sup>x</sup> - 
   * e<sup>-x</sup>)/(e<sup>x</sup> + e<sup>-x</sup>)</code>
   * (i.e. <code>sinh(a)/cosh(a)</code>)
   * with <code>e</code> being <a href="#E">Euler's number</a>.  The returned
   * result is within 2.5 ulps of the exact result.  The absolute value
   * of the exact result is always less than 1.  Computed results are thus
   * less than or equal to 1 for finite arguments, with results within
   * half a ulp of either positive or negative 1 returning the appropriate
   * limit value (i.e. as if the argument was an infinity).
   * </p>
   * <p>
   * If the supplied value is <code>NaN</code> or zero, then the original
   * value is returned.  Positive infinity returns +1.0 and negative infinity
   * returns -1.0.
   * </p>
   * 
   * @param a the numeric argument
   * @return the hyperbolic tangent of <code>a</code>.
   * @since 1.5
   */
  public static double tanh(double a)
  {
    return VMMath.tanh(a);
  }

  /**
   * Return the ulp for the given double argument.  The ulp is the
   * difference between the argument and the next larger double.  Note
   * that the sign of the double argument is ignored, that is,
   * ulp(x) == ulp(-x).  If the argument is a NaN, then NaN is returned.
   * If the argument is an infinity, then +Inf is returned.  If the
   * argument is zero (either positive or negative), then
   * {@link Double#MIN_VALUE} is returned.
   * @param d the double whose ulp should be returned
   * @return the difference between the argument and the next larger double
   * @since 1.5
   */
  public static double ulp(double d)
  {
    if (Double.isNaN(d))
      return d;
    if (Double.isInfinite(d))
      return Double.POSITIVE_INFINITY;
    // This handles both +0.0 and -0.0.
    if (d == 0.0)
      return Double.MIN_VALUE;
    long bits = Double.doubleToLongBits(d);
    final int mantissaBits = 52;
    final int exponentBits = 11;
    final long mantMask = (1L << mantissaBits) - 1;
    long mantissa = bits & mantMask;
    final long expMask = (1L << exponentBits) - 1;
    long exponent = (bits >>> mantissaBits) & expMask;

    // Denormal number, so the answer is easy.
    if (exponent == 0)
      {
        long result = (exponent << mantissaBits) | 1L;
        return Double.longBitsToDouble(result);
      }

    // Conceptually we want to have '1' as the mantissa.  Then we would
    // shift the mantissa over to make a normal number.  If this underflows
    // the exponent, we will make a denormal result.
    long newExponent = exponent - mantissaBits;
    long newMantissa;
    if (newExponent > 0)
      newMantissa = 0;
    else
      {
        newMantissa = 1L << -(newExponent - 1);
        newExponent = 0;
      }
    return Double.longBitsToDouble((newExponent << mantissaBits) | newMantissa);
  }

  /**
   * Return the ulp for the given float argument.  The ulp is the
   * difference between the argument and the next larger float.  Note
   * that the sign of the float argument is ignored, that is,
   * ulp(x) == ulp(-x).  If the argument is a NaN, then NaN is returned.
   * If the argument is an infinity, then +Inf is returned.  If the
   * argument is zero (either positive or negative), then
   * {@link Float#MIN_VALUE} is returned.
   * @param f the float whose ulp should be returned
   * @return the difference between the argument and the next larger float
   * @since 1.5
   */
  public static float ulp(float f)
  {
    if (Float.isNaN(f))
      return f;
    if (Float.isInfinite(f))
      return Float.POSITIVE_INFINITY;
    // This handles both +0.0 and -0.0.
    if (f == 0.0)
      return Float.MIN_VALUE;
    int bits = Float.floatToIntBits(f);
    final int mantissaBits = 23;
    final int exponentBits = 8;
    final int mantMask = (1 << mantissaBits) - 1;
    int mantissa = bits & mantMask;
    final int expMask = (1 << exponentBits) - 1;
    int exponent = (bits >>> mantissaBits) & expMask;

    // Denormal number, so the answer is easy.
    if (exponent == 0)
      {
        int result = (exponent << mantissaBits) | 1;
        return Float.intBitsToFloat(result);
      }

    // Conceptually we want to have '1' as the mantissa.  Then we would
    // shift the mantissa over to make a normal number.  If this underflows
    // the exponent, we will make a denormal result.
    int newExponent = exponent - mantissaBits;
    int newMantissa;
    if (newExponent > 0)
      newMantissa = 0;
    else
      {
        newMantissa = 1 << -(newExponent - 1);
        newExponent = 0;
      }
    return Float.intBitsToFloat((newExponent << mantissaBits) | newMantissa);
  }
}
