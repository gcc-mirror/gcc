/* java.lang.StrictMath -- common mathematical functions, strict Java
   Copyright (C) 1998, 2001, 2002, 2003 Free Software Foundation, Inc.

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

/*
 * Some of the algorithms in this class are in the public domain, as part
 * of fdlibm (freely-distributable math library), available at
 * http://www.netlib.org/fdlibm/, and carry the following copyright:
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

package java.lang;

import gnu.classpath.Configuration;

import java.util.Random;

/**
 * Helper class containing useful mathematical functions and constants.
 * This class mirrors {@link Math}, but is 100% portable, because it uses
 * no native methods whatsoever.  Also, these algorithms are all accurate
 * to less than 1 ulp, and execute in <code>strictfp</code> mode, while
 * Math is allowed to vary in its results for some functions. Unfortunately,
 * this usually means StrictMath has less efficiency and speed, as Math can
 * use native methods.
 *
 * <p>The source of the various algorithms used is the fdlibm library, at:<br>
 * <a href="http://www.netlib.org/fdlibm/">http://www.netlib.org/fdlibm/</a>
 *
 * Note that angles are specified in radians.  Conversion functions are
 * provided for your convenience.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.3
 */
public final strictfp class StrictMath
{
  /**
   * StrictMath is non-instantiable.
   */
  private StrictMath()
  {
  }

  /**
   * A random number generator, initialized on first use.
   *
   * @see #random()
   */
  private static Random rand;

  /**
   * The most accurate approximation to the mathematical constant <em>e</em>:
   * <code>2.718281828459045</code>. Used in natural log and exp.
   *
   * @see #log(double)
   * @see #exp(double)
   */
  public static final double E
    = 2.718281828459045; // Long bits 0x4005bf0z8b145769L.

  /**
   * The most accurate approximation to the mathematical constant <em>pi</em>:
   * <code>3.141592653589793</code>. This is the ratio of a circle's diameter
   * to its circumference.
   */
  public static final double PI
    = 3.141592653589793; // Long bits 0x400921fb54442d18L.

  /**
   * Take the absolute value of the argument. (Absolute value means make
   * it positive.)
   *
   * <p>Note that the the largest negative value (Integer.MIN_VALUE) cannot
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
   * Take the absolute value of the argument. (Absolute value means make
   * it positive.)
   *
   * <p>Note that the the largest negative value (Long.MIN_VALUE) cannot
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
   * Take the absolute value of the argument. (Absolute value means make
   * it positive.)
   *
   * @param f the number to take the absolute value of
   * @return the absolute value
   */
  public static float abs(float f)
  {
    return (f <= 0) ? 0 - f : f;
  }

  /**
   * Take the absolute value of the argument. (Absolute value means make
   * it positive.)
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
   * NaN, and the sine of 0 retains its sign.
   *
   * @param a the angle (in radians)
   * @return sin(a)
   */
  public static double sin(double a)
  {
    if (a == Double.NEGATIVE_INFINITY || ! (a < Double.POSITIVE_INFINITY))
      return Double.NaN;

    if (abs(a) <= PI / 4)
      return sin(a, 0);

    // Argument reduction needed.
    double[] y = new double[2];
    int n = remPiOver2(a, y);
    switch (n & 3)
      {
      case 0:
        return sin(y[0], y[1]);
      case 1:
        return cos(y[0], y[1]);
      case 2:
        return -sin(y[0], y[1]);
      default:
        return -cos(y[0], y[1]);
      }
  }

  /**
   * The trigonometric function <em>cos</em>. The cosine of NaN or infinity is
   * NaN.
   *
   * @param a the angle (in radians).
   * @return cos(a).
   */
  public static double cos(double a)
  {
    if (a == Double.NEGATIVE_INFINITY || ! (a < Double.POSITIVE_INFINITY))
      return Double.NaN;

    if (abs(a) <= PI / 4)
      return cos(a, 0);

    // Argument reduction needed.
    double[] y = new double[2];
    int n = remPiOver2(a, y);
    switch (n & 3)
      {
      case 0:
        return cos(y[0], y[1]);
      case 1:
        return -sin(y[0], y[1]);
      case 2:
        return -cos(y[0], y[1]);
      default:
        return sin(y[0], y[1]);
      }
  }

  /**
   * The trigonometric function <em>tan</em>. The tangent of NaN or infinity
   * is NaN, and the tangent of 0 retains its sign.
   *
   * @param a the angle (in radians)
   * @return tan(a)
   */
  public static double tan(double a)
  {
    if (a == Double.NEGATIVE_INFINITY || ! (a < Double.POSITIVE_INFINITY))
      return Double.NaN;

    if (abs(a) <= PI / 4)
      return tan(a, 0, false);

    // Argument reduction needed.
    double[] y = new double[2];
    int n = remPiOver2(a, y);
    return tan(y[0], y[1], (n & 1) == 1);
  }

  /**
   * The trigonometric function <em>arcsin</em>. The range of angles returned
   * is -pi/2 to pi/2 radians (-90 to 90 degrees). If the argument is NaN or
   * its absolute value is beyond 1, the result is NaN; and the arcsine of
   * 0 retains its sign.
   *
   * @param x the sin to turn back into an angle
   * @return arcsin(x)
   */
  public static double asin(double x)
  {
    boolean negative = x < 0;
    if (negative)
      x = -x;
    if (! (x <= 1))
      return Double.NaN;
    if (x == 1)
      return negative ? -PI / 2 : PI / 2;
    if (x < 0.5)
      {
        if (x < 1 / TWO_27)
          return negative ? -x : x;
        double t = x * x;
        double p = t * (PS0 + t * (PS1 + t * (PS2 + t * (PS3 + t
                                                         * (PS4 + t * PS5)))));
        double q = 1 + t * (QS1 + t * (QS2 + t * (QS3 + t * QS4)));
        return negative ? -x - x * (p / q) : x + x * (p / q);
      }
    double w = 1 - x; // 1>|x|>=0.5.
    double t = w * 0.5;
    double p = t * (PS0 + t * (PS1 + t * (PS2 + t * (PS3 + t
                                                     * (PS4 + t * PS5)))));
    double q = 1 + t * (QS1 + t * (QS2 + t * (QS3 + t * QS4)));
    double s = sqrt(t);
    if (x >= 0.975)
      {
        w = p / q;
        t = PI / 2 - (2 * (s + s * w) - PI_L / 2);
      }
    else
      {
        w = (float) s;
        double c = (t - w * w) / (s + w);
        p = 2 * s * (p / q) - (PI_L / 2 - 2 * c);
        q = PI / 4 - 2 * w;
        t = PI / 4 - (p - q);
      }
    return negative ? -t : t;
  }

  /**
   * The trigonometric function <em>arccos</em>. The range of angles returned
   * is 0 to pi radians (0 to 180 degrees). If the argument is NaN or
   * its absolute value is beyond 1, the result is NaN.
   *
   * @param x the cos to turn back into an angle
   * @return arccos(x)
   */
  public static double acos(double x)
  {
    boolean negative = x < 0;
    if (negative)
      x = -x;
    if (! (x <= 1))
      return Double.NaN;
    if (x == 1)
      return negative ? PI : 0;
    if (x < 0.5)
      {
        if (x < 1 / TWO_57)
          return PI / 2;
        double z = x * x;
        double p = z * (PS0 + z * (PS1 + z * (PS2 + z * (PS3 + z
                                                         * (PS4 + z * PS5)))));
        double q = 1 + z * (QS1 + z * (QS2 + z * (QS3 + z * QS4)));
        double r = x - (PI_L / 2 - x * (p / q));
        return negative ? PI / 2 + r : PI / 2 - r;
      }
    if (negative) // x<=-0.5.
      {
        double z = (1 + x) * 0.5;
        double p = z * (PS0 + z * (PS1 + z * (PS2 + z * (PS3 + z
                                                         * (PS4 + z * PS5)))));
        double q = 1 + z * (QS1 + z * (QS2 + z * (QS3 + z * QS4)));
        double s = sqrt(z);
        double w = p / q * s - PI_L / 2;
        return PI - 2 * (s + w);
      }
    double z = (1 - x) * 0.5; // x>0.5.
    double s = sqrt(z);
    double df = (float) s;
    double c = (z - df * df) / (s + df);
    double p = z * (PS0 + z * (PS1 + z * (PS2 + z * (PS3 + z
                                                     * (PS4 + z * PS5)))));
    double q = 1 + z * (QS1 + z * (QS2 + z * (QS3 + z * QS4)));
    double w = p / q * s + c;
    return 2 * (df + w);
  }

  /**
   * The trigonometric function <em>arcsin</em>. The range of angles returned
   * is -pi/2 to pi/2 radians (-90 to 90 degrees). If the argument is NaN, the
   * result is NaN; and the arctangent of 0 retains its sign.
   *
   * @param x the tan to turn back into an angle
   * @return arcsin(x)
   * @see #atan2(double, double)
   */
  public static double atan(double x)
  {
    double lo;
    double hi;
    boolean negative = x < 0;
    if (negative)
      x = -x;
    if (x >= TWO_66)
      return negative ? -PI / 2 : PI / 2;
    if (! (x >= 0.4375)) // |x|<7/16, or NaN.
      {
        if (! (x >= 1 / TWO_29)) // Small, or NaN.
          return negative ? -x : x;
        lo = hi = 0;
      }
    else if (x < 1.1875)
      {
        if (x < 0.6875) // 7/16<=|x|<11/16.
          {
            x = (2 * x - 1) / (2 + x);
            hi = ATAN_0_5H;
            lo = ATAN_0_5L;
          }
        else // 11/16<=|x|<19/16.
          {
            x = (x - 1) / (x + 1);
            hi = PI / 4;
            lo = PI_L / 4;
          }
      }
    else if (x < 2.4375) // 19/16<=|x|<39/16.
      {
        x = (x - 1.5) / (1 + 1.5 * x);
        hi = ATAN_1_5H;
        lo = ATAN_1_5L;
      }
    else // 39/16<=|x|<2**66.
      {
        x = -1 / x;
        hi = PI / 2;
        lo = PI_L / 2;
      }

    // Break sum from i=0 to 10 ATi*z**(i+1) into odd and even poly.
    double z = x * x;
    double w = z * z;
    double s1 = z * (AT0 + w * (AT2 + w * (AT4 + w * (AT6 + w
                                                      * (AT8 + w * AT10)))));
    double s2 = w * (AT1 + w * (AT3 + w * (AT5 + w * (AT7 + w * AT9))));
    if (hi == 0)
      return negative ? x * (s1 + s2) - x : x - x * (s1 + s2);
    z = hi - ((x * (s1 + s2) - lo) - x);
    return negative ? -z : z;
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
   * </ul><p>This returns theta, the angle of the point. To get r, albeit
   * slightly inaccurately, use sqrt(x*x+y*y).
   *
   * @param y the y position
   * @param x the x position
   * @return <em>theta</em> in the conversion of (x, y) to (r, theta)
   * @see #atan(double)
   */
  public static double atan2(double y, double x)
  {
    if (x != x || y != y)
      return Double.NaN;
    if (x == 1)
      return atan(y);
    if (x == Double.POSITIVE_INFINITY)
      {
        if (y == Double.POSITIVE_INFINITY)
          return PI / 4;
        if (y == Double.NEGATIVE_INFINITY)
          return -PI / 4;
        return 0 * y;
      }
    if (x == Double.NEGATIVE_INFINITY)
      {
        if (y == Double.POSITIVE_INFINITY)
          return 3 * PI / 4;
        if (y == Double.NEGATIVE_INFINITY)
          return -3 * PI / 4;
        return (1 / (0 * y) == Double.POSITIVE_INFINITY) ? PI : -PI;
      }
    if (y == 0)
      {
        if (1 / (0 * x) == Double.POSITIVE_INFINITY)
          return y;
        return (1 / y == Double.POSITIVE_INFINITY) ? PI : -PI;
      }
    if (y == Double.POSITIVE_INFINITY || y == Double.NEGATIVE_INFINITY
        || x == 0)
      return y < 0 ? -PI / 2 : PI / 2;

    double z = abs(y / x); // Safe to do y/x.
    if (z > TWO_60)
      z = PI / 2 + 0.5 * PI_L;
    else if (x < 0 && z < 1 / TWO_60)
      z = 0;
    else
      z = atan(z);
    if (x > 0)
      return y > 0 ? z : -z;
    return y > 0 ? PI - (z - PI_L) : z - PI_L - PI;
  }

  /**
   * Take <em>e</em><sup>a</sup>.  The opposite of <code>log()</code>. If the
   * argument is NaN, the result is NaN; if the argument is positive infinity,
   * the result is positive infinity; and if the argument is negative
   * infinity, the result is positive zero.
   *
   * @param x the number to raise to the power
   * @return the number raised to the power of <em>e</em>
   * @see #log(double)
   * @see #pow(double, double)
   */
  public static double exp(double x)
  {
    if (x != x)
      return x;
    if (x > EXP_LIMIT_H)
      return Double.POSITIVE_INFINITY;
    if (x < EXP_LIMIT_L)
      return 0;

    // Argument reduction.
    double hi;
    double lo;
    int k;
    double t = abs(x);
    if (t > 0.5 * LN2)
      {
        if (t < 1.5 * LN2)
          {
            hi = t - LN2_H;
            lo = LN2_L;
            k = 1;
          }
        else
          {
            k = (int) (INV_LN2 * t + 0.5);
            hi = t - k * LN2_H;
            lo = k * LN2_L;
          }
        if (x < 0)
          {
            hi = -hi;
            lo = -lo;
            k = -k;
          }
        x = hi - lo;
      }
    else if (t < 1 / TWO_28)
      return 1;
    else
      lo = hi = k = 0;

    // Now x is in primary range.
    t = x * x;
    double c = x - t * (P1 + t * (P2 + t * (P3 + t * (P4 + t * P5))));
    if (k == 0)
      return 1 - (x * c / (c - 2) - x);
    double y = 1 - (lo - x * c / (2 - c) - hi);
    return scale(y, k);
  }

  /**
   * Take ln(a) (the natural log).  The opposite of <code>exp()</code>. If the
   * argument is NaN or negative, the result is NaN; if the argument is
   * positive infinity, the result is positive infinity; and if the argument
   * is either zero, the result is negative infinity.
   *
   * <p>Note that the way to get log<sub>b</sub>(a) is to do this:
   * <code>ln(a) / ln(b)</code>.
   *
   * @param x the number to take the natural log of
   * @return the natural log of <code>a</code>
   * @see #exp(double)
   */
  public static double log(double x)
  {
    if (x == 0)
      return Double.NEGATIVE_INFINITY;
    if (x < 0)
      return Double.NaN;
    if (! (x < Double.POSITIVE_INFINITY))
      return x;

    // Normalize x.
    long bits = Double.doubleToLongBits(x);
    int exp = (int) (bits >> 52);
    if (exp == 0) // Subnormal x.
      {
        x *= TWO_54;
        bits = Double.doubleToLongBits(x);
        exp = (int) (bits >> 52) - 54;
      }
    exp -= 1023; // Unbias exponent.
    bits = (bits & 0x000fffffffffffffL) | 0x3ff0000000000000L;
    x = Double.longBitsToDouble(bits);
    if (x >= SQRT_2)
      {
        x *= 0.5;
        exp++;
      }
    x--;
    if (abs(x) < 1 / TWO_20)
      {
        if (x == 0)
          return exp * LN2_H + exp * LN2_L;
        double r = x * x * (0.5 - 1 / 3.0 * x);
        if (exp == 0)
          return x - r;
        return exp * LN2_H - ((r - exp * LN2_L) - x);
      }
    double s = x / (2 + x);
    double z = s * s;
    double w = z * z;
    double t1 = w * (LG2 + w * (LG4 + w * LG6));
    double t2 = z * (LG1 + w * (LG3 + w * (LG5 + w * LG7)));
    double r = t2 + t1;
    if (bits >= 0x3ff6174a00000000L && bits < 0x3ff6b85200000000L)
      {
        double h = 0.5 * x * x; // Need more accuracy for x near sqrt(2).
        if (exp == 0)
          return x - (h - s * (h + r));
        return exp * LN2_H - ((h - (s * (h + r) + exp * LN2_L)) - x);
      }
    if (exp == 0)
      return x - s * (x - r);
    return exp * LN2_H - ((s * (x - r) - exp * LN2_L) - x);
  }

  /**
   * Take a square root. If the argument is NaN or negative, the result is
   * NaN; if the argument is positive infinity, the result is positive
   * infinity; and if the result is either zero, the result is the same.
   *
   * <p>For other roots, use pow(x, 1/rootNumber).
   *
   * @param x the numeric argument
   * @return the square root of the argument
   * @see #pow(double, double)
   */
  public static double sqrt(double x)
  {
    if (x < 0)
      return Double.NaN;
    if (x == 0 || ! (x < Double.POSITIVE_INFINITY))
      return x;

    // Normalize x.
    long bits = Double.doubleToLongBits(x);
    int exp = (int) (bits >> 52);
    if (exp == 0) // Subnormal x.
      {
        x *= TWO_54;
        bits = Double.doubleToLongBits(x);
        exp = (int) (bits >> 52) - 54;
      }
    exp -= 1023; // Unbias exponent.
    bits = (bits & 0x000fffffffffffffL) | 0x0010000000000000L;
    if ((exp & 1) == 1) // Odd exp, double x to make it even.
      bits <<= 1;
    exp >>= 1;

    // Generate sqrt(x) bit by bit.
    bits <<= 1;
    long q = 0;
    long s = 0;
    long r = 0x0020000000000000L; // Move r right to left.
    while (r != 0)
      {
        long t = s + r;
        if (t <= bits)
          {
            s = t + r;
            bits -= t;
            q += r;
          }
        bits <<= 1;
        r >>= 1;
      }

    // Use floating add to round correctly.
    if (bits != 0)
      q += q & 1;
    return Double.longBitsToDouble((q >> 1) + ((exp + 1022L) << 52));
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
   * equal to the value.)
   *
   * @param x the number to raise
   * @param y the power to raise it to
   * @return x<sup>y</sup>
   */
  public static double pow(double x, double y)
  {
    // Special cases first.
    if (y == 0)
      return 1;
    if (y == 1)
      return x;
    if (y == -1)
      return 1 / x;
    if (x != x || y != y)
      return Double.NaN;

    // When x < 0, yisint tells if y is not an integer (0), even(1),
    // or odd (2).
    int yisint = 0;
    if (x < 0 && floor(y) == y)
      yisint = (y % 2 == 0) ? 2 : 1;
    double ax = abs(x);
    double ay = abs(y);

    // More special cases, of y.
    if (ay == Double.POSITIVE_INFINITY)
      {
        if (ax == 1)
          return Double.NaN;
        if (ax > 1)
          return y > 0 ? y : 0;
        return y < 0 ? -y : 0;
      }
    if (y == 2)
      return x * x;
    if (y == 0.5)
      return sqrt(x);

    // More special cases, of x.
    if (x == 0 || ax == Double.POSITIVE_INFINITY || ax == 1)
      {
        if (y < 0)
          ax = 1 / ax;
        if (x < 0)
          {
            if (x == -1 && yisint == 0)
              ax = Double.NaN;
            else if (yisint == 1)
              ax = -ax;
          }
        return ax;
      }
    if (x < 0 && yisint == 0)
      return Double.NaN;

    // Now we can start!
    double t;
    double t1;
    double t2;
    double u;
    double v;
    double w;
    if (ay > TWO_31)
      {
        if (ay > TWO_64) // Automatic over/underflow.
          return ((ax < 1) ? y < 0 : y > 0) ? Double.POSITIVE_INFINITY : 0;
        // Over/underflow if x is not close to one.
        if (ax < 0.9999995231628418)
          return y < 0 ? Double.POSITIVE_INFINITY : 0;
        if (ax >= 1.0000009536743164)
          return y > 0 ? Double.POSITIVE_INFINITY : 0;
        // Now |1-x| is <= 2**-20, sufficient to compute
        // log(x) by x-x^2/2+x^3/3-x^4/4.
        t = x - 1;
        w = t * t * (0.5 - t * (1 / 3.0 - t * 0.25));
        u = INV_LN2_H * t;
        v = t * INV_LN2_L - w * INV_LN2;
        t1 = (float) (u + v);
        t2 = v - (t1 - u);
      }
    else
    {
      long bits = Double.doubleToLongBits(ax);
      int exp = (int) (bits >> 52);
      if (exp == 0) // Subnormal x.
        {
          ax *= TWO_54;
          bits = Double.doubleToLongBits(ax);
          exp = (int) (bits >> 52) - 54;
        }
      exp -= 1023; // Unbias exponent.
      ax = Double.longBitsToDouble((bits & 0x000fffffffffffffL)
                                   | 0x3ff0000000000000L);
      boolean k;
      if (ax < SQRT_1_5)  // |x|<sqrt(3/2).
        k = false;
      else if (ax < SQRT_3) // |x|<sqrt(3).
        k = true;
      else
        {
          k = false;
          ax *= 0.5;
          exp++;
        }

      // Compute s = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5).
      u = ax - (k ? 1.5 : 1);
      v = 1 / (ax + (k ? 1.5 : 1));
      double s = u * v;
      double s_h = (float) s;
      double t_h = (float) (ax + (k ? 1.5 : 1));
      double t_l = ax - (t_h - (k ? 1.5 : 1));
      double s_l = v * ((u - s_h * t_h) - s_h * t_l);
      // Compute log(ax).
      double s2 = s * s;
      double r = s_l * (s_h + s) + s2 * s2
        * (L1 + s2 * (L2 + s2 * (L3 + s2 * (L4 + s2 * (L5 + s2 * L6)))));
      s2 = s_h * s_h;
      t_h = (float) (3.0 + s2 + r);
      t_l = r - (t_h - 3.0 - s2);
      // u+v = s*(1+...).
      u = s_h * t_h;
      v = s_l * t_h + t_l * s;
      // 2/(3log2)*(s+...).
      double p_h = (float) (u + v);
      double p_l = v - (p_h - u);
      double z_h = CP_H * p_h;
      double z_l = CP_L * p_h + p_l * CP + (k ? DP_L : 0);
      // log2(ax) = (s+..)*2/(3*log2) = exp + dp_h + z_h + z_l.
      t = exp;
      t1 = (float) (z_h + z_l + (k ? DP_H : 0) + t);
      t2 = z_l - (t1 - t - (k ? DP_H : 0) - z_h);
    }

    // Split up y into y1+y2 and compute (y1+y2)*(t1+t2).
    boolean negative = x < 0 && yisint == 1;
    double y1 = (float) y;
    double p_l = (y - y1) * t1 + y * t2;
    double p_h = y1 * t1;
    double z = p_l + p_h;
    if (z >= 1024) // Detect overflow.
      {
        if (z > 1024 || p_l + OVT > z - p_h)
          return negative ? Double.NEGATIVE_INFINITY
            : Double.POSITIVE_INFINITY;
      }
    else if (z <= -1075) // Detect underflow.
      {
        if (z < -1075 || p_l <= z - p_h)
          return negative ? -0.0 : 0;
      }

    // Compute 2**(p_h+p_l).
    int n = round((float) z);
    p_h -= n;
    t = (float) (p_l + p_h);
    u = t * LN2_H;
    v = (p_l - (t - p_h)) * LN2 + t * LN2_L;
    z = u + v;
    w = v - (z - u);
    t = z * z;
    t1 = z - t * (P1 + t * (P2 + t * (P3 + t * (P4 + t * P5))));
    double r = (z * t1) / (t1 - 2) - (w + z * w);
    z = scale(1 - (r - z), n);
    return negative ? -z : z;
  }

  /**
   * Get the IEEE 754 floating point remainder on two numbers. This is the
   * value of <code>x - y * <em>n</em></code>, where <em>n</em> is the closest
   * double to <code>x / y</code> (ties go to the even n); for a zero
   * remainder, the sign is that of <code>x</code>. If either argument is NaN,
   * the first argument is infinite, or the second argument is zero, the result
   * is NaN; if x is finite but y is infinite, the result is x.
   *
   * @param x the dividend (the top half)
   * @param y the divisor (the bottom half)
   * @return the IEEE 754-defined floating point remainder of x/y
   * @see #rint(double)
   */
  public static double IEEEremainder(double x, double y)
  {
    // Purge off exception values.
    if (x == Double.NEGATIVE_INFINITY || ! (x < Double.POSITIVE_INFINITY)
        || y == 0 || y != y)
      return Double.NaN;

    boolean negative = x < 0;
    x = abs(x);
    y = abs(y);
    if (x == y || x == 0)
      return 0 * x; // Get correct sign.

    // Achieve x < 2y, then take first shot at remainder.
    if (y < TWO_1023)
      x %= y + y;

    // Now adjust x to get correct precision.
    if (y < 4 / TWO_1023)
      {
        if (x + x > y)
          {
            x -= y;
            if (x + x >= y)
              x -= y;
          }
      }
    else
      {
        y *= 0.5;
        if (x > y)
          {
            x -= y;
            if (x >= y)
              x -= y;
          }
      }
    return negative ? -x : x;
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
    return -floor(-a);
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
    double x = abs(a);
    if (! (x < TWO_52) || (long) a == a)
      return a; // No fraction bits; includes NaN and infinity.
    if (x < 1)
      return a >= 0 ? 0 * a : -1; // Worry about signed zero.
    return a < 0 ? (long) a - 1.0 : (long) a; // Cast to long truncates.
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
    double x = abs(a);
    if (! (x < TWO_52))
      return a; // No fraction bits; includes NaN and infinity.
    if (x <= 0.5)
      return 0 * a; // Worry about signed zero.
    if (x % 2 <= 0.5)
      return (long) a; // Catch round down to even.
    return (long) (a + (a < 0 ? -0.5 : 0.5)); // Cast to long truncates.
  }

  /**
   * Take the nearest integer to the argument.  This is equivalent to
   * <code>(int) Math.floor(f + 0.5f)</code>. If the argument is NaN, the
   * result is 0; otherwise if the argument is outside the range of int, the
   * result will be Integer.MIN_VALUE or Integer.MAX_VALUE, as appropriate.
   *
   * @param f the argument to round
   * @return the nearest integer to the argument
   * @see Integer#MIN_VALUE
   * @see Integer#MAX_VALUE
   */
  public static int round(float f)
  {
    return (int) floor(f + 0.5f);
  }

  /**
   * Take the nearest long to the argument.  This is equivalent to
   * <code>(long) Math.floor(d + 0.5)</code>. If the argument is NaN, the
   * result is 0; otherwise if the argument is outside the range of long, the
   * result will be Long.MIN_VALUE or Long.MAX_VALUE, as appropriate.
   *
   * @param d the argument to round
   * @return the nearest long to the argument
   * @see Long#MIN_VALUE
   * @see Long#MAX_VALUE
   */
  public static long round(double d)
  {
    return (long) floor(d + 0.5);
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
   */
  public static double toDegrees(double rads)
  {
    return (rads * 180) / PI;
  }

  /**
   * Constants for scaling and comparing doubles by powers of 2. The compiler
   * must automatically inline constructs like (1/TWO_54), so we don't list
   * negative powers of two here.
   */
  private static final double
    TWO_16 = 0x10000, // Long bits 0x40f0000000000000L.
    TWO_20 = 0x100000, // Long bits 0x4130000000000000L.
    TWO_24 = 0x1000000, // Long bits 0x4170000000000000L.
    TWO_27 = 0x8000000, // Long bits 0x41a0000000000000L.
    TWO_28 = 0x10000000, // Long bits 0x41b0000000000000L.
    TWO_29 = 0x20000000, // Long bits 0x41c0000000000000L.
    TWO_31 = 0x80000000L, // Long bits 0x41e0000000000000L.
    TWO_49 = 0x2000000000000L, // Long bits 0x4300000000000000L.
    TWO_52 = 0x10000000000000L, // Long bits 0x4330000000000000L.
    TWO_54 = 0x40000000000000L, // Long bits 0x4350000000000000L.
    TWO_57 = 0x200000000000000L, // Long bits 0x4380000000000000L.
    TWO_60 = 0x1000000000000000L, // Long bits 0x43b0000000000000L.
    TWO_64 = 1.8446744073709552e19, // Long bits 0x43f0000000000000L.
    TWO_66 = 7.378697629483821e19, // Long bits 0x4410000000000000L.
    TWO_1023 = 8.98846567431158e307; // Long bits 0x7fe0000000000000L.

  /**
   * Super precision for 2/pi in 24-bit chunks, for use in
   * {@link #remPiOver2()}.
   */
  private static final int TWO_OVER_PI[] = {
    0xa2f983, 0x6e4e44, 0x1529fc, 0x2757d1, 0xf534dd, 0xc0db62,
    0x95993c, 0x439041, 0xfe5163, 0xabdebb, 0xc561b7, 0x246e3a,
    0x424dd2, 0xe00649, 0x2eea09, 0xd1921c, 0xfe1deb, 0x1cb129,
    0xa73ee8, 0x8235f5, 0x2ebb44, 0x84e99c, 0x7026b4, 0x5f7e41,
    0x3991d6, 0x398353, 0x39f49c, 0x845f8b, 0xbdf928, 0x3b1ff8,
    0x97ffde, 0x05980f, 0xef2f11, 0x8b5a0a, 0x6d1f6d, 0x367ecf,
    0x27cb09, 0xb74f46, 0x3f669e, 0x5fea2d, 0x7527ba, 0xc7ebe5,
    0xf17b3d, 0x0739f7, 0x8a5292, 0xea6bfb, 0x5fb11f, 0x8d5d08,
    0x560330, 0x46fc7b, 0x6babf0, 0xcfbc20, 0x9af436, 0x1da9e3,
    0x91615e, 0xe61b08, 0x659985, 0x5f14a0, 0x68408d, 0xffd880,
    0x4d7327, 0x310606, 0x1556ca, 0x73a8c9, 0x60e27b, 0xc08c6b,
  };

  /**
   * Super precision for pi/2 in 24-bit chunks, for use in
   * {@link #remPiOver2()}.
   */
  private static final double PI_OVER_TWO[] = {
    1.570796251296997, // Long bits 0x3ff921fb40000000L.
    7.549789415861596e-8, // Long bits 0x3e74442d00000000L.
    5.390302529957765e-15, // Long bits 0x3cf8469880000000L.
    3.282003415807913e-22, // Long bits 0x3b78cc5160000000L.
    1.270655753080676e-29, // Long bits 0x39f01b8380000000L.
    1.2293330898111133e-36, // Long bits 0x387a252040000000L.
    2.7337005381646456e-44, // Long bits 0x36e3822280000000L.
    2.1674168387780482e-51, // Long bits 0x3569f31d00000000L.
  };

  /**
   * More constants related to pi, used in {@link #remPiOver2()} and
   * elsewhere.
   */
  private static final double
    PI_L = 1.2246467991473532e-16, // Long bits 0x3ca1a62633145c07L.
    PIO2_1 = 1.5707963267341256, // Long bits 0x3ff921fb54400000L.
    PIO2_1L = 6.077100506506192e-11, // Long bits 0x3dd0b4611a626331L.
    PIO2_2 = 6.077100506303966e-11, // Long bits 0x3dd0b4611a600000L.
    PIO2_2L = 2.0222662487959506e-21, // Long bits 0x3ba3198a2e037073L.
    PIO2_3 = 2.0222662487111665e-21, // Long bits 0x3ba3198a2e000000L.
    PIO2_3L = 8.4784276603689e-32; // Long bits 0x397b839a252049c1L.

  /**
   * Natural log and square root constants, for calculation of
   * {@link #exp(double)}, {@link #log(double)} and
   * {@link #power(double, double)}. CP is 2/(3*ln(2)).
   */
  private static final double
    SQRT_1_5 = 1.224744871391589, // Long bits 0x3ff3988e1409212eL.
    SQRT_2 = 1.4142135623730951, // Long bits 0x3ff6a09e667f3bcdL.
    SQRT_3 = 1.7320508075688772, // Long bits 0x3ffbb67ae8584caaL.
    EXP_LIMIT_H = 709.782712893384, // Long bits 0x40862e42fefa39efL.
    EXP_LIMIT_L = -745.1332191019411, // Long bits 0xc0874910d52d3051L.
    CP = 0.9617966939259756, // Long bits 0x3feec709dc3a03fdL.
    CP_H = 0.9617967009544373, // Long bits 0x3feec709e0000000L.
    CP_L = -7.028461650952758e-9, // Long bits 0xbe3e2fe0145b01f5L.
    LN2 = 0.6931471805599453, // Long bits 0x3fe62e42fefa39efL.
    LN2_H = 0.6931471803691238, // Long bits 0x3fe62e42fee00000L.
    LN2_L = 1.9082149292705877e-10, // Long bits 0x3dea39ef35793c76L.
    INV_LN2 = 1.4426950408889634, // Long bits 0x3ff71547652b82feL.
    INV_LN2_H = 1.4426950216293335, // Long bits 0x3ff7154760000000L.
    INV_LN2_L = 1.9259629911266175e-8; // Long bits 0x3e54ae0bf85ddf44L.

  /**
   * Constants for computing {@link #log(double)}.
   */
  private static final double
    LG1 = 0.6666666666666735, // Long bits 0x3fe5555555555593L.
    LG2 = 0.3999999999940942, // Long bits 0x3fd999999997fa04L.
    LG3 = 0.2857142874366239, // Long bits 0x3fd2492494229359L.
    LG4 = 0.22222198432149784, // Long bits 0x3fcc71c51d8e78afL.
    LG5 = 0.1818357216161805, // Long bits 0x3fc7466496cb03deL.
    LG6 = 0.15313837699209373, // Long bits 0x3fc39a09d078c69fL.
    LG7 = 0.14798198605116586; // Long bits 0x3fc2f112df3e5244L.

  /**
   * Constants for computing {@link #pow(double, double)}. L and P are
   * coefficients for series; OVT is -(1024-log2(ovfl+.5ulp)); and DP is ???.
   * The P coefficients also calculate {@link #exp(double)}.
   */
  private static final double
    L1 = 0.5999999999999946, // Long bits 0x3fe3333333333303L.
    L2 = 0.4285714285785502, // Long bits 0x3fdb6db6db6fabffL.
    L3 = 0.33333332981837743, // Long bits 0x3fd55555518f264dL.
    L4 = 0.272728123808534, // Long bits 0x3fd17460a91d4101L.
    L5 = 0.23066074577556175, // Long bits 0x3fcd864a93c9db65L.
    L6 = 0.20697501780033842, // Long bits 0x3fca7e284a454eefL.
    P1 = 0.16666666666666602, // Long bits 0x3fc555555555553eL.
    P2 = -2.7777777777015593e-3, // Long bits 0xbf66c16c16bebd93L.
    P3 = 6.613756321437934e-5, // Long bits 0x3f11566aaf25de2cL.
    P4 = -1.6533902205465252e-6, // Long bits 0xbebbbd41c5d26bf1L.
    P5 = 4.1381367970572385e-8, // Long bits 0x3e66376972bea4d0L.
    DP_H = 0.5849624872207642, // Long bits 0x3fe2b80340000000L.
    DP_L = 1.350039202129749e-8, // Long bits 0x3e4cfdeb43cfd006L.
    OVT = 8.008566259537294e-17; // Long bits 0x3c971547652b82feL.

  /**
   * Coefficients for computing {@link #sin(double)}.
   */
  private static final double
    S1 = -0.16666666666666632, // Long bits 0xbfc5555555555549L.
    S2 = 8.33333333332249e-3, // Long bits 0x3f8111111110f8a6L.
    S3 = -1.984126982985795e-4, // Long bits 0xbf2a01a019c161d5L.
    S4 = 2.7557313707070068e-6, // Long bits 0x3ec71de357b1fe7dL.
    S5 = -2.5050760253406863e-8, // Long bits 0xbe5ae5e68a2b9cebL.
    S6 = 1.58969099521155e-10; // Long bits 0x3de5d93a5acfd57cL.

  /**
   * Coefficients for computing {@link #cos(double)}.
   */
  private static final double
    C1 = 0.0416666666666666, // Long bits 0x3fa555555555554cL.
    C2 = -1.388888888887411e-3, // Long bits 0xbf56c16c16c15177L.
    C3 = 2.480158728947673e-5, // Long bits 0x3efa01a019cb1590L.
    C4 = -2.7557314351390663e-7, // Long bits 0xbe927e4f809c52adL.
    C5 = 2.087572321298175e-9, // Long bits 0x3e21ee9ebdb4b1c4L.
    C6 = -1.1359647557788195e-11; // Long bits 0xbda8fae9be8838d4L.

  /**
   * Coefficients for computing {@link #tan(double)}.
   */
  private static final double
    T0 = 0.3333333333333341, // Long bits 0x3fd5555555555563L.
    T1 = 0.13333333333320124, // Long bits 0x3fc111111110fe7aL.
    T2 = 0.05396825397622605, // Long bits 0x3faba1ba1bb341feL.
    T3 = 0.021869488294859542, // Long bits 0x3f9664f48406d637L.
    T4 = 8.8632398235993e-3, // Long bits 0x3f8226e3e96e8493L.
    T5 = 3.5920791075913124e-3, // Long bits 0x3f6d6d22c9560328L.
    T6 = 1.4562094543252903e-3, // Long bits 0x3f57dbc8fee08315L.
    T7 = 5.880412408202641e-4, // Long bits 0x3f4344d8f2f26501L.
    T8 = 2.464631348184699e-4, // Long bits 0x3f3026f71a8d1068L.
    T9 = 7.817944429395571e-5, // Long bits 0x3f147e88a03792a6L.
    T10 = 7.140724913826082e-5, // Long bits 0x3f12b80f32f0a7e9L.
    T11 = -1.8558637485527546e-5, // Long bits 0xbef375cbdb605373L.
    T12 = 2.590730518636337e-5; // Long bits 0x3efb2a7074bf7ad4L.

  /**
   * Coefficients for computing {@link #asin(double)} and
   * {@link #acos(double)}.
   */
  private static final double
    PS0 = 0.16666666666666666, // Long bits 0x3fc5555555555555L.
    PS1 = -0.3255658186224009, // Long bits 0xbfd4d61203eb6f7dL.
    PS2 = 0.20121253213486293, // Long bits 0x3fc9c1550e884455L.
    PS3 = -0.04005553450067941, // Long bits 0xbfa48228b5688f3bL.
    PS4 = 7.915349942898145e-4, // Long bits 0x3f49efe07501b288L.
    PS5 = 3.479331075960212e-5, // Long bits 0x3f023de10dfdf709L.
    QS1 = -2.403394911734414, // Long bits 0xc0033a271c8a2d4bL.
    QS2 = 2.0209457602335057, // Long bits 0x40002ae59c598ac8L.
    QS3 = -0.6882839716054533, // Long bits 0xbfe6066c1b8d0159L.
    QS4 = 0.07703815055590194; // Long bits 0x3fb3b8c5b12e9282L.

  /**
   * Coefficients for computing {@link #atan(double)}.
   */
  private static final double
    ATAN_0_5H = 0.4636476090008061, // Long bits 0x3fddac670561bb4fL.
    ATAN_0_5L = 2.2698777452961687e-17, // Long bits 0x3c7a2b7f222f65e2L.
    ATAN_1_5H = 0.982793723247329, // Long bits 0x3fef730bd281f69bL.
    ATAN_1_5L = 1.3903311031230998e-17, // Long bits 0x3c7007887af0cbbdL.
    AT0 = 0.3333333333333293, // Long bits 0x3fd555555555550dL.
    AT1 = -0.19999999999876483, // Long bits 0xbfc999999998ebc4L.
    AT2 = 0.14285714272503466, // Long bits 0x3fc24924920083ffL.
    AT3 = -0.11111110405462356, // Long bits 0xbfbc71c6fe231671L.
    AT4 = 0.09090887133436507, // Long bits 0x3fb745cdc54c206eL.
    AT5 = -0.0769187620504483, // Long bits 0xbfb3b0f2af749a6dL.
    AT6 = 0.06661073137387531, // Long bits 0x3fb10d66a0d03d51L.
    AT7 = -0.058335701337905735, // Long bits 0xbfadde2d52defd9aL.
    AT8 = 0.049768779946159324, // Long bits 0x3fa97b4b24760debL.
    AT9 = -0.036531572744216916, // Long bits 0xbfa2b4442c6a6c2fL.
    AT10 = 0.016285820115365782; // Long bits 0x3f90ad3ae322da11L.

  /**
   * Helper function for reducing an angle to a multiple of pi/2 within
   * [-pi/4, pi/4].
   *
   * @param x the angle; not infinity or NaN, and outside pi/4
   * @param y an array of 2 doubles modified to hold the remander x % pi/2
   * @return the quadrant of the result, mod 4: 0: [-pi/4, pi/4],
   *         1: [pi/4, 3*pi/4], 2: [3*pi/4, 5*pi/4], 3: [-3*pi/4, -pi/4]
   */
  private static int remPiOver2(double x, double[] y)
  {
    boolean negative = x < 0;
    x = abs(x);
    double z;
    int n;
    if (Configuration.DEBUG && (x <= PI / 4 || x != x
                                || x == Double.POSITIVE_INFINITY))
      throw new InternalError("Assertion failure");
    if (x < 3 * PI / 4) // If |x| is small.
      {
        z = x - PIO2_1;
        if ((float) x != (float) (PI / 2)) // 33+53 bit pi is good enough.
          {
            y[0] = z - PIO2_1L;
            y[1] = z - y[0] - PIO2_1L;
          }
        else // Near pi/2, use 33+33+53 bit pi.
          {
            z -= PIO2_2;
            y[0] = z - PIO2_2L;
            y[1] = z - y[0] - PIO2_2L;
          }
        n = 1;
      }
    else if (x <= TWO_20 * PI / 2) // Medium size.
      {
        n = (int) (2 / PI * x + 0.5);
        z = x - n * PIO2_1;
        double w = n * PIO2_1L; // First round good to 85 bits.
        y[0] = z - w;
        if (n >= 32 || (float) x == (float) (w))
          {
            if (x / y[0] >= TWO_16) // Second iteration, good to 118 bits.
              {
                double t = z;
                w = n * PIO2_2;
                z = t - w;
                w = n * PIO2_2L - (t - z - w);
                y[0] = z - w;
                if (x / y[0] >= TWO_49) // Third iteration, 151 bits accuracy.
                  {
                    t = z;
                    w = n * PIO2_3;
                    z = t - w;
                    w = n * PIO2_3L - (t - z - w);
                    y[0] = z - w;
                  }
              }
          }
        y[1] = z - y[0] - w;
      }
    else
      {
        // All other (large) arguments.
        int e0 = (int) (Double.doubleToLongBits(x) >> 52) - 1046;
        z = scale(x, -e0); // e0 = ilogb(z) - 23.
        double[] tx = new double[3];
        for (int i = 0; i < 2; i++)
          {
            tx[i] = (int) z;
            z = (z - tx[i]) * TWO_24;
          }
        tx[2] = z;
        int nx = 2;
        while (tx[nx] == 0)
          nx--;
        n = remPiOver2(tx, y, e0, nx);
      }
    if (negative)
      {
        y[0] = -y[0];
        y[1] = -y[1];
        return -n;
      }
    return n;
  }

  /**
   * Helper function for reducing an angle to a multiple of pi/2 within
   * [-pi/4, pi/4].
   *
   * @param x the positive angle, broken into 24-bit chunks
   * @param y an array of 2 doubles modified to hold the remander x % pi/2
   * @param e0 the exponent of x[0]
   * @param nx the last index used in x
   * @return the quadrant of the result, mod 4: 0: [-pi/4, pi/4],
   *         1: [pi/4, 3*pi/4], 2: [3*pi/4, 5*pi/4], 3: [-3*pi/4, -pi/4]
   */
  private static int remPiOver2(double[] x, double[] y, int e0, int nx)
  {
    int i;
    int ih;
    int n;
    double fw;
    double z;
    int[] iq = new int[20];
    double[] f = new double[20];
    double[] q = new double[20];
    boolean recompute = false;

    // Initialize jk, jz, jv, q0; note that 3>q0.
    int jk = 4;
    int jz = jk;
    int jv = max((e0 - 3) / 24, 0);
    int q0 = e0 - 24 * (jv + 1);

    // Set up f[0] to f[nx+jk] where f[nx+jk] = TWO_OVER_PI[jv+jk].
    int j = jv - nx;
    int m = nx + jk;
    for (i = 0; i <= m; i++, j++)
      f[i] = (j < 0) ? 0 : TWO_OVER_PI[j];

    // Compute q[0],q[1],...q[jk].
    for (i = 0; i <= jk; i++)
      {
        for (j = 0, fw = 0; j <= nx; j++)
          fw += x[j] * f[nx + i - j];
        q[i] = fw;
      }

    do
      {
        // Distill q[] into iq[] reversingly.
        for (i = 0, j = jz, z = q[jz]; j > 0; i++, j--)
          {
            fw = (int) (1 / TWO_24 * z);
            iq[i] = (int) (z - TWO_24 * fw);
            z = q[j - 1] + fw;
          }

        // Compute n.
        z = scale(z, q0);
        z -= 8 * floor(z * 0.125); // Trim off integer >= 8.
        n = (int) z;
        z -= n;
        ih = 0;
        if (q0 > 0) // Need iq[jz-1] to determine n.
          {
            i = iq[jz - 1] >> (24 - q0);
            n += i;
            iq[jz - 1] -= i << (24 - q0);
            ih = iq[jz - 1] >> (23 - q0);
          }
        else if (q0 == 0)
          ih = iq[jz - 1] >> 23;
        else if (z >= 0.5)
          ih = 2;

        if (ih > 0) // If q > 0.5.
          {
            n += 1;
            int carry = 0;
            for (i = 0; i < jz; i++) // Compute 1-q.
              {
                j = iq[i];
                if (carry == 0)
                  {
                    if (j != 0)
                      {
                        carry = 1;
                        iq[i] = 0x1000000 - j;
                      }
                  }
                else
                  iq[i] = 0xffffff - j;
              }
            switch (q0)
              {
              case 1: // Rare case: chance is 1 in 12 for non-default.
                iq[jz - 1] &= 0x7fffff;
                break;
              case 2:
                iq[jz - 1] &= 0x3fffff;
              }
            if (ih == 2)
              {
                z = 1 - z;
                if (carry != 0)
                  z -= scale(1, q0);
              }
          }

        // Check if recomputation is needed.
        if (z == 0)
          {
            j = 0;
            for (i = jz - 1; i >= jk; i--)
              j |= iq[i];
            if (j == 0) // Need recomputation.
              {
                int k;
                for (k = 1; iq[jk - k] == 0; k++); // k = no. of terms needed.

                for (i = jz + 1; i <= jz + k; i++) // Add q[jz+1] to q[jz+k].
                  {
                    f[nx + i] = TWO_OVER_PI[jv + i];
                    for (j = 0, fw = 0; j <= nx; j++)
                      fw += x[j] * f[nx + i - j];
                    q[i] = fw;
                  }
                jz += k;
                recompute = true;
              }
          }
      }
    while (recompute);

    // Chop off zero terms.
    if (z == 0)
      {
        jz--;
        q0 -= 24;
        while (iq[jz] == 0)
          {
            jz--;
            q0 -= 24;
          }
      }
    else // Break z into 24-bit if necessary.
      {
        z = scale(z, -q0);
        if (z >= TWO_24)
          {
            fw = (int) (1 / TWO_24 * z);
            iq[jz] = (int) (z - TWO_24 * fw);
            jz++;
            q0 += 24;
            iq[jz] = (int) fw;
          }
        else
          iq[jz] = (int) z;
      }

    // Convert integer "bit" chunk to floating-point value.
    fw = scale(1, q0);
    for (i = jz; i >= 0; i--)
      {
        q[i] = fw * iq[i];
        fw *= 1 / TWO_24;
      }

    // Compute PI_OVER_TWO[0,...,jk]*q[jz,...,0].
    double[] fq = new double[20];
    for (i = jz; i >= 0; i--)
      {
        fw = 0;
        for (int k = 0; k <= jk && k <= jz - i; k++)
          fw += PI_OVER_TWO[k] * q[i + k];
        fq[jz - i] = fw;
      }

    // Compress fq[] into y[].
    fw = 0;
    for (i = jz; i >= 0; i--)
      fw += fq[i];
    y[0] = (ih == 0) ? fw : -fw;
    fw = fq[0] - fw;
    for (i = 1; i <= jz; i++)
      fw += fq[i];
    y[1] = (ih == 0) ? fw : -fw;
    return n;
  }

  /**
   * Helper method for scaling a double by a power of 2.
   *
   * @param x the double
   * @param n the scale; |n| < 2048
   * @return x * 2**n
   */
  private static double scale(double x, int n)
  {
    if (Configuration.DEBUG && abs(n) >= 2048)
      throw new InternalError("Assertion failure");
    if (x == 0 || x == Double.NEGATIVE_INFINITY
        || ! (x < Double.POSITIVE_INFINITY) || n == 0)
      return x;
    long bits = Double.doubleToLongBits(x);
    int exp = (int) (bits >> 52) & 0x7ff;
    if (exp == 0) // Subnormal x.
      {
        x *= TWO_54;
        exp = ((int) (Double.doubleToLongBits(x) >> 52) & 0x7ff) - 54;
      }
    exp += n;
    if (exp > 0x7fe) // Overflow.
      return Double.POSITIVE_INFINITY * x;
    if (exp > 0) // Normal.
      return Double.longBitsToDouble((bits & 0x800fffffffffffffL)
                                     | ((long) exp << 52));
    if (exp <= -54)
      return 0 * x; // Underflow.
    exp += 54; // Subnormal result.
    x = Double.longBitsToDouble((bits & 0x800fffffffffffffL)
                                | ((long) exp << 52));
    return x * (1 / TWO_54);
  }

  /**
   * Helper trig function; computes sin in range [-pi/4, pi/4].
   *
   * @param x angle within about pi/4
   * @param y tail of x, created by remPiOver2
   * @return sin(x+y)
   */
  private static double sin(double x, double y)
  {
    if (Configuration.DEBUG && abs(x + y) > 0.7854)
      throw new InternalError("Assertion failure");
    if (abs(x) < 1 / TWO_27)
      return x;  // If |x| ~< 2**-27, already know answer.

    double z = x * x;
    double v = z * x;
    double r = S2 + z * (S3 + z * (S4 + z * (S5 + z * S6)));
    if (y == 0)
      return x + v * (S1 + z * r);
    return x - ((z * (0.5 * y - v * r) - y) - v * S1);
  }

  /**
   * Helper trig function; computes cos in range [-pi/4, pi/4].
   *
   * @param x angle within about pi/4
   * @param y tail of x, created by remPiOver2
   * @return cos(x+y)
   */
  private static double cos(double x, double y)
  {
    if (Configuration.DEBUG && abs(x + y) > 0.7854)
      throw new InternalError("Assertion failure");
    x = abs(x);
    if (x < 1 / TWO_27)
      return 1;  // If |x| ~< 2**-27, already know answer.

    double z = x * x;
    double r = z * (C1 + z * (C2 + z * (C3 + z * (C4 + z * (C5 + z * C6)))));

    if (x < 0.3)
      return 1 - (0.5 * z - (z * r - x * y));

    double qx = (x > 0.78125) ? 0.28125 : (x * 0.25);
    return 1 - qx - ((0.5 * z - qx) - (z * r - x * y));
  }

  /**
   * Helper trig function; computes tan in range [-pi/4, pi/4].
   *
   * @param x angle within about pi/4
   * @param y tail of x, created by remPiOver2
   * @param invert true iff -1/tan should be returned instead
   * @return tan(x+y)
   */
  private static double tan(double x, double y, boolean invert)
  {
    // PI/2 is irrational, so no double is a perfect multiple of it.
    if (Configuration.DEBUG && (abs(x + y) > 0.7854 || (x == 0 && invert)))
      throw new InternalError("Assertion failure");
    boolean negative = x < 0;
    if (negative)
      {
        x = -x;
        y = -y;
      }
    if (x < 1 / TWO_28) // If |x| ~< 2**-28, already know answer.
      return (negative ? -1 : 1) * (invert ? -1 / x : x);

    double z;
    double w;
    boolean large = x >= 0.6744;
    if (large)
      {
        z = PI / 4 - x;
        w = PI_L / 4 - y;
        x = z + w;
        y = 0;
      }
    z = x * x;
    w = z * z;
    // Break x**5*(T1+x**2*T2+...) into
    //   x**5(T1+x**4*T3+...+x**20*T11)
    // + x**5(x**2*(T2+x**4*T4+...+x**22*T12)).
    double r = T1 + w * (T3 + w * (T5 + w * (T7 + w * (T9 + w * T11))));
    double v = z * (T2 + w * (T4 + w * (T6 + w * (T8 + w * (T10 + w * T12)))));
    double s = z * x;
    r = y + z * (s * (r + v) + y);
    r += T0 * s;
    w = x + r;
    if (large)
      {
        v = invert ? -1 : 1;
        return (negative ? -1 : 1) * (v - 2 * (x - (w * w / (w + v) - r)));
      }
    if (! invert)
      return w;

    // Compute -1.0/(x+r) accurately.
    z = (float) w;
    v = r - (z - x);
    double a = -1 / w;
    double t = (float) a;
    return t + a * (1 + t * z + t * v);
  }
}
