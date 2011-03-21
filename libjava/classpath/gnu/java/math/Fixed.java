/* Fixed.java -- Utility methods for fixed point arithmetics
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.math;

/**
 * Utility methods for fixed point arithmetics.
 */
public final class Fixed
{

  /**
   * Private constructor to avoid instantiation.
   */
  private Fixed()
  {
    // Forbidden constructor.
  }

  /**
   * Divides two fixed point values with <code>n</code> digits.
   *
   * @param n the number of digits
   * @param a the first operand as fixed point value
   * @param b the second operand as fixed point value
   *
   * @return <code>a / b</code> as fixed point value
   */
  public static int div(int n, int a, int b)
  {
    return (int) ((((long) a) << n) / b);
  }

  /**
   * Multiplies two fixed point values with <code>n</code> digits.
   *
   * @param n the number of digits
   * @param a the first operand as fixed point value
   * @param b the second operand as fixed point value
   *
   * @return <code>a * b</code> as fixed point value
   */
  public static int mul(int n, int a, int b)
  {
    return (int) ((((long) a) * b) >> n);
  }

  /**
   * Returns the ceiling value of a fixed point value <code>a</code> with
   * the <code>n</code> digits.
   *
   * @param n the number of digits
   * @param a the fixed point value
   *
   * @return <code>ceil(a)</code> as fixed point value
   */
  public static int ceil(int n, int a)
  {
    return (a + (1 << n - 1)) & -(1 << n);
  }

  /**
   * Returns the floor value of a fixed point value <code>a</code> with
   * <code>n</code> digits.
   *
   * @param n the number of digits
   * @param a the fixed point value
   *
   * @return <code>floor(a)</code> as fixed point value
   */
  public static int floor(int n, int a)
  {
    return a & -(1 << n);
  }

  /**
   * Truncates the number so that only the digits after the point are left.
   *
   * @param n the number of digits
   * @param a the fixed point value
   *
   * @return the truncated value
   */
  public static int trunc(int n, int a)
  {
    return a & (0xFFFFFFFF >>> 32 - n);
  }

  /**
   * Returns the round value of a fixed point value <code>a</code> with
   * the <code>n</code> digits.
   *
   * @param n the number of digits
   * @param a the fixed point value
   *
   * @return <code>round(a)</code> as fixed point value
   */
  public static int round(int n, int a)
  {
    return (a + (1 << (n - 1))) & -(1 << n);
  }

  /**
   * Returns the fixed point value <code>a</code> with <code>n</code> digits
   * as float.
   *
   * @param n the number of digits
   * @param a the fixed point value
   *
   * @return the float value of <code>a</code>
   */
  public static float floatValue(int n, int a)
  {
    return ((float) a) / (1 << n);
  }

  /**
   * Returns the fixed point value <code>a</code> with <code>n</code> digits
   * as double.
   *
   * @param n the number of digits
   * @param a the fixed point value
   *
   * @return the double value of <code>a</code>
   */
  public static double doubleValue(int n, int a)
  {
    return ((double) a) / (1 << n);
  }

  /**
   * Returns the fixed point value that corresponds to the specified float
   * value <code>a</code> with <code>n</code> digits.
   *
   * @param n the number of digits
   * @param a the float value
   *
   * @return the fixed point value
   */
  public static int fixedValue(int n, float a)
  {
    return (int) (a * (1 << n));
  }

  /**
   * Returns the fixed point value that corresponds to the specified double
   * value <code>a</code> with <code>n</code> digits.
   *
   * @param n the number of digits
   * @param a the double value
   *
   * @return the fixed point value
   */
  public static int fixedValue(int n, double a)
  {
    return (int) (a * (1 << n));
  }

  /**
   * Returns the integer value of the specified fixed point value
   * <code>a</code>. This simply cuts of the digits (== floor(a)).
   *
   * @param n the number of digits
   * @param a the fixed point value
   *
   * @return the integer value
   */
  public static int intValue(int n, int a)
  {
    return a >> n;
  }

  /**
   * Returns a fixed point decimal as rounded integer value.
   *
   * @param n the number of digits
   * @param a the fixed point number
   *
   * @return the fixed point decimal as rounded integer value
   */
  public static int roundIntValue(int n, int a)
  {
    return (a + (1 << (n - 1))) >> n;
  }
}
