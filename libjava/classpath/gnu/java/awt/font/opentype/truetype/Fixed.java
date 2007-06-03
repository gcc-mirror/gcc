/* Fixed.java -- Fixed-point arithmetics for TrueType coordinates.
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


package gnu.java.awt.font.opentype.truetype;


/**
 * A utility class for fixed-point arithmetics, where numbers are
 * represented with 26 dot 6 digits. This representation is used by
 * TrueType coordinates.
 *
 * <p>A good compiler will inline calls of methods in this class.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public final class Fixed
{
  public static final int ONE = 1<<6;


  /**
   * The constructor is private so nobody can use it.
   */
  private Fixed()
  {
  }


  /**
   * Multiplies two fixed-point numbers.
   */
  public static int mul(int a, int b)
  {
    return (int) ((((long) a) * b) >> 6);
  }

  public static int mul16(int a, int b)
  {
    return (int) ((((long) a) * b) >> 16);
  }

  public static int div(int a, int b)
  {
    return (int) ((((long) a) << 6) / b);
  }

  public static int div16(int a, int b)
  {
    return (int) ((((long) a) << 16) / b);
  }

  public static int ceil(int a)
  {
    return (a + 63) & -64;
  }


  public static int floor(int a)
  {
    return a & -64;
  }


  /**
   * Calculates the length of a fixed-point vector.
   */
  public static int vectorLength(int x, int y)
  {
    int shift;
    float fx, fy;

    if (x == 0)
      return Math.abs(y);
    else if (y == 0)
      return Math.abs(x);

    /* Use the FPU. */
    fx = ((float) x) / 64.0f;
    fy = ((float) y) / 64.0f;
    return (int) (Math.sqrt(fx * fx + fy * fy) * 64.0);
  }


  public static int intValue(int f)
  {
    return f >> 6;
  }


  public static float floatValue(int f)
  {
    return ((float) f) / 64;
  }
  public static float floatValue16(int f)
  {
    return ((float) f) / 65536;
  }

  public static double doubleValue(int f)
  {
    return ((double) f) / 64;
  }


  public static int valueOf(float f)
  {
    return (int) (f * 64);
  }


  public static int valueOf(double d)
  {
    return (int) (d * 64);
  }

  public static int valueOf16(double d)
  {
    return (int) (d * (1 << 16));
  }

  /**
   * Makes a string representation of a fixed-point number.
   */
  public static String toString(int f)
  {
    return String.valueOf(floatValue(f));
  }


  public static String toString(int x, int y)
  {
    StringBuffer sbuf = new StringBuffer(40);
    sbuf.append('(');
    sbuf.append(((float) x) / 64);
    sbuf.append(", ");
    sbuf.append(((float) y) / 64);
    sbuf.append(')');
    return sbuf.toString();
  }
}
