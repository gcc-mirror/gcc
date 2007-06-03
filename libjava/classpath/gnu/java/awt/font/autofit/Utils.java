/* Utils.java -- A collection of utility functions for the autofitter
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


package gnu.java.awt.font.autofit;

import gnu.java.awt.font.opentype.truetype.Fixed;

/**
 * A collection of utility methods used all around the auto fitter.
 */
class Utils
  implements Constants
{

  private static final int ATAN_BITS = 8;
  private static final byte[] ATAN = new byte[]
  {
     0,  0,  1,  1,  1,  2,  2,  2,
     3,  3,  3,  3,  4,  4,  4,  5,
     5,  5,  6,  6,  6,  7,  7,  7,
     8,  8,  8,  9,  9,  9, 10, 10,
    10, 10, 11, 11, 11, 12, 12, 12,
    13, 13, 13, 14, 14, 14, 14, 15,
    15, 15, 16, 16, 16, 17, 17, 17,
    18, 18, 18, 18, 19, 19, 19, 20,
    20, 20, 21, 21, 21, 21, 22, 22,
    22, 23, 23, 23, 24, 24, 24, 24,
    25, 25, 25, 26, 26, 26, 26, 27,
    27, 27, 28, 28, 28, 28, 29, 29,
    29, 30, 30, 30, 30, 31, 31, 31,
    31, 32, 32, 32, 33, 33, 33, 33,
    34, 34, 34, 34, 35, 35, 35, 35,
    36, 36, 36, 36, 37, 37, 37, 38,
    38, 38, 38, 39, 39, 39, 39, 40,
    40, 40, 40, 41, 41, 41, 41, 42,
    42, 42, 42, 42, 43, 43, 43, 43,
    44, 44, 44, 44, 45, 45, 45, 45,
    46, 46, 46, 46, 46, 47, 47, 47,
    47, 48, 48, 48, 48, 48, 49, 49,
    49, 49, 50, 50, 50, 50, 50, 51,
    51, 51, 51, 51, 52, 52, 52, 52,
    52, 53, 53, 53, 53, 53, 54, 54,
    54, 54, 54, 55, 55, 55, 55, 55,
    56, 56, 56, 56, 56, 57, 57, 57,
    57, 57, 57, 58, 58, 58, 58, 58,
    59, 59, 59, 59, 59, 59, 60, 60,
    60, 60, 60, 61, 61, 61, 61, 61,
    61, 62, 62, 62, 62, 62, 62, 63,
    63, 63, 63, 63, 63, 64, 64, 64
  };

  private static final int ANGLE_PI = 256;
  private static final int ANGLE_PI2 = ANGLE_PI / 2;
  private static final int ANGLE_PI4 = ANGLE_PI / 4;
  private static final int ANGLE_2PI = ANGLE_PI * 2;

  /**
   * Computes the direction constant for the specified vector. The vector is
   * given as differential value already.
   *
   * @param dx the x vector
   * @param dy the y vector
   *
   * @return the direction of that vector, or DIR_NONE, if that vector is not
   *         approximating against one of the major axises
   */
  static int computeDirection(int dx, int dy)
  {
    int dir = DIR_NONE;
    if (dx < 0)
      {
        if (dy < 0)
          {
            if (-dx * 12 < -dy)
              dir = DIR_UP;
            else if (-dy * 12 < -dx)
              dir = DIR_LEFT;
          }
        else // dy >= 0 .
          {
            if (-dx * 12 < dy)
              dir = DIR_DOWN;
            else if (dy * 12 < -dx)
              dir = DIR_LEFT;
          }
      }
    else // dx >= 0 .
      {
        if (dy < 0)
          {
            if (dx * 12 < -dy)
              dir = DIR_UP;
            else if (-dy * 12 < dx)
              dir = DIR_RIGHT;
          }
        else // dy >= 0 .
          {
            if (dx * 12 < dy)
              dir = DIR_DOWN;
            else if (dy * 12 < dx)
              dir = DIR_RIGHT;
          }
      }
    return dir;
  }

  public static int atan(int dx, int dy)
  {
    int angle;
    // Trivial cases.
    if (dy == 0)
      {
        angle = 0;
        if (dx < 0)
          angle = ANGLE_PI;
        return angle;
      }
    else if (dx == 0)
      {
        angle = ANGLE_PI2;
        if (dy < 0)
          angle = - ANGLE_PI2;
        return angle;
      }

    
    angle = 0;
    if (dx < 0)
      {
        dx = -dx;
        dy = -dy;
        angle = ANGLE_PI;
      }
    if (dy < 0)
      {
        int tmp = dx;
        dx = -dy;
        dy = tmp;
        angle -= ANGLE_PI2;
      }
    if (dx == 0 && dy == 0)
      return 0;

    if (dx == dy)
      angle += ANGLE_PI4;
    else if (dx > dy)
      {
        angle += ATAN[Fixed.div(dy, dx) << (ATAN_BITS - 6)];
      }
    else
      {
        angle += ANGLE_PI2 - ATAN[Fixed.div(dx, dy) << (ATAN_BITS - 6)];
      }

    if (angle > ANGLE_PI)
      angle -= ANGLE_2PI;
    return angle;
  }

  public static int angleDiff(int ang1, int ang2)
  {
    int delta = ang2 - ang1;
    delta %= ANGLE_2PI;
    if (delta < 0)
      delta += ANGLE_2PI;
    if (delta > ANGLE_PI)
      delta -= ANGLE_2PI;
    return delta;
  }

  static void sort(int num, int[] array)
  {
    int swap;
    for (int i = 1; i < num; i++)
      {
        for (int j = i; j > 0; j--)
          {
            if (array[j] > array[j - 1])
              break;
            swap = array[j];
            array[j] = array[j - 1];
            array[j - 1] = swap;
          }
      }
  }

  static void sort(int num, Width[] array)
  {
    Width swap;
    for (int i = 1; i < num; i++)
      {
        for (int j = 1; j > 0; j--)
          {
            if (array[j].org > array[j - 1].org)
              break;
            swap = array[j];
            array[j] = array[j - 1];
            array[j - 1] = swap;
          }
      }
  }

  static int pixRound(int val)
  {
    return pixFloor(val + 32);
  }

  static int pixFloor(int val)
  {
    return val & ~63;
  }

  public static int mulDiv(int a, int b, int c)
  {
    long prod = a * b;
    long div = (prod / c);
    return (int) div;
  }

}
