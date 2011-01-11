/* ZigZag.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.javax.imageio.jpeg;

/**
 * This class implements the Zig Zag Algorithm on any array with
 * the same amount of rows and columns. It takes a matrix and in turn builds an
 * encoded byte array (or double array) from it. The adverse is also true, this
 * will take a byte or double array and build a matrix based on the zig zag
 * algorithm.
 * <p>This is used exclusively in the JPEG DCT encoding.</p>
 */
public class ZigZag
{
  public final static boolean ZIGZAG_FORWARD = true;
  public final static boolean ZIGZAG_BACKWARD = false;
  public final static int ZIGZAG_8X8_MAP[] =
  {
   0,   1,  8, 16,  9,  2,  3, 10,
   17, 24, 32, 25, 18, 11,  4,  5,
   12, 19, 26, 33, 40, 48, 41, 34,
   27, 20, 13,  6,  7, 14, 21, 28,
   35, 42, 49, 56, 57, 50, 43, 36,
   29, 22, 15, 23, 30, 37, 44, 51,
   58, 59, 52, 45, 38, 31, 39, 46,
   53, 60, 61, 54, 47, 55, 62, 63
  };

  /**
   * Encodes a matrix of equal width and height to a byte array.
   *
   * @param matrix
   *
   * @return
   */
  public static byte[] encode(byte[][] matrix)
  {
    byte[] buffer = new byte[matrix.length ^ 2];
    boolean direction = ZigZag.ZIGZAG_FORWARD;
    int x = 0, y = 0, index = 0;
    for (int zigIndex = 0; zigIndex < (matrix.length * 2 - 1);
         zigIndex++, direction = !direction)
      {
        if (direction == ZigZag.ZIGZAG_FORWARD)
          {
            while (x >= 0 && y != matrix.length)
              {
                if (x == matrix.length)
                  {
                    x--;
                    y++;
                  }
                buffer[index] = matrix[x][y];
                y++;
                x--;
                index++;
              }
            x++;
          }
        else
          {
            while (y >= 0 && x != matrix.length)
              {
                if (y == matrix.length)
                  {
                    y--;
                    x++;
                  }
                buffer[index] = matrix[x][y];
                y--;
                x++;
                index++;
              }
            y++;
          }
      }
    return (buffer);
  }

  /**
   * Encodes a matrix of equal width and height to a double array
   *
   * @param matrix
   *
   * @return
   */
  public static double[] encode(double[][] matrix)
  {
    double[] buffer = new double[matrix.length * matrix.length];
    boolean direction = ZigZag.ZIGZAG_FORWARD;
    int x = 0, y = 0, index = 0;
    for (int zigIndex = 0; zigIndex < (matrix.length * 2 - 1);
         zigIndex++, direction = !direction)
      {
        if (direction == ZigZag.ZIGZAG_FORWARD)
          {
            while (x >= 0 && y != matrix.length)
              {
                if (x == matrix.length)
                  {
                    x--;
                    y++;
                  }
                buffer[index] = matrix[x][y];
                y++;
                x--;
                index++;
              }
            x++;
          }
        else
          {
            while (y >= 0 && x != matrix.length)
              {
                if (y == matrix.length)
                  {
                    y--;
                    x++;
                  }
                buffer[index] = matrix[x][y];
                y--;
                x++;
                index++;
              }
            y++;
          }
      }
    return (buffer);
  }

  /**
   * Encodes a matrix of equal width and height to a float array
   *
   * @param matrix
   *
   * @return
   */
  public static float[] encode(float[][] matrix)
  {
    float[] buffer = new float[matrix.length * matrix.length];
    boolean direction = ZigZag.ZIGZAG_FORWARD;
    int x = 0, y = 0, index = 0;
    for (int zigIndex = 0; zigIndex < (matrix.length * 2 - 1);
         zigIndex++, direction = !direction)
      {
        if (direction == ZigZag.ZIGZAG_FORWARD)
          {
            while (x >= 0 && y != matrix.length)
              {
                if (x == matrix.length)
                  {
                    x--;
                    y++;
                  }
                buffer[index] = matrix[x][y];
                y++;
                x--;
                index++;
              }
            x++;
          }
        else
          {
            while (y >= 0 && x != matrix.length)
              {
                if (y == matrix.length)
                  {
                    y--;
                    x++;
                  }
                buffer[index] = matrix[x][y];
                y--;
                x++;
                index++;
              }
            y++;
          }
      }
    return (buffer);
  }

  /**
   * Encodes a matrix of equal width and height to a float array
   *
   * @param matrix
   *
   * @return
   */
  public static short[] encode(short[][] matrix)
  {
    short[] buffer = new short[matrix.length * matrix.length];
    boolean direction = ZigZag.ZIGZAG_FORWARD;
    int x = 0, y = 0, index = 0;
    for (int zigIndex = 0; zigIndex < (matrix.length * 2 - 1);
         zigIndex++, direction = !direction)
      {
        if (direction == ZigZag.ZIGZAG_FORWARD)
          {
            while (x >= 0 && y != matrix.length)
              {
                if (x == matrix.length)
                  {
                    x--;
                    y++;
                  }
                buffer[index] = matrix[x][y];
                y++;
                x--;
                index++;
              }
            x++;
          }
        else
          {
            while (y >= 0 && x != matrix.length)
              {
                if (y == matrix.length)
                  {
                    y--;
                    x++;
                  }
                buffer[index] = matrix[x][y];
                y--;
                x++;
                index++;
              }
            y++;
          }
      }
    return (buffer);
  }

  /**
   * Convert a double array into a matrix with the same amount of columns and
   * rows with length sqrt(double array length)
   *
   * @param data
   *
   * @return
   */
  public static double[][] decode(double[] data)
  {
    return decode(data, (int) Math.sqrt(data.length),
                  (int) Math.sqrt(data.length));
  }

  /**
   * Convert a byte array into a matrix with the same amount of columns and
   * rows with length sqrt(double array length)
   *
   * @param data
   *
   * @return
   */
  public static byte[][] decode(byte[] data)
  {
    return decode(data, (int) Math.sqrt(data.length),
                  (int) Math.sqrt(data.length));
  }

  public static int[][] decode(int[] data)
  {
    return decode(data, (int) Math.sqrt(data.length),
                  (int) Math.sqrt(data.length));
  }

  public static byte[][] decode(byte[] data, int width, int height)
  {
    byte[][] buffer = new byte[height][width];

    for (int v = 0; v < height; v++)
      for (int z = 0; z < width; z++)
        buffer[v][z] = 11;

    boolean dir = ZigZag.ZIGZAG_FORWARD;
    int xindex = 0, yindex = 0, dataindex = 0;

    while (xindex < width && yindex < height && dataindex < data.length)
      {
        buffer[yindex][xindex] = data[dataindex];
        dataindex++;

        if (dir == ZigZag.ZIGZAG_FORWARD)
          {
            if (yindex == 0 || xindex == (width - 1))
              {
                dir = ZigZag.ZIGZAG_BACKWARD;
                if (xindex == (width - 1))
                  yindex++;
                else
                  xindex++;
              }
            else
              {
                yindex--;
                xindex++;
              }
          }
        else
          { /* Backwards */
            if (xindex == 0 || yindex == (height - 1))
              {
                dir = ZigZag.ZIGZAG_FORWARD;
                if (yindex == (height - 1))
                  xindex++;
                else
                  yindex++;
              }
            else
              {
                yindex++;
                xindex--;
              }
          }
      }
    return (buffer);
  }

  public static double[][] decode(double[] data, int width, int height)
  {
    double[][] buffer = new double[height][width];

    for (int v = 0; v < height; v++)
      for (int z = 0; z < width; z++)
        buffer[v][z] = 11;

    boolean dir = ZigZag.ZIGZAG_FORWARD;
    int xindex = 0, yindex = 0, dataindex = 0;

    while (xindex < width && yindex < height && dataindex < data.length)
      {
        buffer[yindex][xindex] = data[dataindex];
        dataindex++;
        System.err.println("Setting " + dataindex + " to row: " + yindex
                           + " column: " + xindex + " yourval:"
                           + (yindex*8+xindex));
        if (dir == ZigZag.ZIGZAG_FORWARD)
          {
            if (yindex == 0 || xindex == (width - 1))
              {
                dir = ZigZag.ZIGZAG_BACKWARD;
                if (xindex == (width - 1))
                  yindex++;
                else
                  xindex++;
              }
            else
              {
                yindex--;
                xindex++;
              }
          }
        else
          { /* Backwards */
            if (xindex == 0 || yindex == (height - 1))
              {
                dir = ZigZag.ZIGZAG_FORWARD;
                if (yindex == (height - 1))
                  xindex++;
                else
                  yindex++;
              }
            else
              {
                yindex++;
                xindex--;
              }
          }
      }
    return (buffer);
  }

  public static float[][] decode(float[] data, int width, int height)
  {
    float[][] buffer = new float[height][width];

    for (int v = 0; v < height; v++)
      for (int z = 0; z < width; z++)
        buffer[v][z] = 11;

    boolean dir = ZigZag.ZIGZAG_FORWARD;
    int xindex = 0, yindex = 0, dataindex = 0;

    while (xindex < width && yindex < height && dataindex < data.length)
      {
        buffer[yindex][xindex] = data[dataindex];
        dataindex++;

        if (dir == ZigZag.ZIGZAG_FORWARD)
          {
            if (yindex == 0 || xindex == (width - 1))
              {
                dir = ZigZag.ZIGZAG_BACKWARD;
                if (xindex == (width - 1))
                  yindex++;
                else
                  xindex++;
              }
            else
              {
                yindex--;
                xindex++;
              }
          }
        else
          { /* Backwards */
            if (xindex == 0 || yindex == (height - 1))
              {
                dir = ZigZag.ZIGZAG_FORWARD;
                if (yindex == (height - 1))
                  xindex++;
                else
                  yindex++;
              }
            else
              {
                yindex++;
                xindex--;
              }
          }
      }
    return (buffer);
  }

  public static int[][] decode(int[] data, int width, int height)
  {
    int[][] buffer = new int[height][width];

    for (int v = 0; v < height; v++)
      for (int z = 0; z < width; z++)
        buffer[v][z] = 11;

    boolean dir = ZigZag.ZIGZAG_FORWARD;
    int xindex = 0, yindex = 0, dataindex = 0;

    while (xindex < width && yindex < height && dataindex < data.length)
      {
        buffer[yindex][xindex] = data[dataindex];
        dataindex++;

        if (dir == ZigZag.ZIGZAG_FORWARD)
          {
            if (yindex == 0 || xindex == (width - 1))
              {
                dir = ZigZag.ZIGZAG_BACKWARD;
                if (xindex == (width - 1))
                  yindex++;
                else
                  xindex++;
              }
            else
              {
                yindex--;
                xindex++;
              }
          }
        else
          { /* Backwards */
            if (xindex == 0 || yindex == (height - 1))
              {
                dir = ZigZag.ZIGZAG_FORWARD;
                if (yindex == (height - 1))
                  xindex++;
                else
                  yindex++;
              }
            else
              {
                yindex++;
                xindex--;
              }
          }
      }
    return (buffer);
  }

  public static double[][] decode8x8_map(double input[])
  {
    double[][] output = new double[8][8];
    for(int i=0; i < 64 ; i++)
      output[ZIGZAG_8X8_MAP[i]/8][ZIGZAG_8X8_MAP[i]%8] = input[i];
    return (output);
  }

}
