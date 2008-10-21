/* JPEGQTable.java --
 Copyright (C)  2006  Free Software Foundation, Inc.

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


package javax.imageio.plugins.jpeg;

import gnu.java.lang.CPStringBuilder;

/**
 * The JPEGQTable class represents a quantization table that can be
 * used to encode or decode a JPEG stream.  The standard JPEG
 * luminance and chrominance quantization tables are provided as
 * static fields.  Table entries are stored in natural order, not
 * zig-zag order.
 */
public class JPEGQTable
{
  /**
   * The table entries, stored in natural order.
   */
  private int[] table;

  /**
   * The standard JPEG luminance quantization table.  Values are
   * stored in natural order.
   */
  public static final JPEGQTable K1Luminance = new JPEGQTable(new int[]
      {
        16, 11, 10, 16,  24,  40,  51,  61,
        12, 12, 14, 19,  26,  58,  60,  55,
        14, 13, 16, 24,  40,  57,  69,  56,
        14, 17, 22, 29,  51,  87,  80,  62,
        18, 22, 37, 56,  68, 109, 103,  77,
        24, 35, 55, 64,  81, 104, 113,  92,
        49, 64, 78, 87, 103, 121, 120, 101,
        72, 92, 95, 98, 112, 100, 103,  99
      }, false);

  /**
   * The standard JPEG luminance quantization table, scaled by
   * one-half.  Values are stored in natural order.
   */
  public static final JPEGQTable K1Div2Luminance =
    K1Luminance.getScaledInstance(0.5f, true);

  /**
   * The standard JPEG chrominance quantization table.  Values are
   * stored in natural order.
   */
  public static final JPEGQTable K2Chrominance = new JPEGQTable(new int[]
      {
        17, 18, 24, 47, 99, 99, 99, 99,
        18, 21, 26, 66, 99, 99, 99, 99,
        24, 26, 56, 99, 99, 99, 99, 99,
        47, 66, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99
      }, false);

  /**
   * The standard JPEG chrominance quantization table, scaled by
   * one-half.  Values are stored in natural order.
   */
  public static final JPEGQTable K2Div2Chrominance =
    K2Chrominance.getScaledInstance(0.5f, true);

  /**
   * Construct a new JPEG quantization table.  A copy is created of
   * the table argument.
   *
   * @param table the 64-element value table, stored in natural order
   *
   * @throws IllegalArgumentException if the table is null or if
   * table's length is not equal to 64.
   */
  public JPEGQTable(int[] table)
  {
    this(checkTable(table), true);
  }

  /**
   * Private constructor that avoids unnecessary copying and argument
   * checking.
   *
   * @param table the 64-element value table, stored in natural order
   * @param copy true if a copy should be created of the given table
   */
  private JPEGQTable(int[] table, boolean copy)
  {
    this.table = copy ? (int[]) table.clone() : table;
  }

  private static int[] checkTable(int[] table)
  {
    if (table == null || table.length != 64)
      throw new IllegalArgumentException("invalid JPEG quantization table");

    return table;
  }

  /**
   * Retrieve a copy of the quantization values for this table.
   *
   * @return a copy of the quantization value array
   */
  public int[] getTable()
  {
    return (int[]) table.clone();
  }

  /**
   * Retrieve a copy of this JPEG quantization table with every value
   * scaled by the given scale factor, and clamped from 1 to 255
   * baseline or from 1 to 32767 otherwise.
   *
   * @param scaleFactor the factor by which to scale this table
   * @param forceBaseline clamp scaled values to a maximum of 255 if
   * true, 32767 if false
   *
   * @return a new scaled JPEG quantization table
   */
  public JPEGQTable getScaledInstance(float scaleFactor,
                                      boolean forceBaseline)
  {
    int[] scaledTable = getTable();
    int max = forceBaseline ? 255 : 32767;

    for (int i = 0; i < scaledTable.length; i++)
      {
        scaledTable[i] = Math.round (scaleFactor * (float) scaledTable[i]);
        if (scaledTable[i] < 1)
          scaledTable[i] = 1;
        else if (scaledTable[i] > max)
          scaledTable[i] = max;
      }

    // Do not copy scaledTable.  It is already a copy because we used
    // getTable to retrieve it.
    return new JPEGQTable(scaledTable, false);
  }

  /**
   * Create a string representing this JPEG quantization table.
   */
  public String toString()
  {
    CPStringBuilder buffer = new CPStringBuilder();

    buffer.append("JPEGQTable:\n");
    for (int i = 0; i < 8; i++)
      {
        buffer.append("        ");
        for (int j = 0; j < 8; j++)
          {
            buffer.append(table[i * 8 + j] + " ");
          }
        buffer.append("\n");
      }

    return buffer.toString();
  }
}
