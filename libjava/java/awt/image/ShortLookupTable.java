/* ShortLookupTable.java -- Java class for a pixel translation table.
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package java.awt.image;

/**
 * ShortLookupTable represents translation arrays for pixel values.  It wraps
 * one or more data arrays for each layer (or component) in an image, such as
 * Alpha, R, G, and B.  When doing translation, the offset is subtracted from
 * the pixel values to allow a subset of an array to be used.
 *
 * @author <a href="mailto:jlquinn@optonline.net">Jerry Quinn</a>
 * @version 1.0
 */
public class ShortLookupTable extends LookupTable
{
  // Array of translation tables.
  private short data[][];

  /**
   * Creates a new <code>ShortLookupTable</code> instance.
   *
   * Offset is subtracted from pixel values when looking up in the translation
   * tables.  If data.length is one, the same table is applied to all pixel
   * components.
   * 
   * @param offset Offset to be subtracted.
   * @param data Array of lookup tables.
   * @exception IllegalArgumentException if offset < 0 or data.length < 1.
   */
  public ShortLookupTable(int offset, short[][] data)
    throws IllegalArgumentException
  {
    super(offset, data.length);
    this.data = data;
  }

  /**
   * Creates a new <code>ShortLookupTable</code> instance.
   *
   * Offset is subtracted from pixel values when looking up in the translation
   * table.  The same table is applied to all pixel components.
   * 
   * @param offset Offset to be subtracted.
   * @param data Lookup table for all components.
   * @exception IllegalArgumentException if offset < 0.
   */
  public ShortLookupTable(int offset, short[] data)
    throws IllegalArgumentException
  {
    super(offset, 1);
    this.data = new short[][] {data};
  }

  /** Return the lookup tables. */
  public final short[][] getTable()
  {
    return data;
  }

  /**
   * Return translated values for a pixel.
   *
   * For each value in the pixel src, use the value minus offset as an index
   * in the component array and copy the value there to the output for the
   * component.  If dest is null, the output is a new array, otherwise the
   * translated values are written to dest.  Dest can be the same array as
   * src.
   *
   * For example, if the pixel src is [2, 4, 3], and offset is 1, the output
   * is [comp1[1], comp2[3], comp3[2]], where comp1, comp2, and comp3 are the
   * translation arrays.
   *
   * @param src Component values of a pixel.
   * @param dest Destination array for values, or null.
   * @return Translated values for the pixel.
   */
  public int[] lookupPixel(int[] src, int[] dst)
    throws ArrayIndexOutOfBoundsException
  {
    if (dst == null)
      dst = new int[numComponents];

    if (data.length == 1)
      for (int i=0; i < src.length; i++)
	dst[i] = data[0][src[i] - offset];
    else
      for (int i=0; i < src.length; i++)
	dst[i] = data[i][src[i] - offset];
      
    return dst;
  }

  /**
   * Return translated values for a pixel.
   *
   * For each value in the pixel src, use the value minus offset as an index
   * in the component array and copy the value there to the output for the
   * component.  If dest is null, the output is a new array, otherwise the
   * translated values are written to dest.  Dest can be the same array as
   * src.
   *
   * For example, if the pixel src is [2, 4, 3], and offset is 1, the output
   * is [comp1[1], comp2[3], comp3[2]], where comp1, comp2, and comp3 are the
   * translation arrays.
   *
   * @param src Component values of a pixel.
   * @param dest Destination array for values, or null.
   * @return Translated values for the pixel.
   */
  public short[] lookupPixel(short[] src, short[] dst)
    throws ArrayIndexOutOfBoundsException
  {
    if (dst == null)
      dst = new short[numComponents];

    if (data.length == 1)
      for (int i=0; i < src.length; i++)
	dst[i] = data[0][((int)src[i]) - offset];
    else
      for (int i=0; i < src.length; i++)
	dst[i] = data[i][((int)src[i]) - offset];
      
    return dst;

  }
}
