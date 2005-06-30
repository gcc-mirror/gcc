/* LookupTable.java -- Java class for a pixel translation table.
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package java.awt.image;

/**
 * LookupTable represents translation arrays for pixel values.  It wraps one
 * or more data arrays for each layer (or component) in an image, such as
 * Alpha, R, G, and B.  When doing translation, the offset is subtracted from
 * the pixel values to allow a subset of an array to be used.
 *
 * @see ByteLookupTable
 * @see ShortLookupTable
 *
 * @author Jerry Quinn (jlquinn@optonline.net)
 * @version 1.0
 */
public abstract class LookupTable
{
  // Not protected since that's part of the public API.
  int offset;
  int numComponents;

  /**
   * Creates a new <code>LookupTable</code> instance.
   *
   * If numComponents is 1, the same translation table is used for all pixel
   * components.
   * 
   * @param offset Offset to be subtracted.
   * @param numComponents Number of image components.
   * @exception IllegalArgumentException if offset < 0 or numComponents < 1.
   */
  protected LookupTable(int offset, int numComponents)
    throws IllegalArgumentException
  {
    if (offset < 0 || numComponents < 1)
      throw new IllegalArgumentException();
    this.offset = offset;
    this.numComponents = numComponents;
  }

  /** Return the number of components. */
  public int getNumComponents()
  {
    return numComponents;
  }

  /** Return the offset. */
  public int getOffset()
  {
    return offset;
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
  public abstract int[] lookupPixel(int[] src, int[] dest);
}
