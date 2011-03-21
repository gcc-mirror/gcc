/* PixelInterleavedSampleModel.java
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
 * A <code>SampleModel</code> that uses exactly one element of the
 * raster&#x2019;s {@link DataBuffer} per pixel, holds all bands in a
 * single bank, and stores band data in pixel-interleaved manner.
 *
 * @since 1.2
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class PixelInterleavedSampleModel
  extends ComponentSampleModel
{
  public PixelInterleavedSampleModel(int dataType, int width, int height,
                                     int pixelStride, int scanlineStride,
                                     int[] bandOffsets)
  {
    super(dataType, width, height, pixelStride, scanlineStride,
          bandOffsets);
  }


  /**
   * Creates a new <code>SampleModel</code> that is like this one, but
   * uses the specified width and height.
   *
   * @param width the number of pixels in the horizontal direction.
   *
   * @param height the number of pixels in the vertical direction.
   */
  public SampleModel createCompatibleSampleModel(int width, int height)
  {
    // Find minimum band offset.
    int minBandoff = bandOffsets[0];
    int numBands = bandOffsets.length;
    for (int i = 1; i < numBands; i++)
      {
        if (bandOffsets[i] < minBandoff)
          {
            minBandoff = bandOffsets[i];
          }
      }
    // Adjust band offsets so that minimum offset is at 0.
    int[] bandOff;
    if (minBandoff > 0)
      {
        bandOff = new int[numBands];
        for (int i = 0; i < numBands; i++)
          {
            bandOff[i] = bandOffsets[i] - minBandoff;
          }
      }
    else
      {
        bandOff = bandOffsets;
      }
    // Adjust scanline stride for new width.
    return new PixelInterleavedSampleModel(dataType, width, height,
                                           pixelStride, pixelStride * width,
                                           bandOff);
  }


  /**
   * Creates a new <code>SampleModel</code> that is like this one, but
   * uses only a subset of its bands.
   *
   * @param bands an array whose elements indicate which bands shall
   * be part of the subset. For example, <code>[0, 2, 3]</code> would
   * create a SampleModel containing bands #0, #2 and #3.
   */
  public SampleModel createSubsetSampleModel(int[] bands)
  {
    int[] subOffsets;

    subOffsets = new int[bands.length];
    for (int i = 0; i < bands.length; i++)
      subOffsets[i] = bandOffsets[bands[i]];

    return new PixelInterleavedSampleModel(dataType, width, height,
                                           pixelStride, scanlineStride,
                                           subOffsets);
  }
}
