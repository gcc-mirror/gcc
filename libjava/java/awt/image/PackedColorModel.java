/* Copyright (C) 2000, 2002  Free Software Foundation

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.awt.image;

import java.awt.Point;
import java.awt.color.ColorSpace;
import gnu.java.awt.BitMaskExtent;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public abstract class PackedColorModel extends ColorModel
{
  private int masks[];
  
  /* Package accessibility, the DirectColorModel needs this array */
  int shifts[];

  public PackedColorModel(ColorSpace cspace, int pixelBits,
			  int[] colorMaskArray, int alphaMask,
			  boolean isAlphaPremultiplied,
			  int transparency,
			  int transferType)
  {
    super(pixelBits, calcBitsPerComponent(colorMaskArray, alphaMask),
	  cspace, (alphaMask != 0), isAlphaPremultiplied, transparency,
	  transferType);
    initMasks(colorMaskArray, alphaMask);
    if ((pixelBits<1) || (pixelBits>32)) {
      throw new IllegalArgumentException("pixels per bits must be " +
					 "in the range [1, 32]");
    }
  }
    
  private static int[] calcBitsPerComponent(int[] colorMaskArray,
					    int alphaMask)
  {
    int numComponents = colorMaskArray.length;
    if (alphaMask != 0) numComponents++;
    
    int[] bitsPerComponent = new int[numComponents];
    
    BitMaskExtent extent = new BitMaskExtent();
    for (int b=0; b<colorMaskArray.length; b++)
      {
	extent.setMask(colorMaskArray[b]);
	bitsPerComponent[b] = extent.bitWidth;
      }
    if (alphaMask != 0)
      {
	extent.setMask(alphaMask);
	bitsPerComponent[numComponents-1] = extent.bitWidth;
      }
    return bitsPerComponent;
  }

  /** Initializes the masks.
   *
   * @return an array containing the number of bits per color
   * component.
   */
  private void initMasks(int[] colorMaskArray, int alphaMask)
  {
    int numComponents = colorMaskArray.length;
    if (alphaMask == 0)
      {
	masks = colorMaskArray;
      }
    else
      {
	masks = new int[numComponents+1];
	System.arraycopy(colorMaskArray, 0,
			 masks, 0,
			 numComponents);
	masks[numComponents++] = alphaMask;
      }
	
    shifts = new int[numComponents];
	
    // Bit field handling have been moved to a utility class
    BitMaskExtent extent = new BitMaskExtent();
    for (int b=0; b<numComponents; b++)
      {
	extent.setMask(masks[b]);
	shifts[b] = extent.leastSignificantBit;
      }
  }
    
  public PackedColorModel(ColorSpace cspace, int pixelBits,
			  int rmask, int gmask, int bmask,
			  int amask, boolean isAlphaPremultiplied,
			  int transparency,
			  int transferType)
  {
    this(cspace, pixelBits, makeColorMaskArray(rmask, gmask, bmask),
	 amask, isAlphaPremultiplied, transparency, transferType);
  }
    
  /* TODO: If there is a alpha mask, it is inefficient to create a
     color mask array that will be discarded when the alpha mask is
     appended. We should probably create a private constructor that
     takes a complete array of masks (color+alpha) as an
     argument. */

  private static int[] makeColorMaskArray(int rmask, int gmask, int bmask)
  {
    int[] colorMaskArray = { rmask, gmask, bmask };
    return colorMaskArray;
  }   

  public final int getMask(int index)
  {
    return masks[index];
  }
  
  public final int[] getMasks()
  {
    return masks;
  }

  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    return new SinglePixelPackedSampleModel(transferType, w, h, masks);
  }
    
  public boolean isCompatibleSampleModel(SampleModel sm)
  {
    if (!super.isCompatibleSampleModel(sm)) return false;
    if (!(sm instanceof SinglePixelPackedSampleModel)) return false;
    
    SinglePixelPackedSampleModel sppsm =
      (SinglePixelPackedSampleModel) sm;
    return java.util.Arrays.equals(sppsm.getBitMasks(), masks);
  }

  public WritableRaster getAlphaRaster(WritableRaster raster) {
    if (!hasAlpha()) return null;
	
    SampleModel sm = raster.getSampleModel();
    int[] alphaBand = { sm.getNumBands() - 1 };
    SampleModel alphaModel = sm.createSubsetSampleModel(alphaBand);
    DataBuffer buffer = raster.getDataBuffer();
    Point origin = new Point(0, 0);
    return Raster.createWritableRaster(alphaModel, buffer, origin);
  }
    
  public boolean equals(Object obj)
  {
    if (!super.equals(obj)) return false;
    if (!(obj instanceof PackedColorModel)) return false;
    
    PackedColorModel other = (PackedColorModel) obj;
    
    return java.util.Arrays.equals(masks, other.masks);
  }
}
