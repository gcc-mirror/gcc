/* Copyright (C) 2000, 2002, 2004  Free Software Foundation

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

import gnu.java.awt.BitMaskExtent;

import java.awt.Point;
import java.awt.color.ColorSpace;

/**
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
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
