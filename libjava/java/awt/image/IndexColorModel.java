/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.image;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import gnu.java.awt.Buffers;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class IndexColorModel extends ColorModel
{
  private byte[] r;
  private byte[] g;
  private byte[] b;
  private byte[] a;
  private int[] argb;
  private byte[] cmap;
  private int start;
  private int transparent;
  private int size;
  
  public IndexColorModel(int bits, int size, byte[] r, byte[] g, byte[] b)
  {
    super(bits, nArray(bits, 3),
	  ColorSpace.getInstance(ColorSpace.CS_sRGB),
	  false,  // no transparency
	  false,  // no premultiplied
	  Transparency.OPAQUE,
	  Buffers.smallestAppropriateTransferType(bits));
    this.r = r;
    this.g = g;
    this.b = b;
    this.size = size;
  }

  public IndexColorModel(int bits, int size, byte[] r, byte[] g, byte[] b,
			 int transparent)
  {
    super(bits, nArray(bits, 4), 
	  ColorSpace.getInstance(ColorSpace.CS_sRGB),
	  true,  // has transparency
	  false,
	  Transparency.BITMASK,
	  Buffers.smallestAppropriateTransferType(bits));
    this.r = r;
    this.g = g;
    this.b = b;
    this.transparent = transparent;
    this.size = size;
  }

  public IndexColorModel(int bits, int size, byte[] r, byte[] g, byte[] b,
			 byte[] a)
  {
    super(bits, nArray(bits, 4),
	  ColorSpace.getInstance(ColorSpace.CS_sRGB),
	  true,  // has transparency
	  false,
	  Transparency.BITMASK,
	  Buffers.smallestAppropriateTransferType(bits));
    this.r = r;
    this.g = g;
    this.b = b;
    this.a = a;
    this.size = size;
  }

  public IndexColorModel(int bits, int size, byte[] cmap, int start,
			 boolean hasAlpha)
  {
    super(bits, nArray(bits, hasAlpha ? 4 : 3),
	  ColorSpace.getInstance(ColorSpace.CS_sRGB),
	  hasAlpha,
	  false,
	  hasAlpha ? Transparency.TRANSLUCENT : Transparency.OPAQUE,
	  Buffers.smallestAppropriateTransferType(bits));
    this.cmap = cmap;
    this.start = start;
    this.size = size;
  }

  public IndexColorModel(int bits, int size, byte[] cmap, int start,
			 boolean hasAlpha, int transparent,
			 int transferType)
  {
    super(bits, nArray(bits, hasAlpha ? 4 : 3),
	  ColorSpace.getInstance(ColorSpace.CS_sRGB),
	  hasAlpha,
	  false,
	  hasAlpha ? 
	  Transparency.TRANSLUCENT :
	  ((transparent < 0) ?
	   Transparency.OPAQUE :
	   Transparency.BITMASK),
	  transferType);
    this.cmap = cmap;
    this.start = start;
    this.size = size;
  }

  public final int getMapSize()
  {
    return size;
  }
  
  public final int getTransparentPixel()
  {
    return transparent;
  }

  public final void getReds(byte r[])
  {
    if (this.r == null) calcRGBArrays();
    System.arraycopy(this.r, 0, r, 0, getMapSize());
  }
  
  public final void getGreens(byte g[])
  {
    if (this.g == null) calcRGBArrays();
    System.arraycopy(this.g, 0, g, 0, getMapSize());
  }
  
  public final void getBlues(byte b[])
  {
    if (this.b == null) calcRGBArrays();
    System.arraycopy(this.b, 0, b, 0, getMapSize());
  }

  public final void getAlphas(byte a[])
  {
    if (this.a == null) calcAlphaArray();
    System.arraycopy(this.a, 0, a, 0, getMapSize());
  }

  public final void getRGBs(int rgb[])
  {
    if (this.argb == null) calcARGBArray();
    System.arraycopy(this.argb, 0, rgb, 0, getMapSize());
  }

  public int getRed(int pixel)
  {
    try
      {
	return r[pixel];
      }
    catch (NullPointerException npe)
      {
	calcRGBArrays();
	return r[pixel];
      }
  }

  public int getGreen(int pixel)
  {
    try
      {
	return g[pixel];
      }
    catch (NullPointerException npe)
      {
	calcRGBArrays();
	return g[pixel];
      }
  }

  public int getBlue(int pixel)
  {
    try
      {
	return b[pixel];
      }
    catch (NullPointerException npe)
      {
	calcRGBArrays();
	return b[pixel];
      }
  }
  
  public int getAlpha(int pixel)
  {
    try
      {
	return a[pixel];
      } 
    catch (NullPointerException npe)
      {
	calcAlphaArray();
	return a[pixel];
      }
  }

  private void calcRGBArrays() {
    int j=0;
    boolean hasAlpha = hasAlpha();
    r = new byte[size];
    g = new byte[size];
    b = new byte[size];
    if (hasAlpha) a = new byte[size];
    
    for (int i=0; i<size; i++)
      {
	r[i] = cmap[j++];
	g[i] = cmap[j++];
	b[i] = cmap[j++];
	if (hasAlpha()) a[i] = cmap[j++];
      }
  }

  private void calcAlphaArray()
  {
    int transparency = getTransparency();
    switch (transparency)
      {
      case Transparency.OPAQUE:
      case Transparency.BITMASK:
	a = nArray((byte) 255, size);
	if (transparency == Transparency.BITMASK)
	  a[transparent] = 0;
	break;
      case Transparency.TRANSLUCENT:
	calcRGBArrays();
      }
  }

  private void calcARGBArray()
  {
    int mapSize = getMapSize();
    argb = new int[mapSize];
    for (int p=0; p<mapSize; p++) argb[p] = getRGB(p);
  }
  
  public int getRed(Object inData)
  {
    return getRed(getPixelFromArray(inData));
  }

  public int getGreen(Object inData)
  {
    return getGreen(getPixelFromArray(inData));
  }

  public int getBlue(Object inData)
  {
    return getBlue(getPixelFromArray(inData));
  }
    
  public int getAlpha(Object inData)
  {
    return getAlpha(getPixelFromArray(inData));
  }

  public int getRGB(Object inData)
  {
    return getRGB(getPixelFromArray(inData));
  }

  public Object getDataElements(int rgb, Object pixel)
  {
    int av, rv, gv, bv;
    // using 8 bit values
    av = (rgb >>> 24) & 0xff;
    rv = (rgb >>> 16) & 0xff;
    gv = (rgb >>>  8) & 0xff;
    bv = (rgb >>>  0) & 0xff;
    
    int pixelValue = getPixelValue(av, rv, gv, bv);

    /* In this color model, the whole pixel fits in the first element
       of the array. */
    DataBuffer buffer = Buffers.createBuffer(transferType, pixel, 1);
    buffer.setElem(0, pixelValue);
    return Buffers.getData(buffer);
  }
    
  private int getPixelValue(int av, int rv, int gv, int bv)
  {
    if (r == null) calcRGBArrays();
    if (a == null) calcAlphaArray();
    
    int minDAlpha = 1<<8;
    int minDRGB = (1<<8)*(1<<8)*3;
    int pixelValue = -1;
    for (int i=0; i<size; i++)
      {
	int dAlpha = Math.abs(av-(a[i]&0xff));
	if (dAlpha > minDAlpha) continue;
	int dR = rv-(r[i]&0xff);
	int dG = gv-(g[i]&0xff);
	int dB = bv-(b[i]&0xff);
	int dRGB = dR*dR + dG*dG + dB*dB;
	
	if (dRGB >= minDRGB) continue;
	
	pixelValue = i;
	minDRGB = dRGB;
      }
    return pixelValue;
  }  

  public int[] getComponents(int pixel, int[] components, int offset)
  {
    int numComponents = getNumComponents();
    if (components == null) components = new int[offset + numComponents];
    components[offset++] = (r[pixel]&0xff);
    components[offset++] = (g[pixel]&0xff);
    components[offset++] = (b[pixel]&0xff);
    if (hasAlpha()) components[offset++] = (a[pixel]&0xff);
    return components;
  }
	
  public final int[] getComponents(Object pixel, int[] components,
				   int offset)
  {
    return getComponents(getPixelFromArray(pixel), components, offset);
  }
  
  public int getDataElement(int[] components, int offset)
  {
    int r = components[offset++];
    int g = components[offset++];
    int b = components[offset++];
    int a = hasAlpha() ? components[offset++] : 255;
    
    return getPixelValue(a, r, g, b);
  }
  
  public Object getDataElements(int[] components, int offset, Object pixel)
  {
    int pixelValue = getDataElement(components, offset);
    
    /* In this color model, the whole pixel fits in the first element
       of the array. */
    DataBuffer buffer = Buffers.createBuffer(transferType, pixel, 1);
    buffer.setElem(0, pixelValue);
    return Buffers.getData(buffer);
  }
    
  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    int[] bandOffsets = {0};
    return new ComponentSampleModel(transferType, w, h,
				    1, // pixel stride
				    w, // scanline stride
				    bandOffsets);
  }
}
