/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/** 
 * Structure containing image data that resides on the client side.
 * The format, depth and offset attributes of an XImage determines how
 * bitfields are encoded in a raster image. However, it does not
 * determine how a color is encoded into a bitfield. I.e. the XImage
 * pixel values in a specific structure, but does not determine what
 * colors that will be used to represent these pixel values on the
 * screen.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class XImage
{
  /** This object reference points to the data, hindering garbage
      collection of the data. */
  Object dataRef;

  // Must match definitions in X.h:
  public static final int XYBITMAP_FORMAT = 0,
                          XYPIXMAP_FORMAT = 1,
                          ZPIXMAP_FORMAT  = 2;
  
  // Must match definitions in X.h:
  public static final int LEAST_SIGNIFICANT_B_FIRST_ORDER  = 0,
                          MOST_SIGNIFICANT_B_FIRST_ORDER   = 1;
  
  public XImage(Visual visual, int depth, int format, int xoffset,
		int width, int height, int bitmapPad,
		int bytesPerLine)
  {
    this(visual, depth, format, xoffset, width, height, bitmapPad,
	 bytesPerLine,
	 0 // bitsPerPixel
	 );
  }

  public XImage(Visual visual, int depth, int format, int xoffset,
		int width, int height, int bitmapPad,
		int bytesPerLine, int bitsPerPixel)
  {
    if (visual == null) throw new 
      NullPointerException("a visual must be specified");
	
    init(visual, depth, format, xoffset, width, height,
	 bitmapPad, bytesPerLine, bitsPerPixel);
  }

  public native void init(Visual visual, int depth, int format, int xoffset,
			  int width, int height, int bitmapPad,
			  int bytesPerLine, int bitsPerPixel);
  
  private native void init(Visual visual, int width, int height);

    
  public XImage(Visual visual, int width, int height)
  {
    this(visual, width, height,
	 true // Automatically allocate memory
	 );
  }

  /** 
   * Create a new XImage.
   *
   * @param allocate specifies whether to automatically allocate
   * memory for the image.  It is possible to create the data array
   * elsewhere, so that we can for instance use a DataBufferUShort as
   * data.  Ie. not limit ourself to byte arrays.  This is done by
   * passing false and calling a setData() method manually after
   * creation.
   */
  public XImage(Visual visual, int width, int height, boolean allocate)
  {
    if (visual == null)
      throw new NullPointerException("a visual must be specified");
    
    init(visual, width, height);

    if (allocate)
      {
	/* Now that Xlib has figured out the appropriate bytes per
	   line, we can allocate memory for the image.  */
	// FIXME: What about formats with several layers/bands?
	byte[] data = new byte[getBytesPerLine()*height];

	setData(data, 0);
      }
  }

  /**
   * Attach image data to this XImage.
   *
   * @param offset the index of the first actual data element in the array.
   */
  public void setData(byte[] data, int offset)
  {
    dataRef = data;
    internalSetData(data, offset);
  }

  /**
   * Attach image data to this XImage. 
   *
   * @param offset the index of the first actual data element in the
   *  array.  Note: this is short offset, not a byte offset.
   */
  public void setData(short[] data, int offset)
  {
    dataRef = data;
    internalSetData(data, offset);
  }

  /**
   * Attach image data to this XImage
   * 
   * @param offset the index of the first actual data element in the array.
   * Note: this is not a byte offset.
   */
  public void setData(int[] data, int offset)
  {
    dataRef = data;
    internalSetData(data, offset);
  }
  
  private native void internalSetData(byte[] data, int offset);
  private native void internalSetData(short[] data, int offset);
  private native void internalSetData(int[] data, int offset);
    
  protected native void finalize();

  boolean ownsData = false;
  RawData structure = null;

  public final native int getWidth();
  public final native int getHeight();
  public final native int getDepth();
  public final native int getFormat();

  public final boolean isZPixmapFormat()
  {
    return getFormat() == ZPIXMAP_FORMAT;
  } 


  /** 
   * Get the xoffset. The xoffset avoids the need of shifting the
   * scanlines into place.
   */
  public final native int getXOffset();

  public native final int getBytesPerLine();
  public native final int getBitsPerPixel();

  public native final int getImageByteOrder();
  public native final int getBitmapBitOrder();
  public native final int getBitmapUnit();
  public native final int getBitmapPad();


  // True/Direct Color specific:
  public native int getRedMask();
  public native int getGreenMask();
  public native int getBlueMask();


  /**
   * Set a pixel value at a given position in the image. This method
   * is slow. Don't use it, except as a fall-back.
   */
  public native final void setPixel(int x, int y, int pixel);

  public String toString()
  {
    String format;
    switch(getFormat())
      {
      case ZPIXMAP_FORMAT:
	format = "ZPixmapFormat";
	break;
      default:
	format = "unknown";
      }
    
    String imageByteOrder;
    switch(getImageByteOrder())
      {
      case LEAST_SIGNIFICANT_B_FIRST_ORDER:
	imageByteOrder = "leastSignificantByteFirst";
	break;
      case MOST_SIGNIFICANT_B_FIRST_ORDER:
	imageByteOrder = "mostSignificantByteFirst";
	break;
      default:
	imageByteOrder = "unknwon";
      }
    
    String bitmapBitOrder;
    switch(getBitmapBitOrder())
      {
      case LEAST_SIGNIFICANT_B_FIRST_ORDER:
	bitmapBitOrder = "leastSignificantBitFirst";
	break;
      case MOST_SIGNIFICANT_B_FIRST_ORDER:
	bitmapBitOrder = "mostSignificantBitFirst";
	break;
      default:
	bitmapBitOrder = "unknown";
      }
    
    return getClass().getName() + "[" + format +
      ", width=" + getWidth() +
      ", height=" + getHeight() +
      ", bytesPerLine=" + getBytesPerLine() +
      ", xoffset=" + getXOffset() +
      ", depth=" + getDepth() +
      ", bitsPerPixel=" + getBitsPerPixel() +
      ", bitmapUnit=" + getBitmapUnit() +
      ", bitmapPad=" + getBitmapPad() +
      ", byteOrder=" + imageByteOrder +
      ", bitOrder=" + bitmapBitOrder +
      "]";
  }
}
