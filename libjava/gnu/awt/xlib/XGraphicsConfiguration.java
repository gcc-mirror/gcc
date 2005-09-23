/* Copyright (C) 2000, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.GraphicsConfiguration;
import java.awt.Rectangle;
import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.GraphicsDevice;
import java.awt.Point;
import java.awt.Color;
import java.awt.color.ColorSpace;
import java.awt.Font;
import java.awt.image.*;
import java.awt.geom.AffineTransform;
import gnu.gcj.xlib.GC;
import gnu.gcj.xlib.Drawable;
import gnu.gcj.xlib.Window;
import gnu.gcj.xlib.XImage;
import gnu.gcj.xlib.Visual;
import gnu.gcj.xlib.Colormap;
import gnu.gcj.xlib.XColor;
import gnu.gcj.xlib.Screen;
import gnu.gcj.xlib.Display;
import gnu.gcj.xlib.XException;
import gnu.java.awt.Buffers;
import java.util.Enumeration;
import java.util.Hashtable;

public class XGraphicsConfiguration extends GraphicsConfiguration
{
  //public abstract GraphicsDevice getDevice();
  
  Visual visual;
  int format;
  Colormap colormap;
  ColorModel imageCM;
  ColorModel pixelCM;
  private static final int CACHE_SIZE_PER_DISPLAY = 10;
  static FontMetricsCache fontMetricsCache = new FontMetricsCache ();
  
  /** Font metrics cache class.  Caches at most CACHE_SIZE_PER_DISPLAY
   * XFontMetrics objects for each display device.  When a display's cache
   * gets full, the least-recently used entry is overwritten.
   * XXX: lruOrder rolls over after a few billion operations, so it might
   * on very rare occasions misinterpret which is the oldest entry
   */
  static class FontMetricsCache
  {
    private java.util.Hashtable displays = new java.util.Hashtable ();
    
    /** Font metrics cache for a display device
     */
    class PerDisplayCache
    {
      private int lruCount = 0;
      private java.util.Hashtable entries = new java.util.Hashtable ();
      
      class CacheEntry
      {
        int lruOrder;
        XFontMetrics fm;
        Font font;
      }
      
      /** Get an entry (null if not there) and update LRU ordering
       */
      XFontMetrics get (Font font)
      {
        CacheEntry entry = (CacheEntry)entries.get (font);
        if (entry != null)
        {
          entry.lruOrder = lruCount++;
        }
        return (entry==null) ? null : entry.fm;
      }
      
      /** Put an entry in the cache, eliminating the oldest entry if
       * the cache is at capacity.
       */
      void put (Font font, XFontMetrics fontMetrics)
      {
        if (entries.size () >= CACHE_SIZE_PER_DISPLAY)
        {
          // cache is full -- eliminate the oldest entry
          // slow operation, but shouldn't happen very often
          int maxAge = 0;
          CacheEntry oldestEntry = null;
          int referenceCount = lruCount;
          for (Enumeration e = entries.elements (); e.hasMoreElements ();)
          {
            CacheEntry entry = (CacheEntry)e.nextElement ();
            if ((referenceCount-entry.lruOrder) > maxAge)
            {
              maxAge = referenceCount-entry.lruOrder;
              oldestEntry = entry;
            }
          }
          if (oldestEntry != null)
            entries.remove (oldestEntry.font);
        }
        CacheEntry newEntry = new CacheEntry ();
        newEntry.lruOrder = lruCount++;
        newEntry.fm = fontMetrics;
        newEntry.font = font;
        entries.put (font,newEntry);
      }
    }
    
    /** Get the font metrics for a font, if it is present in the cache.
     * @param font The AWT font for which to find the font metrics
     * @param display The display, to select the cached entries for that display
     * @return The font metrics, or null if not cached
     */
    XFontMetrics get (Font font, Display display)
    {
      PerDisplayCache cache = (PerDisplayCache)displays.get (display);
      return (cache==null) ? null : cache.get (font);
    }
    
    /** Put a font in the cache
     * @param font The font
     * @param display The display
     * @param fontMetrics The font metrics
     */
    void put (Font font, Display display, XFontMetrics fontMetrics)
    {
      PerDisplayCache cache = (PerDisplayCache)displays.get (display);
      if (cache == null)
      {
        cache = new PerDisplayCache ();
        displays.put (display,cache);
      }
      cache.put (font,fontMetrics);
    }
  }
  
  public XGraphicsConfiguration(Visual visual)
  {
    this.visual = visual;
  }

  public BufferedImage createCompatibleImage(int width, int height)
  {
    XImage ximg = new XImage(visual, width, height,
			     false // do not auto allocate memory
			     );

    Point origin = new Point(0, 0);
    WritableRaster raster = createRasterForXImage(ximg, origin);

    /* This is not a good way of doing this. Multiple toolkits may
       want to share the BufferedImage. */
    Hashtable props = new Hashtable();
    props.put("gnu.gcj.xlib.XImage", ximg);
    props.put("java.awt.GraphicsConfiguration", this);
    
    BufferedImage bimg = new BufferedImage(imageCM,raster, false, props);

    DataBuffer dataB = raster.getDataBuffer();
    attachData(ximg, dataB, 0);
    return bimg;
  }

  WritableRaster createRasterForXImage(XImage ximage, Point origin)
  {
    if (imageCM == null) prepareColorModel(ximage);
    
    /*
      This will not work, since it creates a sample model that
      does not necessarily match the format of the XImage.
      
      WritableRaster raster =
      imageCM.createCompatibleWritableRaster(width, height); */
    
    // Create a sample model matching the XImage:

    SampleModel imageSM = null;

    int width = ximage.getWidth();
    int height = ximage.getHeight();
    int bitsPerPixel = ximage.getBitsPerPixel();
    int dataType =
      Buffers.smallestAppropriateTransferType(bitsPerPixel);
    int bitsPerDataElement = DataBuffer.getDataTypeSize(dataType);
    int scanlineStride = ximage.getBytesPerLine()*8/bitsPerDataElement;
    
    if (imageCM instanceof IndexColorModel)
      {
	int[] bandOffsets = {0};
	imageSM = new ComponentSampleModel(dataType,
					   width, height,
					   1, // pixel stride
					   scanlineStride,
					   bandOffsets);
      }
    else if (imageCM instanceof PackedColorModel)
      {
	PackedColorModel pcm = (PackedColorModel) imageCM;
	int[] masks = pcm.getMasks();
	
	imageSM = new SinglePixelPackedSampleModel(dataType,
						   width, height,
						   scanlineStride,
						   masks);
      }

    if (imageSM == null)
      {
	throw new UnsupportedOperationException("creating sample model " +
						"for " + imageCM +
						" not implemented");
      }

    WritableRaster raster = Raster.createWritableRaster(imageSM, origin);
    return raster;
  }



  /**
   * Attach a the memory of a data buffer to an XImage
   * structure. [This method is not gnu.awt.xlib specific, and should
   * maybe be moved to a different location.]
   *
   * @param offset Offset to data. The given offset does not include
   * data buffer offset, which will also be added.  
   */
  static void attachData(XImage ximage, DataBuffer dataB, int offset)
  {
    offset += dataB.getOffset();
    switch (dataB.getDataType())
      {
      case DataBuffer.TYPE_BYTE:
	ximage.setData(((DataBufferByte) dataB).getData(), offset);
	break;
      case DataBuffer.TYPE_USHORT:
	ximage.setData(((DataBufferUShort) dataB).getData(), offset);
	break;
      case DataBuffer.TYPE_INT:
	ximage.setData(((DataBufferInt) dataB).getData(), offset);
	break;
      default:
	throw
	  new UnsupportedOperationException("Do not know how to " +
					    "set data for data " +
					    "type " +
					    dataB.getDataType());
      }
  }
    
  void prepareColorModel(XImage ximage)
  {
    format = ximage.getFormat();
    int bitsPerPixel = ximage.getBitsPerPixel();
    switch (format) {
    case XImage.ZPIXMAP_FORMAT:
      calcZPixmapModels(bitsPerPixel);
      break;
      
    default:
      throw new UnsupportedOperationException("unimplemented format");
    }
  }

  void calcZPixmapModels(int bitsPerPixel)
  {
    switch (visual.getVisualClass())
      {
      case Visual.VC_TRUE_COLOR:
	calcDecomposedRGBModels(bitsPerPixel);
	break;
      case Visual.VC_PSEUDO_COLOR:
	calcPseudoColorModels(bitsPerPixel);
	break;
      default:
	String msg = "unimplemented visual class";
	throw new UnsupportedOperationException(msg);
      }
  }
    
  void calcDecomposedRGBModels(int bitsPerPixel)
  {
    int dataType = Buffers.smallestAppropriateTransferType(bitsPerPixel);
    
    
    if (DataBuffer.getDataTypeSize(dataType) == bitsPerPixel)
      {	
	ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_sRGB);
	
	imageCM = new DirectColorModel(cs,
				       visual.getDepth(),
				       visual.getRedMask(),
				       visual.getGreenMask(),
				       visual.getBlueMask(),
				       0, // no alpha
				       false,
				       dataType);
      }
    else
      {
	throw new
	  UnsupportedOperationException("unimplemented bits per pixel");
      }
    }
    
  void calcPseudoColorModels(int bitsPerPixel)
  {
    if (colormap == null)
      colormap = visual.getScreen().getDefaultColormap();
    
    XColor[] colArray = colormap.getXColors();
	
    int numCol = colArray.length;
    byte[] rmap = new byte[numCol];
    byte[] gmap = new byte[numCol];
    byte[] bmap = new byte[numCol];
    byte[] amap = new byte[numCol];
    
    for (int i=0; i < numCol; i++)
      {
	XColor color = colArray[i];
	if (color.getFlags() == Colormap.FLAG_SHARED)
	  {
	    rmap[i] = (byte) (color.getRed()   >> 8);
	    gmap[i] = (byte) (color.getGreen() >> 8);
	    bmap[i] = (byte) (color.getBlue()  >> 8);
	    amap[i] = (byte) 0xff;
	  } // else, leave default zero values...
      }

    imageCM = new IndexColorModel(visual.getDepth(), numCol,
				  rmap, gmap, bmap, amap);
  }

  /**
   * Gets the associated device that this configuration describes.
   *
   * @return the device
   */
  public GraphicsDevice getDevice()
  {
    throw new UnsupportedOperationException("not implemented");  
  }

  /**
   * Returns a buffered image optimized to this device, so that blitting can
   * be supported in the buffered image.
   *
   * @param w the width of the buffer
   * @param h the height of the buffer
   * @return the buffered image, or null if none is supported
   */
  public BufferedImage createCompatibleImage(int width,
					     int height,
					     int transparency)
  {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * Returns a buffered volatile image optimized to this device, so that
   * blitting can be supported in the buffered image. Because the buffer is
   * volatile, it can be optimized by native graphics accelerators.
   *
   * @param w the width of the buffer
   * @param h the height of the buffer
   * @return the buffered image, or null if none is supported
   * @see Component#createVolatileImage(int, int)
   * @since 1.4
   */
  public VolatileImage createCompatibleVolatileImage(int w, int h)
  {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * FIXME: I'm not sure which color model that should be returned here.
   */
  public ColorModel getColorModel()
  {
    if (pixelCM == null)
      preparePixelCM();
    return pixelCM;
  }

  void preparePixelCM()
  {
    switch (visual.getVisualClass())
      {
      case Visual.VC_TRUE_COLOR:
	pixelCM = new DirectColorModel(visual.getDepth(),
				       visual.getRedMask(),
				       visual.getGreenMask(),
				       visual.getBlueMask());
	break;
      case Visual.VC_PSEUDO_COLOR:

	if (colormap == null)
	  colormap = visual.getScreen().getDefaultColormap();
	
	XColor[] colArray = colormap.getXColors();
	
	int numCol = colArray.length;
	byte[] rmap = new byte[numCol];
	byte[] gmap = new byte[numCol];
	byte[] bmap = new byte[numCol];
	byte[] amap = new byte[numCol];
	
	for (int i=0; i < numCol; i++)
	  {
	    XColor color = colArray[i];
	    if (color.getFlags() == Colormap.FLAG_SHARED) {
	      rmap[i] = (byte) (color.getRed()   >> 8);
	      gmap[i] = (byte) (color.getGreen() >> 8);
	      bmap[i] = (byte) (color.getBlue()  >> 8);
	      amap[i] = (byte) 0xff;
	    } // else, leave default zero values...

	  }

	pixelCM = new IndexColorModel(visual.getDepth(), numCol,
				      rmap, gmap, bmap, amap);
	break;
      default:
	throw new UnsupportedOperationException("not implemented");
      }
  }
  
  public ColorModel getColorModel(int transparency)
  {
    throw new UnsupportedOperationException("not implemented");
  }
    
  public AffineTransform getDefaultTransform()
  {
    throw new UnsupportedOperationException("not implemented");
  }

  public AffineTransform getNormalizingTransform()
  {
    throw new UnsupportedOperationException("not implemented");
  }
 
  public Rectangle getBounds()
  {
    throw new UnsupportedOperationException("not implemented");
  }

  Visual getVisual()
  {
    return visual;
  }
    
  /* FIXME: This should be moved to XGraphicsDevice... */
  XFontMetrics getXFontMetrics (java.awt.Font awtFont)
  {
    // If the metrics object for this font is already cached, use it.
    // Otherwise create and cache it.
    Display display = visual.getScreen ().getDisplay ();
    XFontMetrics fm = fontMetricsCache.get (awtFont,display);
    if (fm == null)
    {
      String foundry      = "*";
      String family       = awtFont.getName ();
      String weight       = awtFont.isBold () ? "bold" : "medium";
      String slant        = awtFont.isItalic () ? "i" : "r";
      String sWidth       = "*";
      String addStyle     = "";
      String pixelSize    = "*";
      String pointSize    = awtFont.getSize () + "0";
      String xres         = "*";
      String yres         = "*";
      String spacing      = "*";
      String averageWidth = "*";
      String charset      = "iso10646-1"; // because we use functions like XDrawString16
      
      String logicalFontDescription =
        "-" + // FontNameRegistry prefix
        foundry   + "-" + family    + "-" + weight       + "-" +
        slant     + "-" + sWidth    + "-" + addStyle     + "-" +
        pixelSize + "-" + pointSize + "-" + xres         + "-" +
        yres      + "-" + spacing   + "-" + averageWidth + "-";
      
      // Try to load a Unicode font.  If that doesn't work, try again, without
      // specifying the character set.
      try
      {
        gnu.gcj.xlib.Font xfont = new gnu.gcj.xlib.Font (display, logicalFontDescription + charset);
        fm = new XFontMetrics (xfont, awtFont);
      }
      catch (XException e)
      {
        gnu.gcj.xlib.Font xfont = new gnu.gcj.xlib.Font (display, logicalFontDescription + "*-*");
        fm = new XFontMetrics (xfont, awtFont);
      }
      fontMetricsCache.put (awtFont,display,fm);
    }
    return fm;
  }

  int getPixel(Color color)
  {
    /* FIXME: consider an integer technique whenever
     * the ColorModel is 8 bits per color.
     * The problem with using integers is that it doesn't work unless
     * the colors are 8 bits each (as in the array), since ColorModel.getDataElement(int[],int)
     * expects non-normalized values.  For example, in a 16-bit display mode, you
     * would typically have 5 bits each for red and blue, and 6 bits for green.
    int[] components =
    {
      color.getRed (),
      color.getGreen (),
      color.getBlue (),
      0xff
    };
     */
    
    int[] unnormalizedComponents = { 0, 0, 0, 0xff };
    ColorModel cm = getColorModel ();
    if (color != null)
    {
      float[] normalizedComponents =
      {
        ((float)color.getRed ()) / 255F,
        ((float)color.getGreen ()) / 255F,
        ((float)color.getBlue ()) / 255F,
        1
      };
      cm.getUnnormalizedComponents(normalizedComponents, 0,
           unnormalizedComponents, 0);
    }
    return cm.getDataElement (unnormalizedComponents, 0);
  }

  /**
   * @since 1.5
   */
  public VolatileImage createCompatibleVolatileImage (int width, int height,
                                                      int transparency)
  {
    return null;
  }
}
