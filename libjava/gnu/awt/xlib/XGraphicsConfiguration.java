/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.GraphicsConfiguration;
import java.awt.Rectangle;
import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Color;
import java.awt.color.ColorSpace;
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
import gnu.java.awt.Buffers;
import java.util.Hashtable;

public class XGraphicsConfiguration extends GraphicsConfiguration
{
  //public abstract GraphicsDevice getDevice();
  
  Visual visual;
  int format;
  Colormap colormap;
  ColorModel imageCM;
  ColorModel pixelCM;
  
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
   * data buffer offset, which will also be added.  */
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

  public BufferedImage createCompatibleImage(int width,
					     int height,
					     int transparency)
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
  XFontMetrics getXFontMetrics(java.awt.Font awtFont)
  {
    // FIXME: do caching...
    
    String family       = "*";
    String name         = awtFont.getName();
    String weight       = awtFont.isBold() ? "bold" : "medium";
    String slant        = awtFont.isItalic() ? "i" : "r";
    String addStyle     = "*";
    String pixelSize    = "*";
    String pointSize    = awtFont.getSize() + "0";
    String xres         = "*";
    String yres         = "*";
    String spacing      = "*";
    String averageWidth = "*";
    String charset      = "*";
    
    String logicalFontDescription =
      family    + "-" + name         + "-" + weight    + "-" +
      slant     + "-" + addStyle     + "-" + pixelSize + "-" +
      pointSize + "-" + xres         + "-" + yres      + "-" +
      spacing   + "-" + averageWidth + "-" + charset;
    
    Display display = visual.getScreen().getDisplay();
    gnu.gcj.xlib.Font xfont =
      new gnu.gcj.xlib.Font(display, logicalFontDescription);
    return new XFontMetrics(xfont, awtFont);
  }

  int getPixel(Color color)
  {
    int[] components =
        {
	  color.getRed(),
	  color.getGreen(),
	  color.getBlue(),
	  0xff
	};
	
    ColorModel cm = getColorModel();
    return cm.getDataElement(components, 0);
  }
}
