/* Copyright (C) 2000, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.*;
import java.awt.image.WritableRaster;
import java.awt.image.Raster;
import java.awt.image.DataBuffer;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.BufferedImage;
import gnu.gcj.xlib.GC;
import gnu.gcj.xlib.XImage;
import gnu.gcj.xlib.Drawable;
import gnu.gcj.xlib.Window;
import gnu.gcj.xlib.Drawable;
import gnu.gcj.xlib.Pixmap;
import gnu.gcj.xlib.Visual;
import gnu.awt.j2d.DirectRasterGraphics;
import gnu.awt.j2d.MappedRaster;

public class XGraphics implements Cloneable, DirectRasterGraphics
{
  static class XRaster extends MappedRaster
  {
    XImage ximage;
    
    public XRaster(WritableRaster raster, XImage ximage, ColorModel cm)
    {
      super(raster, cm);
      this.ximage = ximage;
    }
  }
  
  GC context;
  XGraphicsConfiguration config;
  Rectangle clipBounds;
    
  XFontMetrics metrics;


  public Object clone()
  {
    try
      {
	XGraphics gfxCopy = (XGraphics) super.clone();
	gfxCopy.context = context.create();
	
	return gfxCopy;
      }
    catch (CloneNotSupportedException ex)
      {
	// This should never happen.
	throw new InternalError ();
      }
  }

  public void dispose()
  {
    GC lContext = context;
    context = null;
    config = null;
    clipBounds = null;
    metrics = null;
    
    if (lContext != null)
    {
      lContext.dispose();
    }	    
  }

  public XGraphics(Drawable drawable, XGraphicsConfiguration config)
  {
    context = GC.create(drawable);
    this.config = config;
  }  
  
  public void setColor(Color color)
  {
    context.setForeground(config.getPixel(color));
  }

  public void setPaintMode()
  {
    throw new UnsupportedOperationException("not implemented");
  }

  public void setXORMode(Color c1)
  {
    throw new UnsupportedOperationException("not implemented");
  }
    
  public void setFont(Font font)
  {
    if ((metrics != null) && font.equals(metrics.getFont())) return;

    metrics = config.getXFontMetrics(font);
    context.setFont(metrics.xfont);
  }
    
  public FontMetrics getFontMetrics(Font font)
  {
    if ((metrics != null) && font.equals(metrics.getFont()))
      return metrics;
    
    return config.getXFontMetrics(font);
  }
    
  public void setClip(int x, int y, int width, int height)
  {
    Rectangle[] rects = { new Rectangle(x, y, width, height) };
    context.setClipRectangles(rects);
  }
    
  public void setClip(Shape clip)
  {
    /* TODO: create a special RectangleUnion shape that can be
       used to draw advantage of the GCs ability to set multiple
       rectangles. 
    */

    /* FIXME: creating all these objects is wasteful and can be
       costly in the long run, since this code is run at every
       expose. */
    Rectangle newClipBounds = clip.getBounds();
    
    if ((clipBounds != null) && !clipBounds.contains(newClipBounds))
      {
	System.err.println("warning: old clip ("+ clipBounds +") does " +
			   "not fully contain new clip (" +
			   newClipBounds + ")");
      }
    clipBounds = newClipBounds;
    Rectangle[] rects = { clipBounds };
    context.setClipRectangles(rects);
  }
    
  public void copyArea(int x, int y, int width, int height, int
		       dx, int dy)
  {
    throw new UnsupportedOperationException("not implemented");
  }
    
  public void drawLine(int x1, int y1, int x2, int y2)
  {
    context.drawLine(x1, y1, x2, y2);
  }
    
  public void drawRect(int x, int y, int width, int height)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
    
  public void fillRect(int x, int y, int width, int height)
  {
    context.fillRectangle(x, y, width, height);
  }
    
  public void drawArc(int x, int y, int width, int height, int
		      startAngle, int arcAngle)
  {
    context.drawArc (x, y, width, height, startAngle, arcAngle);
  }
    
  public void fillArc(int x, int y, int width, int height, int
		      startAngle, int arcAngle)
  {
    context.fillArc (x, y, width, height, startAngle, arcAngle);
  }
    
  public void drawPolyline(int[] xPoints, int[] yPoints, int
			   nPoints)
  {
    throw new UnsupportedOperationException("not implemented");
  }
    
  public void drawPolygon(int[] xPoints, int[] yPoints, int
			  nPoints)
  {
    throw new UnsupportedOperationException("not implemented");
  }
    
  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints,
			  int translateX, int translateY)
  {
    context.fillPolygon(xPoints, yPoints, nPoints, translateX, translateY);
  }

  public void drawString(String str, int x, int y)
  {
    context.drawString(str, x, y);
  }

  public boolean drawImage(Image img, int x, int y,
			   ImageObserver observer)
  {
    if (img instanceof XOffScreenImage)
    {
      // FIXME: have to enforce clip, or is it OK as-is?
      XGraphicsConfiguration.XOffScreenImage offScreenImage
        = ((XGraphicsConfiguration.XOffScreenImage)img);
      Pixmap pixmap = offScreenImage.getPixmap ();
      context.copyArea (pixmap, 0, 0, x, y,
        offScreenImage.getWidth (), offScreenImage.getHeight ());
      return true;
    }
    if (clipBounds == null)
      return false; // ***FIXME***

    if (!(img instanceof BufferedImage))
      {
	throw new AWTError("unknown image class");
      }
	
    BufferedImage bimg = (BufferedImage) img;

    XImage ximg = (XImage) bimg.getProperty("gnu.gcj.xlib.XImage");
    if (ximg == null)
      {
	System.err.println("FIXME: skipping null XImage, should " +
			   "really do on the spot conversion");
	return false;
      }

    /*
      +------------------
      |    clip
      |     +---------+
      | img |         |
      |  +--+-------+ |
      |  |  |       | | 
      |  |  |       | |
      |  |  +-------+-+
      |  |          |
      |  +----------+
    */

    int iLeft   = Math.max(x, clipBounds.x);
    int iTop    = Math.max(y, clipBounds.y);
    int iRight  = Math.min(x + bimg.getWidth(),
			   clipBounds.x + clipBounds.width);
    int iBottom = Math.min(y + bimg.getHeight(),
			   clipBounds.y + clipBounds.height);
    
    int srcX = iLeft - x;
    int srcY = iTop  - y;
    
    int width  = iRight  - iLeft;
    int height = iBottom - iTop;
    
    if ((width > 0) && (height > 0))
      context.putImage(ximg, srcX, srcY, iLeft, iTop, width, height);

    return true;
  }

  public MappedRaster mapRaster(Rectangle bounds)
  {
    Visual visual = config.getVisual();
    XImage ximage = new XImage(visual, bounds.width, bounds.height,
			       false // do not auto allocate memory
			       );

    WritableRaster raster =
      config.createRasterForXImage(ximage,
				   new Point(bounds.x, bounds.y));
    
    DataBuffer dataB = raster.getDataBuffer();
    XGraphicsConfiguration.attachData(ximage, dataB, 0);

    Drawable drawable = context.getDrawable();

    // TODO: restrict to clipping

    Rectangle mBounds = drawable.copyIntoXImage(ximage, bounds, 0, 0);
	
    return new XRaster(raster, ximage, config.imageCM);
  }
    
    
  public void unmapRaster(MappedRaster mappedRaster)
  {
    XRaster xraster = (XRaster) mappedRaster;
    XImage ximage = xraster.ximage;
    Raster raster = xraster.getRaster();
    int x = raster.getMinX();
    int y = raster.getMinY();
    int width = raster.getWidth();
    int height = raster.getHeight();
    
    context.putImage(ximage, 0, 0, x, y, width, height);
  }
}
