/* Copyright (C) 2000, 2003  Free Software Foundation
 
   This file is part of libgcj.
 
This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.Image;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.ImageConsumer;
import java.util.Hashtable;
import gnu.awt.j2d.DirectRasterGraphics;
import gnu.awt.j2d.Graphics2DImpl;
import gnu.awt.j2d.IntegerGraphicsState;
import gnu.gcj.xlib.Drawable;
import gnu.gcj.xlib.Pixmap;
import gnu.gcj.xlib.Screen;
import gnu.gcj.xlib.Visual;
import gnu.gcj.xlib.GC;

/** Image class for xlib off-screen buffers.
 * The image is stored in a server-side pixmap for best performance.
 * This class supports getGraphics, so you can draw on the pixmap, and is
 * specially handled when doing drawImage, so that the image copy is done
 * entirely in the X server.
 * This class does not support rasterization, for which you'd need an XImage.
 *
 * @author  scott gilbertson <scottg@mantatest.com> <sgilbertson@cogeco.ca>
 */
public class XOffScreenImage extends Image 
                             implements IntegerGraphicsState.ScreenCoupledImage,
                             ImageConsumer
{
  private Pixmap pixmap;
  private XGraphicsConfiguration config;
  private int width;
  private int height;
  private Drawable drawable;
  private ImageProducer prod;
  private GC gc;
  private ColorModel pixmapColorModel;
  
  /** Create a new XOffScreenImage
   * @param config Graphics configuration, to compare against on-screen 
   *               components and to create the appropriate Graphics
   * @param drawable The drawable with which the image is compatible
   * @param width The width of the image
   * @param height The height of the image
   * @param cm The ColorModel associated with drawable
   */
  XOffScreenImage (XGraphicsConfiguration config, Drawable drawable, int width, int height, ColorModel cm)
  {
    this.config = config;
    this.width = width;
    this.height = height;
    this.drawable = drawable;
    pixmapColorModel = cm;
    pixmap = new Pixmap (drawable, width, height, drawable.getDepth ());
    gc = GC.create (pixmap);
  }
  
  /** Create a new XOffScreenImage and obtain image data from an ImageProducer
   * @param config Graphics configuration, to compare against on-screen 
   *               components and to create the appropriate Graphics
   * @param drawable The drawable with which the image is compatible
   * @param prod The source of image data for this image
   * @param cm The ColorModel associated with drawable
   */
  XOffScreenImage (XGraphicsConfiguration config, Drawable drawable, ImageProducer prod, ColorModel cm)
  {
    this.config = config;
    this.width = 0;  // size will be overridden in a moment
    this.height = 0;
    this.drawable = drawable;
    this.prod = prod;
    pixmapColorModel = cm;
    prod.startProduction (this);
  }
  
  /** Get the pixmap which contains this image
   * @return The pixmap
   */
  public Pixmap getPixmap ()
  {
    return pixmap;
  }
  
  /** Flushes (that is, destroys) any resources used for this image.  This
   * includes the actual image data.
   */
  public void flush ()
  {
    // FIXME: should dispose pixmap
    pixmap = null;
  }
  
  /** Returns a graphics context object for drawing an off-screen object.
   * This method is only valid for off-screen objects.
   *
   * @return a graphics context object for an off-screen object
   * @see Graphics#createImage(int, int)
   */
  public Graphics getGraphics ()
  {
    DirectRasterGraphics gfxDevice = new XGraphics (pixmap, config);
    IntegerGraphicsState igState = new IntegerGraphicsState (gfxDevice);
    Graphics2DImpl gfx2d = new Graphics2DImpl (config);
    gfx2d.setState (igState);
    return gfx2d;
  }
  
  /** Returns the height of the image, or -1 if it is unknown.  If the
   * image height is unknown, the observer object will be notified when
   * the value is known.
   *
   * @param observer the image observer for this object
   * @return the height in pixels
   * @see #getWidth(ImageObserver)
   */
  public int getHeight (ImageObserver observer)
  {
    return height;
  }
  
  /** Returns the height of the image, or -1 if it is unknown.  If the
   * image height is unknown, the observer object will be notified when
   * the value is known.
   *
   * @return the height in pixels
   * @see #getWidth()
   */
  public int getHeight ()
  {
    return height;
  }
  
  /** Returns the image producer object for this object. The producer is the
   * object which generates pixels for this image.
   *
   * @return the image producer for this object
   */
  public ImageProducer getSource ()
  {
    if (prod == null)
      throw new UnsupportedOperationException ("getSource not supported");
    else
      return prod;
  }
  
  /** Returns the width of the image, or -1 if it is unknown.  If the
   * image width is unknown, the observer object will be notified when
   * the value is known.
   *
   * @param observer the image observer for this object
   * @return the width in pixels
   * @see #getHeight(ImageObserver)
   */
  public int getWidth (ImageObserver observer)
  {
    return width;
  }
  
  /** Returns the width of the image, or -1 if it is unknown.  If the
   * image width is unknown, the observer object will be notified when
   * the value is known.
   *
   * @return the width in pixels
   * @see #getHeight()
   */
  public int getWidth ()
  {
    return width;
  }

  /** This method requests a named property for an object.  The value of the
   * property is returned. The value <code>UndefinedProperty</code> is
   * returned if there is no property with the specified name.  The value
   * <code>null</code> is returned if the properties for the object are
   * not yet known.  In this case, the specified image observer is notified
   * when the properties are known.
   *
   * @param name the requested property name
   * @param observer the image observer for this object
   * @return the named property, if available
   * @see #UndefinedProperty
   */
  public Object getProperty (String name, ImageObserver observer)
  {
    return null;
  }
  
  /** Get the GraphicsConfiguration to which this image is coupled
   * @return the GraphicsConfiguration
   */
  public GraphicsConfiguration getGraphicsConfiguration ()
  {
    return config;
  }
  
  public void imageComplete (int status)
  {
  }
  
  public void setColorModel (ColorModel model)
  {
  }
  
  public void setDimensions (int width, int height)
  {
    this.width = width;
    this.height = height;
    pixmap = new Pixmap (drawable, width, height, drawable.getDepth ());
    gc = GC.create (pixmap);
  }
  
  public void setHints (int flags)
  {
  }
  
  public void setPixels (int x, int y, int w, int h, ColorModel model, int[] pixels, int offset, int scansize)
  {
    int idx = 0;
    float[] normalizedComponents = new float [4];
    int[] unnormalizedComponents = { 0, 0, 0, 0xff };
    normalizedComponents[3] = 1;
    for (int yp=y; yp < (y + h); yp++)
    {
      for (int xp=x; xp < (x + w); xp++)
      {
        int p = (yp - y) * scansize + (xp - x) + offset;
        // FIXME: there HAS to be a more efficient mechanism for color mapping
        normalizedComponents[0] = (float)model.getRed (pixels[p]) / 255F;
        normalizedComponents[1] = (float)model.getGreen (pixels[p]) / 255F;
        normalizedComponents[2] = (float)model.getBlue (pixels[p]) / 255F;
        pixmapColorModel.getUnnormalizedComponents (normalizedComponents, 0,
          unnormalizedComponents, 0);
        int pixelColor = pixmapColorModel.getDataElement (unnormalizedComponents, 0);
        gc.setForeground (pixelColor);
        gc.drawPoint (xp, yp);
      }
    }
  }
  
  public void setPixels (int x, int y, int w, int h, ColorModel model, byte[] pixels, int offset, int scansize)
  {
    int idx = 0;
    float[] normalizedComponents = new float [4];
    int[] unnormalizedComponents = { 0, 0, 0, 0xff };
    normalizedComponents[3] = 1;
    for (int yp=y; yp < (y + h); yp++)
    {
      for (int xp=x; xp < (x + w); xp++)
      {
        // FIXME: there HAS to be a more efficient mechanism for color mapping
        int p = (yp - y) * scansize + (xp - x) + offset;
        normalizedComponents[0] = (float)model.getRed (pixels[p]) / 255F;
        normalizedComponents[1] = (float)model.getGreen (pixels[p]) / 255F;
        normalizedComponents[2] = (float)model.getBlue (pixels[p]) / 255F;
        pixmapColorModel.getUnnormalizedComponents (normalizedComponents, 0,
          unnormalizedComponents, 0);
        int pixelColor = pixmapColorModel.getDataElement (unnormalizedComponents, 0);
        gc.setForeground (pixelColor);
        gc.drawPoint (xp, yp);
      }
    }
  }
  
  public void setProperties (Hashtable props)
  {
  }
  
}
