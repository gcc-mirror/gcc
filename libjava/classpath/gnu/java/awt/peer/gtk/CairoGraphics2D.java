/* CairoGraphics2D.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import gnu.classpath.Configuration;

import gnu.java.awt.ClasspathToolkit;

import java.awt.AWTPermission;
import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.CompositeContext;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Paint;
import java.awt.PaintContext;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.TexturePaint;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.awt.image.DirectColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.ImagingOpException;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.awt.image.renderable.RenderContext;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.HashMap;
import java.util.Map;

/**
 * This is an abstract implementation of Graphics2D on Cairo. 
 *
 * It should be subclassed for different Cairo contexts.
 *
 * Note for subclassers: Apart from the constructor (see comments below),
 * The following abstract methods must be implemented:
 *
 * Graphics create()
 * GraphicsConfiguration getDeviceConfiguration()
 * copyArea(int x, int y, int width, int height, int dx, int dy)
 *
 * Also, dispose() must be overloaded to free any native datastructures 
 * used by subclass and in addition call super.dispose() to free the
 * native cairographics2d structure and cairo_t.
 *
 * @author Sven de Marothy
 */
public abstract class CairoGraphics2D extends Graphics2D
{
  static 
  {
    if (true) // GCJ LOCAL
      {
        System.loadLibrary("gtkpeer");
      }
  }

  /**
   * Important: This is a pointer to the native cairographics2d structure
   *
   * DO NOT CHANGE WITHOUT CHANGING NATIVE CODE.
   */
  long nativePointer;

  // Drawing state variables
  /**
   * The current paint
   */
  Paint paint;
  boolean customPaint;

  /**
   * The current stroke
   */
  Stroke stroke;

  /*
   * Current foreground and background color.
   */
  Color fg, bg;

  /**
   * Current clip shape.
   */
  Shape clip;

  /**
   * Current transform.
   */
  AffineTransform transform;

  /**
   * Current font.
   */
  Font font;

  /**
   * The current compositing context, if any.
   */
  Composite comp;
  CompositeContext compCtx;

  /**
   * Rendering hint map.
   */
  private RenderingHints hints;
  
  /**
   * Status of the anti-alias flag in cairo.
   */
  private boolean antialias = false;
  private boolean ignoreAA = false;

  /**
   * Some operations (drawing rather than filling) require that their
   * coords be shifted to land on 0.5-pixel boundaries, in order to land on
   * "middle of pixel" coordinates and light up complete pixels. 
   */
  protected boolean shiftDrawCalls = false;

  /**
   * Keep track if the first clip to be set, which is restored on setClip(null);
   */
  private boolean firstClip = true;
  private Shape originalClip;

  /**
   * Stroke used for 3DRects
   */
  private static BasicStroke draw3DRectStroke = new BasicStroke();

  static ColorModel rgb32 = new DirectColorModel(32, 0xFF0000, 0xFF00, 0xFF);
  static ColorModel argb32 = new DirectColorModel(32, 0xFF0000, 0xFF00, 0xFF, 
						  0xFF000000);

  /**
   * Native constants for interpolation methods.
   * Note, this corresponds to an enum in native/jni/gtk-peer/cairographics2d.h
   */
  public static final int INTERPOLATION_NEAREST         = 0,
                          INTERPOLATION_BILINEAR        = 1,
                          INTERPOLATION_BICUBIC         = 5,
                          ALPHA_INTERPOLATION_SPEED     = 2,
                          ALPHA_INTERPOLATION_QUALITY   = 3,
                          ALPHA_INTERPOLATION_DEFAULT   = 4;
  // TODO: Does ALPHA_INTERPOLATION really correspond to CAIRO_FILTER_FAST/BEST/GOOD?
  
  /**
   * Constructor does nothing.
   */
  public CairoGraphics2D()
  {
  }

  /**
   * Sets up the default values and allocates the native cairographics2d structure
   * @param cairo_t_pointer a native pointer to a cairo_t of the context.
   */
  public void setup(long cairo_t_pointer)
  { 
    nativePointer = init(cairo_t_pointer);
    setRenderingHints(new RenderingHints(getDefaultHints()));
    setFont(new Font("SansSerif", Font.PLAIN, 12));
    setColor(Color.black);
    setBackground(Color.white);
    setPaint(Color.black);
    setStroke(new BasicStroke());
    setTransform(new AffineTransform());
    cairoSetAntialias(nativePointer, antialias);
  }

  /**
   * Same as above, but copies the state of another CairoGraphics2D.
   */
  public void copy(CairoGraphics2D g, long cairo_t_pointer)
  {
    nativePointer = init(cairo_t_pointer);
    paint = g.paint;
    stroke = g.stroke;
    setRenderingHints(g.hints);
    
    Color foreground;

    if (g.fg.getAlpha() != -1)
      foreground = new Color(g.fg.getRed(), g.fg.getGreen(), g.fg.getBlue(),
                             g.fg.getAlpha());
    else
      foreground = new Color(g.fg.getRGB());

    if (g.bg != null)
      {
        if (g.bg.getAlpha() != -1)
          bg = new Color(g.bg.getRed(), g.bg.getGreen(), g.bg.getBlue(),
                         g.bg.getAlpha());
        else
          bg = new Color(g.bg.getRGB());
      }

    firstClip = g.firstClip;
    originalClip = g.originalClip;
    clip = g.getClip();

    if (g.transform == null)
      transform = null;
    else
      transform = new AffineTransform(g.transform);

    setFont(g.font);
    setColor(foreground);
    setBackground(bg);
    setPaint(paint);
    setStroke(stroke);
    setTransformImpl(transform);
    setClip(clip);
    setComposite(comp);
    
    antialias = !g.antialias;
    setAntialias(g.antialias);
  }

  /**
   * Generic destructor - call the native dispose() method.
   */
  public void finalize()
  {
    dispose();
  }

  /**
   * Disposes the native cairographics2d structure, including the 
   * cairo_t and any gradient stuff, if allocated. 
   * Subclasses should of course overload and call this if 
   * they have additional native structures.
   */
  public void dispose()
  {
    disposeNative(nativePointer);
    nativePointer = 0;
    if (compCtx != null)
      compCtx.dispose();
  }

  /**
   * Allocate the cairographics2d structure and set the cairo_t pointer in it.
   * @param pointer - a cairo_t pointer, casted to a long.
   */
  protected native long init(long pointer);

  /**
   * These are declared abstract as there may be context-specific issues.
   */
  public abstract Graphics create();

  public abstract GraphicsConfiguration getDeviceConfiguration();

  protected abstract void copyAreaImpl(int x, int y, int width, int height,
                                       int dx, int dy);


  /**
   * Find the bounds of this graphics context, in device space.
   * 
   * @return the bounds in device-space
   */
  protected abstract Rectangle2D getRealBounds();

  ////// Native Methods ////////////////////////////////////////////////////

  /**
   * Dispose of allocate native resouces.
   */
  public native void disposeNative(long pointer);

  /**
   * Draw pixels as an RGBA int matrix
   * @param w - width
   * @param h - height
   * @param stride - stride of the array width
   * @param i2u - affine transform array
   */
  protected native void drawPixels(long pointer, int[] pixels, int w, int h,
                                 int stride, double[] i2u, double alpha,
                                 int interpolation);

  protected native void setGradient(long pointer, double x1, double y1,
                                  double x2, double y2,
                                  int r1, int g1, int b1, int a1, int r2,
                                  int g2, int b2, int a2, boolean cyclic);
  
  protected native void setPaintPixels(long pointer, int[] pixels, int w,
                                     int h, int stride, boolean repeat,
                                     int x, int y);

  /**
   * Set the current transform matrix
   */
  protected native void cairoSetMatrix(long pointer, double[] m);
  
  /**
   * Scaling method
   */
  protected native void cairoScale(long pointer, double x, double y);

  /**
   * Set the compositing operator
   */
  protected native void cairoSetOperator(long pointer, int cairoOperator);

  /**
   * Sets the current color in RGBA as a 0.0-1.0 double
   */
  protected native void cairoSetRGBAColor(long pointer, double red, double green,
                                        double blue, double alpha);

  /**
   * Sets the current winding rule in Cairo
   */
  protected native void cairoSetFillRule(long pointer, int cairoFillRule);

  /**
   * Set the line style, cap, join and miter limit.
   * Cap and join parameters are in the BasicStroke enumerations.
   */
  protected native void cairoSetLine(long pointer, double width, int cap,
                                   int join, double miterLimit);

  /**
   * Set the dash style
   */
  protected native void cairoSetDash(long pointer, double[] dashes, int ndash,
                                   double offset);

  /*
   * Draws a Glyph Vector
   */
  protected native void cairoDrawGlyphVector(long pointer, GdkFontPeer font, 
                                   float x, float y, int n, 
                                   int[] codes, float[] positions, long[] fontset);

  /**
   * Set the font in cairo.
   */
  protected native void cairoSetFont(long pointer, GdkFontPeer font);

  /**
   * Appends a rectangle to the current path
   */
  protected native void cairoRectangle(long pointer, double x, double y,
                                     double width, double height);
  
  /**
   * Appends an arc to the current path
   */
  protected native void cairoArc(long pointer, double x, double y,
                               double radius, double angle1, double angle2);

  /**
   * Save / restore a cairo path
   */
  protected native void cairoSave(long pointer);
  protected native void cairoRestore(long pointer);

  /**
   * New current path
   */
  protected native void cairoNewPath(long pointer);

  /** 
   * Close current path
   */
  protected native void cairoClosePath(long pointer);

  /** moveTo */
  protected native void cairoMoveTo(long pointer, double x, double y);

  /** lineTo */
  protected native void cairoLineTo(long pointer, double x, double y);

  /** Cubic curve-to */
  protected native void cairoCurveTo(long pointer, double x1, double y1,
                                   double x2, double y2,
                                   double x3, double y3);

  /**
   * Stroke current path
   */
  protected native void cairoStroke(long pointer);

  /**
   * Fill current path
   */
  protected native void cairoFill(long pointer, double alpha);

  /** 
   * Clip current path
   */
  protected native void cairoClip(long pointer);

  /** 
   * Clear clip
   */
  protected native void cairoResetClip(long pointer);
  
  /**
   * Set antialias.
   */
  protected native void cairoSetAntialias(long pointer, boolean aa);


  ///////////////////////// TRANSFORMS ///////////////////////////////////
  /**
   * Set the current transform
   */ 
  public void setTransform(AffineTransform tx)
  {
    // Transform clip into target space using the old transform.
    updateClip(transform);

    // Update the native transform.
    setTransformImpl(tx);

    // Transform the clip back into user space using the inverse new transform.
    try
      {
        updateClip(transform.createInverse());
      }
    catch (NoninvertibleTransformException ex)
      {
        // TODO: How can we deal properly with this?
        ex.printStackTrace();
      }

    if (clip != null)
      setClip(clip);
  }

  private void setTransformImpl(AffineTransform tx)
  {
    transform = tx;
    if (transform != null)
      {
        double[] m = new double[6];
        transform.getMatrix(m);
        cairoSetMatrix(nativePointer, m);
      }
  }

  public void transform(AffineTransform tx)
  {
    if (transform == null)
      transform = new AffineTransform(tx);
    else
      transform.concatenate(tx);

    if (clip != null)
      {
        try
          {
            AffineTransform clipTransform = tx.createInverse();
            updateClip(clipTransform);
          }
        catch (NoninvertibleTransformException ex)
          {
            // TODO: How can we deal properly with this?
            ex.printStackTrace();
          }
      }

    setTransformImpl(transform);
  }

  public void rotate(double theta)
  {
    transform(AffineTransform.getRotateInstance(theta));
  }

  public void rotate(double theta, double x, double y)
  {
    transform(AffineTransform.getRotateInstance(theta, x, y));
  }

  public void scale(double sx, double sy)
  {
    transform(AffineTransform.getScaleInstance(sx, sy));
  }

  /**
   * Translate the system of the co-ordinates. As translation is a frequent
   * operation, it is done in an optimised way, unlike scaling and rotating.
   */
  public void translate(double tx, double ty)
  {
    if (transform != null)
      transform.translate(tx, ty);
    else
      transform = AffineTransform.getTranslateInstance(tx, ty);

    if (clip != null)
      {
        // FIXME: this should actuall try to transform the shape
        // rather than degrade to bounds.
        if (clip instanceof Rectangle2D)
          {
            Rectangle2D r = (Rectangle2D) clip;
            r.setRect(r.getX() - tx, r.getY() - ty, r.getWidth(),
                      r.getHeight());
          }
        else
          {
            AffineTransform clipTransform =
              AffineTransform.getTranslateInstance(-tx, -ty);
            updateClip(clipTransform);
          }
      }

    setTransformImpl(transform);
  }
  
  public void translate(int x, int y)
  {
    translate((double) x, (double) y);
  }

  public void shear(double shearX, double shearY)
  {
    transform(AffineTransform.getShearInstance(shearX, shearY));
  }

  ///////////////////////// DRAWING STATE ///////////////////////////////////

  public void clip(Shape s)
  {
    // Do not touch clip when s == null.
    if (s == null)
      {
        // The spec says this should clear the clip. The reference
        // implementation throws a NullPointerException instead. I think,
        // in this case we should conform to the specs, as it shouldn't
        // affect compatibility.
        setClip(null);
        return;
      }

    // If the current clip is still null, initialize it.
    if (clip == null)
      {
        clip = getRealBounds();
      }

    // This is so common, let's optimize this.
    if (clip instanceof Rectangle2D && s instanceof Rectangle2D)
      {
        Rectangle2D clipRect = (Rectangle2D) clip;
        Rectangle2D r = (Rectangle2D) s;
        Rectangle2D.intersect(clipRect, r, clipRect);
        setClip(clipRect);
      }
   else
     {
       Area current;
       if (clip instanceof Area)
         current = (Area) clip;
       else
         current = new Area(clip);

       Area intersect;
       if (s instanceof Area)
         intersect = (Area) s;
       else
         intersect = new Area(s);

       current.intersect(intersect);
       clip = current;
       // Call setClip so that the native side gets notified.
       setClip(clip);
     }
  }

  public Paint getPaint()
  {
    return paint;
  }

  public AffineTransform getTransform()
  {
    return (AffineTransform) transform.clone();
  }

  public void setPaint(Paint p)
  {
    if (p == null)
      return;

    paint = p;
    if (paint instanceof Color)
      {
        setColor((Color) paint);
        customPaint = false;
      }
    
    else if (paint instanceof TexturePaint)
      {
        TexturePaint tp = (TexturePaint) paint;
        BufferedImage img = tp.getImage();

        // map the image to the anchor rectangle  
        int width = (int) tp.getAnchorRect().getWidth();
        int height = (int) tp.getAnchorRect().getHeight();

        double scaleX = width / (double) img.getWidth();
        double scaleY = height / (double) img.getHeight();

        AffineTransform at = new AffineTransform(scaleX, 0, 0, scaleY, 0, 0);
        AffineTransformOp op = new AffineTransformOp(at, getRenderingHints());
        BufferedImage texture = op.filter(img, null);
        int[] pixels = texture.getRGB(0, 0, width, height, null, 0, width);
        setPaintPixels(nativePointer, pixels, width, height, width, true, 0, 0);
        customPaint = false;
      }
    
    else if (paint instanceof GradientPaint)
      {
        GradientPaint gp = (GradientPaint) paint;
        Point2D p1 = gp.getPoint1();
        Point2D p2 = gp.getPoint2();
        Color c1 = gp.getColor1();
        Color c2 = gp.getColor2();
        setGradient(nativePointer, p1.getX(), p1.getY(), p2.getX(), p2.getY(),
                    c1.getRed(), c1.getGreen(), c1.getBlue(), c1.getAlpha(),
                    c2.getRed(), c2.getGreen(), c2.getBlue(), c2.getAlpha(),
                    gp.isCyclic());
        customPaint = false;
      }
    else
      {
        customPaint = true;
      }        
  }
  
  /**
   * Sets a custom paint
   * 
   * @param bounds the bounding box, in user space
   */
  protected void setCustomPaint(Rectangle bounds)
  {
    if (paint instanceof Color || paint instanceof TexturePaint
        || paint instanceof GradientPaint)
      return;
    
    int userX = bounds.x;
    int userY = bounds.y;
    int userWidth = bounds.width;
    int userHeight = bounds.height;
    
    // Find bounds in device space
    Rectangle2D bounds2D = getTransformedBounds(bounds, transform);
    int deviceX = (int)bounds2D.getX();
    int deviceY = (int)bounds2D.getY();
    int deviceWidth = (int)Math.ceil(bounds2D.getWidth());
    int deviceHeight = (int)Math.ceil(bounds2D.getHeight());

    // Get raster of the paint background
    PaintContext pc = paint.createContext(CairoSurface.cairoColorModel,
                                          new Rectangle(deviceX, deviceY,
                                                        deviceWidth,
                                                        deviceHeight),
                                          bounds,
                                          transform, hints);
    
    Raster raster = pc.getRaster(deviceX, deviceY, deviceWidth,
                                 deviceHeight);
    
    // Clear the transform matrix in Cairo, since the raster returned by the
    // PaintContext is already in device-space
    AffineTransform oldTx = new AffineTransform(transform);
    setTransformImpl(new AffineTransform());    

    // Set pixels in cairo, aligning the top-left of the background image
    // to the top-left corner in device space
    if (pc.getColorModel().equals(CairoSurface.cairoColorModel)
        && raster.getSampleModel().getTransferType() == DataBuffer.TYPE_INT)
      {
        // Use a fast copy if the paint context can uses a Cairo-compatible
        // color model
        setPaintPixels(nativePointer,
                       (int[])raster.getDataElements(0, 0, deviceWidth,
                                                     deviceHeight, null),
                       deviceWidth, deviceHeight, deviceWidth, false,
                       deviceX, deviceY);
      }
    
    else if (pc.getColorModel().equals(CairoSurface.cairoCM_opaque)
            && raster.getSampleModel().getTransferType() == DataBuffer.TYPE_INT)
      {
        // We can also optimize if the context uses a similar color model
        // but without an alpha channel; we just add the alpha
        int[] pixels = (int[])raster.getDataElements(0, 0, deviceWidth,
                                                     deviceHeight, null);
        
        for (int i = 0; i < pixels.length; i++)
          pixels[i] = 0xff000000 | (pixels[i] & 0x00ffffff);
        
        setPaintPixels(nativePointer, pixels, deviceWidth, deviceHeight,
                       deviceWidth, false, deviceX, deviceY);
      }
    
    else
      {
        // Fall back on wrapping the raster in a BufferedImage, and 
        // use BufferedImage.getRGB() to do color-model conversion 
        WritableRaster wr = Raster.createWritableRaster(raster.getSampleModel(),
                                                        new Point(raster.getMinX(),
                                                                  raster.getMinY()));
        wr.setRect(raster);
        
        BufferedImage img2 = new BufferedImage(pc.getColorModel(), wr,
                                               pc.getColorModel().isAlphaPremultiplied(),
                                               null);
        
        setPaintPixels(nativePointer,
                       img2.getRGB(0, 0, deviceWidth, deviceHeight, null, 0,
                                   deviceWidth),
                       deviceWidth, deviceHeight, deviceWidth, false,
                       deviceX, deviceY);
      }
    
    // Restore transform
    setTransformImpl(oldTx);    
  }

  public Stroke getStroke()
  {
    return stroke;
  }

  public void setStroke(Stroke st)
  {
    stroke = st;
    if (stroke instanceof BasicStroke)
      {
        BasicStroke bs = (BasicStroke) stroke;
        cairoSetLine(nativePointer, bs.getLineWidth(), bs.getEndCap(), 
                     bs.getLineJoin(), bs.getMiterLimit());

        float[] dashes = bs.getDashArray();
        if (dashes != null)
          {
            double[] double_dashes = new double[dashes.length];
            for (int i = 0; i < dashes.length; i++)
              double_dashes[i] = dashes[i];
            
            cairoSetDash(nativePointer, double_dashes, double_dashes.length,
                         (double) bs.getDashPhase());
          }
        else
          cairoSetDash(nativePointer, new double[0], 0, 0.0);
      }
  }

  /**
   * Utility method to find the bounds of a shape, including the stroke width.
   * 
   * @param s the shape
   * @return the bounds of the shape, including stroke width
   */
  protected Rectangle findStrokedBounds(Shape s)
  {
    Rectangle r = s.getBounds();
    
    if (stroke instanceof BasicStroke)
      {
        int strokeWidth = (int)Math.ceil(((BasicStroke)stroke).getLineWidth());
        r.x -= strokeWidth / 2;
        r.y -= strokeWidth / 2;
        r.height += strokeWidth;
        r.width += strokeWidth;
      }
    else
      {
        Shape s2 = stroke.createStrokedShape(s);
        r = s2.getBounds();
      }
    
    return r;
  }

  public void setPaintMode()
  {
    setComposite(AlphaComposite.SrcOver);
  }

  public void setXORMode(Color c)
  {
    // FIXME: implement
  }

  public void setColor(Color c)
  {
    if (c == null)
      c = Color.BLACK;

    fg = c;
    paint = c;
    updateColor();
  }
  
  /**
   * Set the current fg value as the cairo color.
   */
  void updateColor()
  {
    if (fg == null)
      fg = Color.BLACK;
    
    cairoSetRGBAColor(nativePointer, fg.getRed() / 255.0,
                      fg.getGreen() / 255.0,fg.getBlue() / 255.0,
                      fg.getAlpha() / 255.0);
  }

  public Color getColor()
  {
    return fg;
  }

  public void clipRect(int x, int y, int width, int height)
  {
    if (clip == null)
      setClip(new Rectangle(x, y, width, height));
    else if (clip instanceof Rectangle)
      {
        computeIntersection(x, y, width, height, (Rectangle) clip);
        setClip(clip);
      }
    else
      clip(new Rectangle(x, y, width, height));
  }

  public Shape getClip()
  {
    if (clip == null)
      return null;
    else if (clip instanceof Rectangle2D)
      return clip.getBounds2D(); //getClipInDevSpace();
    else
      {
        GeneralPath p = new GeneralPath();
        PathIterator pi = clip.getPathIterator(null);
        p.append(pi, false);
        return p;
      }
  }

  public Rectangle getClipBounds()
  {
    if (clip == null)
      return null;
    else
      return clip.getBounds();
  }

  protected Rectangle2D getClipInDevSpace()
  {
    Rectangle2D uclip = clip.getBounds2D();
    if (transform == null)
      return uclip;
    else
      return getTransformedBounds(clip.getBounds2D(), transform);
  }

  public void setClip(int x, int y, int width, int height)
  {
    if( width < 0 || height < 0 )
      return;

    setClip(new Rectangle2D.Double(x, y, width, height));
  }

  public void setClip(Shape s)
  {
    // The first time the clip is set, save it as the original clip 
    // to reset to on s == null. We can rely on this being non-null 
    // because the constructor in subclasses is expected to set the 
    // initial clip properly.
    if( firstClip )
      {
        originalClip = s;
        firstClip = false;
      }

    clip = s;
    cairoResetClip(nativePointer);

    if (clip != null)
      {
        cairoNewPath(nativePointer);
        if (clip instanceof Rectangle2D)
          {
            Rectangle2D r = (Rectangle2D) clip;
            cairoRectangle(nativePointer, r.getX(), r.getY(), r.getWidth(),
                           r.getHeight());
          }
        else
          walkPath(clip.getPathIterator(null), false);
        
        cairoClip(nativePointer);
      }
  }

  public void setBackground(Color c)
  {
    if (c == null)
      c = Color.WHITE;
    bg = c;
  }

  public Color getBackground()
  {
    return bg;
  }

  /**
   * Return the current composite.
   */
  public Composite getComposite()
  {
    if (comp == null)
      return AlphaComposite.SrcOver;
    else
      return comp;
  }

  /**
   * Sets the current composite context.
   */
  public void setComposite(Composite comp)
  {
    if (this.comp == comp)
      return;
    
    this.comp = comp;
    if (compCtx != null)
      compCtx.dispose();
    compCtx = null;

    if (comp instanceof AlphaComposite)
      {
        AlphaComposite a = (AlphaComposite) comp;
        cairoSetOperator(nativePointer, a.getRule());
      }
      
    else
      {
        cairoSetOperator(nativePointer, AlphaComposite.SRC_OVER);
        
        if (comp != null)
          {
            // FIXME: this check is only required "if this Graphics2D
            // context is drawing to a Component on the display screen".
            SecurityManager sm = System.getSecurityManager();
            if (sm != null)
              sm.checkPermission(new AWTPermission("readDisplayPixels"));
    
            compCtx = comp.createContext(getBufferCM(), getNativeCM(), hints);
          }
      }
  }
  
  /**
   * Returns the Colour Model describing the native, raw image data for this
   * specific peer.
   *  
   * @return ColorModel the ColorModel of native data in this peer
   */
  protected abstract ColorModel getNativeCM();
  
  /**
   * Returns the Color Model describing the buffer that this peer uses
   * for custom composites.
   * 
   * @return ColorModel the ColorModel of the composite buffer in this peer.
   */
  protected ColorModel getBufferCM()
  {
    // This may be overridden by some subclasses
    return getNativeCM();
  }

  ///////////////////////// DRAWING PRIMITIVES ///////////////////////////////////

  public void draw(Shape s)
  {
    if ((stroke != null && ! (stroke instanceof BasicStroke))
        || (comp instanceof AlphaComposite && ((AlphaComposite) comp).getAlpha() != 1.0))
      {
        // Cairo doesn't support stroking with alpha, so we create the stroked
        // shape and fill with alpha instead
        fill(stroke.createStrokedShape(s));
        return;
      }

    if (customPaint)
      {
        Rectangle r = findStrokedBounds(s);
        setCustomPaint(r);
      }

    setAntialias(!hints.get(RenderingHints.KEY_ANTIALIASING)
                       .equals(RenderingHints.VALUE_ANTIALIAS_OFF));
    createPath(s, true);
    cairoStroke(nativePointer);
  }

  public void fill(Shape s)
  {
    createPath(s, false);

    if (customPaint)
      setCustomPaint(s.getBounds());

    setAntialias(!hints.get(RenderingHints.KEY_ANTIALIASING)
                       .equals(RenderingHints.VALUE_ANTIALIAS_OFF));
    double alpha = 1.0;
    if (comp instanceof AlphaComposite)
      alpha = ((AlphaComposite) comp).getAlpha();
    cairoFill(nativePointer, alpha);
  }

  private void createPath(Shape s, boolean isDraw)
  {
    cairoNewPath(nativePointer);

    // Optimize rectangles, since there is a direct Cairo function
    if (s instanceof Rectangle2D)
      {
        Rectangle2D r = (Rectangle2D) s;
        
        // Pixels need to be shifted in draw operations to ensure that they
        // light up entire pixels, but we also need to make sure the rectangle
        // does not get distorted by this shifting operation
        double x = shiftX(r.getX(),shiftDrawCalls && isDraw);
        double y = shiftY(r.getY(), shiftDrawCalls && isDraw);
        double w = Math.round(r.getWidth());
        double h = Math.round(r.getHeight());
        cairoRectangle(nativePointer, x, y, w, h);
      }
    
    // Lines are easy too
    else if (s instanceof Line2D)
      {
        Line2D l = (Line2D) s;
        cairoMoveTo(nativePointer, shiftX(l.getX1(), shiftDrawCalls && isDraw),
                  shiftY(l.getY1(), shiftDrawCalls && isDraw));
        cairoLineTo(nativePointer, shiftX(l.getX2(), shiftDrawCalls && isDraw),
                  shiftY(l.getY2(), shiftDrawCalls && isDraw));
      }

    // We can optimize ellipses too; however we don't bother optimizing arcs:
    // the iterator is fast enough (an ellipse requires 5 steps using the
    // iterator, while most arcs are only 2-3)
    else if (s instanceof Ellipse2D)
      {
        Ellipse2D e = (Ellipse2D) s;

        double radius = Math.min(e.getHeight(), e.getWidth()) / 2;

        // Cairo only draws circular shapes, but we can use a stretch to make
        // them into ellipses
        double xscale = 1, yscale = 1;
        if (e.getHeight() != e.getWidth())
          {
            cairoSave(nativePointer);

            if (e.getHeight() < e.getWidth())
              xscale = e.getWidth() / (radius * 2);
            else
              yscale = e.getHeight() / (radius * 2);

            if (xscale != 1 || yscale != 1)
              cairoScale(nativePointer, xscale, yscale);
          }

        cairoArc(nativePointer,
                 shiftX(e.getCenterX() / xscale, shiftDrawCalls && isDraw),
                 shiftY(e.getCenterY() / yscale, shiftDrawCalls && isDraw),
                 radius, 0, Math.PI * 2);

        if (xscale != 1 || yscale != 1)
          cairoRestore(nativePointer);
      }

    // All other shapes are broken down and drawn in steps using the
    // PathIterator
    else
      walkPath(s.getPathIterator(null), shiftDrawCalls && isDraw);
  }

  /**
   * Note that the rest of the drawing methods go via fill() or draw() for the drawing,
   * although subclasses may with to overload these methods where context-specific 
   * optimizations are possible (e.g. bitmaps and fillRect(int, int, int, int)
   */

  public void clearRect(int x, int y, int width, int height)
  {
    if (bg != null)
      cairoSetRGBAColor(nativePointer, bg.getRed() / 255.0,
                        bg.getGreen() / 255.0, bg.getBlue() / 255.0,
                        bg.getAlpha() / 255.0);

    Composite oldcomp = comp;
    setComposite(AlphaComposite.Src);
    fillRect(x, y, width, height);

    setComposite(oldcomp);
    updateColor();
  }

  public void draw3DRect(int x, int y, int width, int height, boolean raised)
  {
    Stroke tmp = stroke;
    setStroke(draw3DRectStroke);
    super.draw3DRect(x, y, width, height, raised);
    setStroke(tmp);
  }

  public void drawArc(int x, int y, int width, int height, int startAngle,
                      int arcAngle)
  {
    draw(new Arc2D.Double((double) x, (double) y, (double) width,
                          (double) height, (double) startAngle,
                          (double) arcAngle, Arc2D.OPEN));
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    // The coordinates being pairwise identical means one wants
    // to draw a single pixel. This is emulated by drawing
    // a one pixel sized rectangle.
    if (x1 == x2 && y1 == y2)
      fill(new Rectangle(x1, y1, 1, 1));
    else
      draw(new Line2D.Double(x1, y1, x2, y2));
  }

  public void drawRect(int x, int y, int width, int height)
  {
    draw(new Rectangle(x, y, width, height));
  }

  public void fillArc(int x, int y, int width, int height, int startAngle,
                      int arcAngle)
  {
    fill(new Arc2D.Double((double) x, (double) y, (double) width,
                          (double) height, (double) startAngle,
                          (double) arcAngle, Arc2D.PIE));
  }

  public void fillRect(int x, int y, int width, int height)
  {
    fill (new Rectangle(x, y, width, height));
  }

  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    fill(new Polygon(xPoints, yPoints, nPoints));
  }

  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    draw(new Polygon(xPoints, yPoints, nPoints));
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints)
  {
    for (int i = 1; i < nPoints; i++)
      draw(new Line2D.Double(xPoints[i - 1], yPoints[i - 1],
                             xPoints[i], yPoints[i]));
  }

  public void drawOval(int x, int y, int width, int height)
  {
    drawArc(x, y, width, height, 0, 360);
  }

  public void drawRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    draw(new RoundRectangle2D.Double(x, y, width, height, arcWidth, arcHeight));
  }

  public void fillOval(int x, int y, int width, int height)
  {
    fillArc(x, y, width, height, 0, 360);
  }

  public void fillRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    fill(new RoundRectangle2D.Double(x, y, width, height, arcWidth, arcHeight));
  }

  /**
   * CopyArea - performs clipping to the native surface as a convenience 
   * (requires getRealBounds). Then calls copyAreaImpl.
   */
  public void copyArea(int ox, int oy, int owidth, int oheight, 
		       int odx, int ody)
  {
    // FIXME: does this handle a rotation transform properly?
    // (the width/height might not be correct)
    Point2D pos = transform.transform(new Point2D.Double(ox, oy),
                                      (Point2D) null);
    Point2D dim = transform.transform(new Point2D.Double(ox + owidth, 
                                                         oy + oheight),
                                      (Point2D) null);
    Point2D p2 = transform.transform(new Point2D.Double(ox + odx, oy + ody),
                                     (Point2D) null);
    int x = (int)pos.getX();
    int y = (int)pos.getY();
    int width = (int)(dim.getX() - pos.getX());
    int height = (int)(dim.getY() - pos.getY());
    int dx = (int)(p2.getX() - pos.getX());
    int dy = (int)(p2.getY() - pos.getY());

    Rectangle2D r = getRealBounds();

    if( width <= 0 || height <= 0 )
      return;
    // Return if outside the surface
    if( x + dx > r.getWidth() || y + dy > r.getHeight() )
      return;

    if( x + dx + width < r.getX() || y + dy + height < r.getY() )
      return;

    // Clip edges if necessary 
    if( x + dx < r.getX() ) // left
      {
        width = x + dx + width;
        x = (int)r.getX() - dx;
      }

    if( y + dy < r.getY() ) // top
      {
        height = y + dy + height;
        y = (int)r.getY() - dy;
      }

    if( x + dx + width >= r.getWidth() ) // right
      width = (int)r.getWidth() - dx - x;

    if( y + dy + height >= r.getHeight() ) // bottom
      height = (int)r.getHeight() - dy - y;

    copyAreaImpl(x, y, width, height, dx, dy);
  }

  ///////////////////////// RENDERING HINTS ///////////////////////////////////

  public void setRenderingHint(RenderingHints.Key hintKey, Object hintValue)
  {
    hints.put(hintKey, hintValue);

    shiftDrawCalls = hints.containsValue(RenderingHints.VALUE_STROKE_NORMALIZE)
      || hints.containsValue(RenderingHints.VALUE_STROKE_DEFAULT);
  }

  public Object getRenderingHint(RenderingHints.Key hintKey)
  {
    return hints.get(hintKey);
  }

  public void setRenderingHints(Map<?,?> hints)
  {
    this.hints = new RenderingHints(getDefaultHints());
    this.hints.putAll(hints);
    
    shiftDrawCalls = hints.containsValue(RenderingHints.VALUE_STROKE_NORMALIZE)
      || hints.containsValue(RenderingHints.VALUE_STROKE_DEFAULT);
    
    if (compCtx != null)
      {
        compCtx.dispose();
        compCtx = comp.createContext(getNativeCM(), getNativeCM(), this.hints);
      }
  }

  public void addRenderingHints(Map hints)
  {
    this.hints.putAll(hints);
  }

  public RenderingHints getRenderingHints()
  {
    return hints;
  }
  
  private int getInterpolation()
  {
    if (this.hints.containsValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR))
      return INTERPOLATION_NEAREST;

    else if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR))
      return INTERPOLATION_BILINEAR;

    else if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC))
      return INTERPOLATION_BICUBIC;

    else if (hints.containsValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED))
      return ALPHA_INTERPOLATION_SPEED;

    else if (hints.containsValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY))
      return ALPHA_INTERPOLATION_QUALITY;

    else if (hints.containsValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT))
      return ALPHA_INTERPOLATION_DEFAULT;

    // Do bilinear interpolation as default
    return INTERPOLATION_BILINEAR;
  }
  
  /**
   * Set antialias if needed.  If the ignoreAA flag is set, this method will
   * return without doing anything.
   * 
   * @param needAA RenderingHints.VALUE_ANTIALIAS_ON or RenderingHints.VALUE_ANTIALIAS_OFF
   */
  private void setAntialias(boolean needAA)
  {
    if (ignoreAA)
      return;
    
    if (needAA != antialias)
      {
        antialias = !antialias;
        cairoSetAntialias(nativePointer, antialias);
      }
  }

  ///////////////////////// IMAGE. METHODS ///////////////////////////////////

  protected boolean drawImage(Image img, AffineTransform xform,
                            Color bgcolor, ImageObserver obs)
  {
    if (img == null)
      return false;

    if (xform == null)
      xform = new AffineTransform();

    // In this case, xform is an AffineTransform that transforms bounding
    // box of the specified image from image space to user space. However
    // when we pass this transform to cairo, cairo will use this transform
    // to map "user coordinates" to "pixel" coordinates, which is the 
    // other way around. Therefore to get the "user -> pixel" transform 
    // that cairo wants from "image -> user" transform that we currently
    // have, we will need to invert the transformation matrix.
    AffineTransform invertedXform;

    try
      {
        invertedXform = xform.createInverse();
      }
    catch (NoninvertibleTransformException e)
      {
        throw new ImagingOpException("Unable to invert transform "
                                     + xform.toString());
      }

    // Unrecognized image - convert to a BufferedImage
    // Note - this can get us in trouble when the gdk lock is re-acquired.
    // for example by VolatileImage. See ComponentGraphics for how we work
    // around this.
    img = AsyncImage.realImage(img, obs);
    if( !(img instanceof BufferedImage) )
      {
        ImageProducer source = img.getSource();
        if (source == null)
          return false;
        img = Toolkit.getDefaultToolkit().createImage(source);
      }

    BufferedImage b = (BufferedImage) img;
    Raster raster;
    double[] i2u = new double[6];
    int width = b.getWidth();
    int height = b.getHeight();
    
    // If this BufferedImage has a BufferedImageGraphics object, 
    // use the cached CairoSurface that BIG is drawing onto
    
    if( BufferedImageGraphics.bufferedImages.get( b ) != null )
      raster = BufferedImageGraphics.bufferedImages.get( b );
    else
      raster = b.getRaster();

    invertedXform.getMatrix(i2u);

    double alpha = 1.0;
    if (comp instanceof AlphaComposite)
      alpha = ((AlphaComposite) comp).getAlpha();

    if(raster instanceof CairoSurface
        && ((CairoSurface)raster).sharedBuffer == true)
      {
        drawCairoSurface((CairoSurface)raster, xform, alpha, getInterpolation());
        updateColor();
        return true;
      }
	    
    if( bgcolor != null )
      {
        Color oldColor = bg;
        setBackground(bgcolor);
        
        Rectangle2D bounds = new Rectangle2D.Double(0, 0, width, height);
        bounds = getTransformedBounds(bounds, xform);
        
        clearRect((int)bounds.getX(), (int)bounds.getY(),
                  (int)bounds.getWidth(), (int)bounds.getHeight());
        
        setBackground(oldColor);
      }

    int[] pixels = b.getRGB(0, 0, width, height, null, 0, width);
    // FIXME: The above method returns data in the standard ARGB colorspace,
    // meaning data should NOT be alpha pre-multiplied; however Cairo expects
    // data to be premultiplied.
    
    cairoSave(nativePointer);
    Rectangle2D bounds = new Rectangle2D.Double(0, 0, width, height);
    bounds = getTransformedBounds(bounds, xform);
    cairoRectangle(nativePointer, bounds.getX(), bounds.getY(),
                   bounds.getWidth(), bounds.getHeight());
    cairoClip(nativePointer);

    drawPixels(nativePointer, pixels, width, height, width, i2u, alpha,
               getInterpolation());
    
    cairoRestore(nativePointer);

    // Cairo seems to lose the current color which must be restored.
    updateColor();
    return true;
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    drawRaster(image.getColorModel(), image.getData(), xform, null);
  }

  public void drawRenderableImage(RenderableImage image, AffineTransform xform)
  {
    drawRenderedImage(image.createRendering(new RenderContext(xform)), xform);
  }

  public boolean drawImage(Image img, AffineTransform xform, ImageObserver obs)
  {
    return drawImage(img, xform, null, obs);
  }

  public void drawImage(BufferedImage image, BufferedImageOp op, int x, int y)
  {
    Image filtered = image;
    if (op != null)
      filtered = op.filter(image, null);
    drawImage(filtered, new AffineTransform(1f, 0f, 0f, 1f, x, y), null, null);
  }

  public boolean drawImage(Image img, int x, int y, ImageObserver observer)
  {
    return drawImage(img, new AffineTransform(1f, 0f, 0f, 1f, x, y), null,
                     observer);
  }

  public boolean drawImage(Image img, int x, int y, Color bgcolor,
                           ImageObserver observer)
  {
    return drawImage(img, x, y, img.getWidth(observer),
                     img.getHeight(observer), bgcolor, observer);
  }

  public boolean drawImage(Image img, int x, int y, int width, int height,
                           Color bgcolor, ImageObserver observer)
  {
    double scaleX = width / (double) img.getWidth(observer);
    double scaleY = height / (double) img.getHeight(observer);
    if( scaleX == 0 || scaleY == 0 )
      return true;

    return drawImage(img, new AffineTransform(scaleX, 0f, 0f, scaleY, x, y),
                     bgcolor, observer);
  }

  public boolean drawImage(Image img, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    return drawImage(img, x, y, width, height, null, observer);
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2, Color bgcolor,
                           ImageObserver observer)
  {
    if (img == null)
      return false;

    int sourceWidth = sx2 - sx1;
    int sourceHeight = sy2 - sy1;

    int destWidth = dx2 - dx1;
    int destHeight = dy2 - dy1;

    if(destWidth == 0 || destHeight == 0 || sourceWidth == 0 || 
       sourceHeight == 0)
      return true;

    double scaleX = destWidth / (double) sourceWidth;
    double scaleY = destHeight / (double) sourceHeight;

    // FIXME: Avoid using an AT if possible here - it's at least twice as slow.
    
    Shape oldClip = getClip();
    int cx, cy, cw, ch;
    if( dx1 < dx2 ) 
      { cx = dx1; cw = dx2 - dx1; }
    else
      { cx = dx2; cw = dx1 - dx2; }
    if( dy1 < dy2 ) 
      { cy = dy1; ch = dy2 - dy1; }
    else
      { cy = dy2; ch = dy1 - dy2; }
    
    clipRect( cx, cy, cw, ch );

    AffineTransform tx = new AffineTransform();
    tx.translate( dx1 - sx1*scaleX, dy1 - sy1*scaleY );
    tx.scale( scaleX, scaleY );

    boolean retval = drawImage(img, tx, bgcolor, observer);
    setClip( oldClip );
    return retval;
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           ImageObserver observer)
  {
    return drawImage(img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, null, observer);
  }
  
  /**
   * Optimized method for drawing a CairoSurface onto this graphics context.
   * 
   * @param surface The surface to draw.
   * @param tx The transformation matrix (cannot be null).
   * @param alpha The alpha value to paint with ( 0 <= alpha <= 1).
   * @param interpolation The interpolation type.
   */
  protected void drawCairoSurface(CairoSurface surface, AffineTransform tx,
                                  double alpha, int interpolation)
  {
    // Find offset required if this surface is a sub-raster, and append offset
    // to transformation.
    if (surface.getSampleModelTranslateX() != 0
        || surface.getSampleModelTranslateY() != 0)
      {
        Point2D origin = new Point2D.Double(0, 0);
        Point2D offset = new Point2D.Double(surface.getSampleModelTranslateX(),
                                            surface.getSampleModelTranslateY());
        
        tx.transform(origin, origin);
        tx.transform(offset, offset);
        
        tx.translate(offset.getX() - origin.getX(),
                     offset.getY() - origin.getY());
      }
    
    // Find dimensions of this surface relative to the root parent surface
    Rectangle bounds = new Rectangle(-surface.getSampleModelTranslateX(),
                                     -surface.getSampleModelTranslateY(),
                                     surface.width, surface.height);
    
    // Clip to the translated image
    //   We use direct cairo methods to avoid the overhead of maintaining a
    //   java copy of the clip, since we will be reverting it immediately
    //   after drawing
    Shape newBounds = tx.createTransformedShape(bounds);
    cairoSave(nativePointer);
    walkPath(newBounds.getPathIterator(null), false);
    cairoClip(nativePointer);
    
    // Draw the surface
    try
    {
      double[] i2u = new double[6];
      tx.createInverse().getMatrix(i2u);
      surface.nativeDrawSurface(surface.surfacePointer, nativePointer, i2u,
                                alpha, interpolation);
    }
    catch (NoninvertibleTransformException ex)
    {
      // This should never happen(?), so we don't need to do anything here.
      ;
    }
    
    // Restore clip
    cairoRestore(nativePointer);
  }


  ///////////////////////// TEXT METHODS ////////////////////////////////////

  public void drawString(String str, float x, float y)
  {
    if (str == null || str.length() == 0)
      return;
    GdkFontPeer fontPeer = (GdkFontPeer) font.getPeer();
    TextLayout tl = (TextLayout) fontPeer.textLayoutCache.get(str);
    if (tl == null)
      {
        tl = new TextLayout( str, getFont(), getFontRenderContext() );
        fontPeer.textLayoutCache.put(str, tl);
      }
    
    // Set antialias to text_antialiasing, and set the ignoreAA flag so that
    // the setting doesn't get overridden in a draw() or fill() call.
    setAntialias(!hints.get(RenderingHints.KEY_TEXT_ANTIALIASING)
                       .equals(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    ignoreAA = true;
    
    tl.draw(this, x, y);
    ignoreAA = false;
  }

  public void drawString(String str, int x, int y)
  {
    drawString (str, (float) x, (float) y);
  }

  public void drawString(AttributedCharacterIterator ci, int x, int y)
  {
    drawString (ci, (float) x, (float) y);
  }

  public void drawGlyphVector(GlyphVector gv, float x, float y)
  {
    double alpha = 1.0;

    if( gv.getNumGlyphs() <= 0 )
      return;

    if (customPaint)
      setCustomPaint(gv.getOutline().getBounds());
    
    if (comp instanceof AlphaComposite)
      alpha = ((AlphaComposite) comp).getAlpha();
    
    setAntialias(!hints.get(RenderingHints.KEY_TEXT_ANTIALIASING)
                       .equals(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    ignoreAA = true;
    
    if (gv instanceof FreetypeGlyphVector && alpha == 1.0
        && !((FreetypeGlyphVector)gv).hasTransforms())
      {
        int n = gv.getNumGlyphs ();
        int[] codes = gv.getGlyphCodes (0, n, null);
        long[] fontset = ((FreetypeGlyphVector)gv).getGlyphFonts (0, n, null);
        float[] positions = gv.getGlyphPositions (0, n, null);

        setFont (gv.getFont ());
        GdkFontPeer fontPeer = (GdkFontPeer) font.getPeer();
        synchronized (fontPeer) 
          { 
            cairoDrawGlyphVector(nativePointer, fontPeer,
                                 x, y, n, codes, positions, fontset);
          }
      }
    else
      {
        translate(x, y);
        fill(gv.getOutline());
        translate(-x, -y);
      }
    
    ignoreAA = false;
  }

  public void drawString(AttributedCharacterIterator ci, float x, float y)
  {
    GlyphVector gv = getFont().createGlyphVector(getFontRenderContext(), ci);
    drawGlyphVector(gv, x, y);
  }

  /**
   * Should perhaps be contexct dependent, but this is left for now as an 
   * overloadable default implementation.
   */
  public FontRenderContext getFontRenderContext()
  {
    return new FontRenderContext(transform, true, true);
  }

  // Until such time as pango is happy to talk directly to cairo, we
  // actually need to redirect some calls from the GtkFontPeer and
  // GtkFontMetrics into the drawing kit and ask cairo ourselves.

  public FontMetrics getFontMetrics()
  {
    return getFontMetrics(getFont());
  }

  public FontMetrics getFontMetrics(Font f)
  {
    return ((GdkFontPeer) f.getPeer()).getFontMetrics(f);
  }

  public void setFont(Font f)
  {
    // Sun's JDK does not throw NPEs, instead it leaves the current setting
    // unchanged. So do we.
    if (f == null)
      return;

    if (f.getPeer() instanceof GdkFontPeer)
      font = f;
    else
      font = 
        ((ClasspathToolkit)(Toolkit.getDefaultToolkit()))
        .getFont(f.getName(), f.getAttributes());    
    
    GdkFontPeer fontpeer = (GdkFontPeer) getFont().getPeer();
    synchronized (fontpeer)
      {
        cairoSetFont(nativePointer, fontpeer);
      }
  }

  public Font getFont()
  {
    if (font == null)
      return new Font("SansSerif", Font.PLAIN, 12);
    return font;
  }

  /////////////////////// MISC. PUBLIC METHODS /////////////////////////////////

  public boolean hit(Rectangle rect, Shape s, boolean onStroke)
  {
    if( onStroke )
      {
        Shape stroked = stroke.createStrokedShape( s );
        return stroked.intersects( (double)rect.x, (double)rect.y, 
                                   (double)rect.width, (double)rect.height );
      }
    return s.intersects( (double)rect.x, (double)rect.y, 
			 (double)rect.width, (double)rect.height );
  }

  public String toString()
  {
    return  (getClass().getName()
             + "[font=" + getFont().toString()
             + ",color=" + fg.toString()
	     + "]");
  }

  ///////////////////////// PRIVATE METHODS ///////////////////////////////////

  /**
   * All the drawImage() methods eventually get delegated here if the image
   * is not a Cairo surface.
   *
   * @param bgcolor - if non-null draws the background color before 
   * drawing the image.
   */
  private boolean drawRaster(ColorModel cm, Raster r,
                             AffineTransform imageToUser, Color bgcolor)
  {
    if (r == null)
      return false;

    SampleModel sm = r.getSampleModel();
    DataBuffer db = r.getDataBuffer();

    if (db == null || sm == null)
      return false;

    if (cm == null)
      cm = ColorModel.getRGBdefault();

    double[] i2u = new double[6];
    if (imageToUser != null)
      imageToUser.getMatrix(i2u);
    else
      {
        i2u[0] = 1;
        i2u[1] = 0;
        i2u[2] = 0;
        i2u[3] = 1;
        i2u[4] = 0;
        i2u[5] = 0;
      }

    int[] pixels = findSimpleIntegerArray(cm, r);

    if (pixels == null)
      {
        // FIXME: I don't think this code will work correctly with a non-RGB
        // MultiPixelPackedSampleModel. Although this entire method should 
        // probably be rewritten to better utilize Cairo's different supported
        // data formats.
        if (sm instanceof MultiPixelPackedSampleModel)
          {
            pixels = r.getPixels(0, 0, r.getWidth(), r.getHeight(), pixels);
            for (int i = 0; i < pixels.length; i++)
              pixels[i] = cm.getRGB(pixels[i]);
          }
        else
          {
            pixels = new int[r.getWidth() * r.getHeight()];
            for (int i = 0; i < pixels.length; i++)
              pixels[i] = cm.getRGB(db.getElem(i));
          }
      }

    // Change all transparent pixels in the image to the specified bgcolor,
    // or (if there's no alpha) fill in an alpha channel so that it paints
    // correctly.
    if (cm.hasAlpha())
      {
        if (bgcolor != null && cm.hasAlpha())
          for (int i = 0; i < pixels.length; i++)
            {
              if (cm.getAlpha(pixels[i]) == 0)
                pixels[i] = bgcolor.getRGB();
            }
      }
    else
      for (int i = 0; i < pixels.length; i++)
        pixels[i] |= 0xFF000000;

    double alpha = 1.0;
    if (comp instanceof AlphaComposite)
      alpha = ((AlphaComposite) comp).getAlpha();
    
    drawPixels(nativePointer, pixels, r.getWidth(), r.getHeight(),
               r.getWidth(), i2u, alpha, getInterpolation());

    // Cairo seems to lose the current color which must be restored.
    updateColor();
    
    return true;
  }

  /**
   * Shifts an x-coordinate by 0.5 in device space.
   */
  private double shiftX(double coord, boolean doShift)
  {
    if (doShift)
      {
        double shift = 0.5;
        if (!transform.isIdentity())
          shift /= transform.getScaleX();
        return (coord + shift);
      }
    else
      return coord;
  }

  /**
   * Shifts a y-coordinate by 0.5 in device space.
   */
  private double shiftY(double coord, boolean doShift)
  {
    if (doShift)
      {
        double shift = 0.5;
        if (!transform.isIdentity())
          shift /= transform.getScaleY();
        return (coord + shift);
      }
    else
      return coord;
  }

  /**
   * Adds a pathIterator to the current Cairo path, also sets the cairo winding rule.
   */
  private void walkPath(PathIterator p, boolean doShift)
  {
    double x = 0;
    double y = 0;
    double[] coords = new double[6];

    cairoSetFillRule(nativePointer, p.getWindingRule());
    for (; ! p.isDone(); p.next())
      {
        int seg = p.currentSegment(coords);
        switch (seg)
          {
          case PathIterator.SEG_MOVETO:
            x = shiftX(coords[0], doShift);
            y = shiftY(coords[1], doShift);
            cairoMoveTo(nativePointer, x, y);
            break;
          case PathIterator.SEG_LINETO:
            x = shiftX(coords[0], doShift);
            y = shiftY(coords[1], doShift);
            cairoLineTo(nativePointer, x, y);
            break;
          case PathIterator.SEG_QUADTO:
            // splitting a quadratic bezier into a cubic:
            // see: http://pfaedit.sourceforge.net/bezier.html
            double x1 = x + (2.0 / 3.0) * (shiftX(coords[0], doShift) - x);
            double y1 = y + (2.0 / 3.0) * (shiftY(coords[1], doShift) - y);

            double x2 = x1 + (1.0 / 3.0) * (shiftX(coords[2], doShift) - x);
            double y2 = y1 + (1.0 / 3.0) * (shiftY(coords[3], doShift) - y);

            x = shiftX(coords[2], doShift);
            y = shiftY(coords[3], doShift);
            cairoCurveTo(nativePointer, x1, y1, x2, y2, x, y);
            break;
          case PathIterator.SEG_CUBICTO:
            x = shiftX(coords[4], doShift);
            y = shiftY(coords[5], doShift);
            cairoCurveTo(nativePointer, shiftX(coords[0], doShift),
                         shiftY(coords[1], doShift),
                         shiftX(coords[2], doShift),
                         shiftY(coords[3], doShift), x, y);
            break;
          case PathIterator.SEG_CLOSE:
            cairoClosePath(nativePointer);
            break;
          }
      }
  }

  /**
   * Used by setRenderingHints()
   */
  private Map<RenderingHints.Key, Object> getDefaultHints()
  {
    HashMap<RenderingHints.Key, Object> defaultHints =
      new HashMap<RenderingHints.Key, Object>();

    defaultHints.put(RenderingHints.KEY_TEXT_ANTIALIASING,
                     RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT);

    defaultHints.put(RenderingHints.KEY_STROKE_CONTROL,
                     RenderingHints.VALUE_STROKE_DEFAULT);

    defaultHints.put(RenderingHints.KEY_FRACTIONALMETRICS,
                     RenderingHints.VALUE_FRACTIONALMETRICS_OFF);

    defaultHints.put(RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_OFF);

    defaultHints.put(RenderingHints.KEY_RENDERING,
                     RenderingHints.VALUE_RENDER_DEFAULT);

    return defaultHints;
  }

  /**
   * Used by drawRaster and GdkPixbufDecoder
   */
  public static int[] findSimpleIntegerArray (ColorModel cm, Raster raster)
  {
    if (cm == null || raster == null)
      return null;

    if (! cm.getColorSpace().isCS_sRGB())
      return null;

    if (! (cm instanceof DirectColorModel))
      return null;

    DirectColorModel dcm = (DirectColorModel) cm;

    if (dcm.getRedMask() != 0x00FF0000 || dcm.getGreenMask() != 0x0000FF00
        || dcm.getBlueMask() != 0x000000FF)
      return null;

    if (! (raster instanceof WritableRaster))
      return null;

    if (raster.getSampleModel().getDataType() != DataBuffer.TYPE_INT)
      return null;

    if (! (raster.getDataBuffer() instanceof DataBufferInt))
      return null;

    DataBufferInt db = (DataBufferInt) raster.getDataBuffer();

    if (db.getNumBanks() != 1)
      return null;

    // Finally, we have determined that this is a single bank, [A]RGB-int
    // buffer in sRGB space. It's worth checking all this, because it means
    // that cairo can paint directly into the data buffer, which is very
    // fast compared to all the normal copying and converting.

    return db.getData();
  }

  /**
   * Helper method to transform the clip. This is called by the various
   * transformation-manipulation methods to update the clip (which is in
   * userspace) accordingly.
   *
   * The transform usually is the inverse transform that was applied to the
   * graphics object.
   *
   * @param t the transform to apply to the clip
   */
  private void updateClip(AffineTransform t)
  {
    if (clip == null)
      return;

    // If the clip is a rectangle, and the transformation preserves the shape
    // (translate/stretch only), then keep the clip as a rectangle
    double[] matrix = new double[4];
    t.getMatrix(matrix);
    if (clip instanceof Rectangle2D && matrix[1] == 0 && matrix[2] == 0)
      {
        Rectangle2D rect = (Rectangle2D)clip;
        double[] origin = new double[] {rect.getX(), rect.getY()};
        double[] dimensions = new double[] {rect.getWidth(), rect.getHeight()};
        t.transform(origin, 0, origin, 0, 1);
        t.deltaTransform(dimensions, 0, dimensions, 0, 1);
        rect.setRect(origin[0], origin[1], dimensions[0], dimensions[1]);
      }
    else
      {
        if (! (clip instanceof GeneralPath))
          clip = new GeneralPath(clip);
    
        GeneralPath p = (GeneralPath) clip;
        p.transform(t);
      }
  }

  private static Rectangle computeIntersection(int x, int y, int w, int h,
                                               Rectangle rect)
  {
    int x2 = rect.x;
    int y2 = rect.y;
    int w2 = rect.width;
    int h2 = rect.height;

    int dx = (x > x2) ? x : x2;
    int dy = (y > y2) ? y : y2;
    int dw = (x + w < x2 + w2) ? (x + w - dx) : (x2 + w2 - dx);
    int dh = (y + h < y2 + h2) ? (y + h - dy) : (y2 + h2 - dy);

    if (dw >= 0 && dh >= 0)
      rect.setBounds(dx, dy, dw, dh);
    else
      rect.setBounds(0, 0, 0, 0);

    return rect;
  }
  
  static Rectangle2D getTransformedBounds(Rectangle2D bounds, AffineTransform tx)
  {
    double x1 = bounds.getX();
    double x2 = bounds.getX() + bounds.getWidth();
    double x3 = x1;
    double x4 = x2;
    double y1 = bounds.getY();
    double y2 = y1;
    double y3 = bounds.getY() + bounds.getHeight();
    double y4 = y3;
    
    double[] points = new double[] {x1, y1, x2, y2, x3, y3, x4, y4};
    tx.transform(points, 0, points, 0, 4);
    
    double minX = points[0];
    double maxX = minX;
    double minY = points[1];
    double maxY = minY;
    for (int i = 0; i < 8; i++)
      {
        if (points[i] < minX)
          minX = points[i];
        if (points[i] > maxX)
          maxX = points[i];
        i++;
        
        if (points[i] < minY)
          minY = points[i];
        if (points[i] > maxY)
          maxY = points[i];
      }
    
    return new Rectangle2D.Double(minX, minY, (maxX - minX), (maxY - minY));
  }
}
