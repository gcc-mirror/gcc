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

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.Polygon;
import java.awt.TexturePaint;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Line2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorModel;
import java.awt.image.CropImageFilter;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.awt.image.DirectColorModel;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageObserver;
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
import java.util.Stack;

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
    System.loadLibrary("gtkpeer");
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

  /**
   * Rendering hint map.
   */
  private RenderingHints hints;

  /**
   * Some operations (drawing rather than filling) require that their
   * coords be shifted to land on 0.5-pixel boundaries, in order to land on
   * "middle of pixel" coordinates and light up complete pixels. 
   */
  private boolean shiftDrawCalls = false;

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
   * Constructor does nothing.
   */
  public CairoGraphics2D()
  {
  }

  /**
   * Sets up the default values and allocates the native cairographics2d structure
   * @param cairo_t_pointer, a native pointer to a cairo_t of the context.
   */
  public void setup(long cairo_t_pointer)
  { 
    nativePointer = init(cairo_t_pointer);
    setRenderingHints(new RenderingHints(getDefaultHints()));
    font = new Font("SansSerif", Font.PLAIN, 12);
    setColor(Color.black);
    setBackground(Color.white);
    setPaint(Color.black);
    setStroke(new BasicStroke());
    setTransform(new AffineTransform());
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

    if (g.clip == null)
      clip = null;
    else
      clip = new Rectangle(g.getClipBounds());

    if (g.transform == null)
      transform = new AffineTransform();
    else
      transform = new AffineTransform(g.transform);

    font = g.font;

    setColor(foreground);
    setBackground(bg);
    setPaint(paint);
    setStroke(stroke);
    setTransform(transform);
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
    disposeNative();
    nativePointer = 0;
  }

  /**
   * Allocate the cairographics2d structure and set the cairo_t pointer in it.
   * @param pointer - a cairo_t pointer, casted to a long.
   */
  private native long init(long pointer);

  /**
   * These are declared abstract as there may be context-specific issues.
   */
  public abstract Graphics create();

  public abstract GraphicsConfiguration getDeviceConfiguration();

  protected abstract void copyAreaImpl(int x, int y, 
				       int width, int height, int dx, int dy);


  protected abstract Rectangle2D getRealBounds();

  ////// Native Methods ////////////////////////////////////////////////////

  /**
   * Dispose of allocate native resouces.
   */
  public native void disposeNative();

  /**
   * Draw pixels as an RGBA int matrix
   * @param w, h - width and height
   * @param stride - stride of the array width
   * @param i2u - affine transform array
   */
  private native void drawPixels(int[] pixels, int w, int h, int stride,
                                 double[] i2u);

  private native void setGradient(double x1, double y1, double x2, double y2,
                                  int r1, int g1, int b1, int a1, int r2,
                                  int g2, int b2, int a2, boolean cyclic);
  
  private native void setTexturePixels(int[] pixels, int w, int h, int stride);

  /**
   * Set the current transform matrix
   */
  private native void cairoSetMatrix(double[] m);

  /**
   * Set the compositing operator
   */
  private native void cairoSetOperator(int cairoOperator);

  /**
   * Sets the current color in RGBA as a 0.0-1.0 double
   */
  private native void cairoSetRGBAColor(double red, double green,
                                        double blue, double alpha);

  /**
   * Sets the current winding rule in Cairo
   */
  private native void cairoSetFillRule(int cairoFillRule);

  /**
   * Set the line style, cap, join and miter limit.
   * Cap and join parameters are in the BasicStroke enumerations.
   */
  private native void cairoSetLine(double width, int cap, int join, double miterLimit);

  /**
   * Set the dash style
   */
  private native void cairoSetDash(double[] dashes, int ndash, double offset);

  /*
   * Draws a Glyph Vector
   */
  native void cairoDrawGlyphVector(GdkFontPeer font, 
                                   float x, float y, int n, 
                                   int[] codes, float[] positions);


  private native void cairoRelCurveTo(double dx1, double dy1, double dx2,
                                      double dy2, double dx3, double dy3);

  /**
   * Appends a rectangle to the current path
   */
  private native void cairoRectangle(double x, double y, double width,
                                     double height);

  /**
   * New current path
   */
  private native void cairoNewPath();

  /** 
   * Close current path
   */
  private native void cairoClosePath();

  /** moveTo */
  private native void cairoMoveTo(double x, double y);

  /** relative moveTo */
  private native void cairoRelMoveTo(double dx, double dy);

  /** lineTo */
  private native void cairoLineTo(double x, double y);

  /** relative lineTo */
  private native void cairoRelLineTo(double dx, double dy);

  /** Cubic curve-to */
  private native void cairoCurveTo(double x1, double y1, double x2, double y2,
                                   double x3, double y3);

  /**
   * Stroke current path
   */
  private native void cairoStroke();

  /**
   * Fill current path
   */
  private native void cairoFill();

  /** 
   * Clip current path
   */
  private native void cairoClip();

  /** 
   * Save clip
   */
  private native void cairoPreserveClip();

  /** 
   * Save clip
   */
  private native void cairoResetClip();

  /**
   * Set interpolation types
   */
  private native void cairoSurfaceSetFilter(int filter);

  ///////////////////////// TRANSFORMS ///////////////////////////////////
  /**
   * Set the current transform
   */ 
  public void setTransform(AffineTransform tx)
  {
    transform = tx;
    if (transform != null)
      {
	double[] m = new double[6];
	transform.getMatrix(m);
	cairoSetMatrix(m);
      }
  }
  
  public void transform(AffineTransform tx)
  {
    if (transform == null)
      transform = new AffineTransform(tx);
    else
      transform.concatenate(tx);
    setTransform(transform);
    if (clip != null)
      {
	// FIXME: this should actuall try to transform the shape
	// rather than degrade to bounds.
	Rectangle2D r = clip.getBounds2D();
	double[] coords = new double[]
	  {
	    r.getX(), r.getY(), r.getX() + r.getWidth(),
	    r.getY() + r.getHeight()
	  };
	try
	  {
	    tx.createInverse().transform(coords, 0, coords, 0, 2);
	    r.setRect(coords[0], coords[1], coords[2] - coords[0],
	              coords[3] - coords[1]);
	    clip = r;
	  }
	catch (java.awt.geom.NoninvertibleTransformException e)
	  {
	  }
      }
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
        Rectangle2D r;

        if (clip instanceof Rectangle2D)
          r = (Rectangle2D) clip;
        else
          r = clip.getBounds2D();

        r.setRect(r.getX() - tx, r.getY() - ty, r.getWidth(), r.getHeight());
        clip = r;
      }

    setTransform(transform);
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
      return;

    // If the current clip is still null, initialize it.
    if (clip == null)
      clip = originalClip;
    
    // This is so common, let's optimize this. 
    else if (clip instanceof Rectangle2D && s instanceof Rectangle2D)
      {
        Rectangle2D clipRect = (Rectangle2D) clip;
        Rectangle2D r = (Rectangle2D) s;
        Rectangle2D.intersect(clipRect, r, clipRect);
        // Call setClip so that subclasses get notified.
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
    if (paint == null)
      return;

    paint = p;
    if (paint instanceof Color)
      {
        setColor((Color) paint);
      }
    else if (paint instanceof TexturePaint)
      {
	TexturePaint tp = (TexturePaint) paint;
	BufferedImage img = tp.getImage();

	// map the image to the anchor rectangle  
	int width = (int) tp.getAnchorRect().getWidth();
	int height = (int) tp.getAnchorRect().getHeight();

	double scaleX = (width+1) / (double) img.getWidth();
	double scaleY = (height+1) / (double) img.getHeight();

	AffineTransform at = new AffineTransform(scaleX, 0, 0, scaleY, 0, 0);
	AffineTransformOp op = new AffineTransformOp(at, getRenderingHints());
	BufferedImage texture = op.filter(img, null);
	int[] pixels = texture.getRGB(0, 0, width, height, null, 0, width);
	setTexturePixels(pixels, width, height, width);
      }
    else if (paint instanceof GradientPaint)
      {
	GradientPaint gp = (GradientPaint) paint;
	Point2D p1 = gp.getPoint1();
	Point2D p2 = gp.getPoint2();
	Color c1 = gp.getColor1();
	Color c2 = gp.getColor2();
	setGradient(p1.getX(), p1.getY(), p2.getX(), p2.getY(), c1.getRed(),
	            c1.getGreen(), c1.getBlue(), c1.getAlpha(), c2.getRed(),
	            c2.getGreen(), c2.getBlue(), c2.getAlpha(), gp.isCyclic());
      }
    else
      throw new java.lang.UnsupportedOperationException();
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
	cairoSetLine(bs.getLineWidth(), bs.getEndCap(), 
		     bs.getLineJoin(), bs.getMiterLimit());

	float[] dashes = bs.getDashArray();
	if (dashes != null)
	  {
	    double[] double_dashes = new double[dashes.length];
	    for (int i = 0; i < dashes.length; i++)
	      double_dashes[i] = dashes[i];
	    cairoSetDash(double_dashes, double_dashes.length,
	                 (double) bs.getDashPhase());
	  }
	else
	  cairoSetDash(new double[0], 0, 0.0);
      }
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
    cairoSetRGBAColor(fg.getRed() / 255.0, fg.getGreen() / 255.0,
                      fg.getBlue() / 255.0, fg.getAlpha() / 255.0);
  }

  public Color getColor()
  {
    return fg;
  }

  public void clipRect(int x, int y, int width, int height)
  {
    clip(new Rectangle(x, y, width, height));
  }

  public Shape getClip()
  {
    if (clip == null)
      return null;
    else
      return clip.getBounds2D(); //getClipInDevSpace();
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
      {
	Point2D pos = transform.transform(new Point2D.Double(uclip.getX(),
	                                                     uclip.getY()),
	                                  (Point2D) null);
	Point2D extent = transform.deltaTransform(new Point2D.Double(uclip
	                                                             .getWidth(),
	                                                             uclip
	                                                             .getHeight()),
	                                          (Point2D) null);
	return new Rectangle2D.Double(pos.getX(), pos.getY(), extent.getX(),
	                              extent.getY());
      }
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

    if (s == null)
      clip = originalClip;
    else
      clip = s;

    cairoResetClip();

    cairoNewPath();
    if (clip instanceof Rectangle2D)
      {
	Rectangle2D r = (Rectangle2D) clip;
	cairoRectangle(r.getX(), r.getY(), r.getWidth(), r.getHeight());
      }
    else
      walkPath(clip.getPathIterator(null), false);
    
    cairoClip();
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
    this.comp = comp;

    if (comp instanceof AlphaComposite)
      {
	AlphaComposite a = (AlphaComposite) comp;
	cairoSetOperator(a.getRule());
	Color c = getColor();
	setColor(new Color(c.getRed(), c.getGreen(), c.getBlue(),
	                   (int) (a.getAlpha() * ((float) c.getAlpha()))));
      }
    else
      {
        // FIXME: implement general Composite support
        throw new java.lang.UnsupportedOperationException();
      }
  }

  ///////////////////////// DRAWING PRIMITIVES ///////////////////////////////////

  public void draw(Shape s)
  {
    if (stroke != null && ! (stroke instanceof BasicStroke))
      {
	fill(stroke.createStrokedShape(s));
	return;
      }

    cairoNewPath();

    if (s instanceof Rectangle2D)
      {
	Rectangle2D r = (Rectangle2D) s;
	cairoRectangle(shifted(r.getX(), shiftDrawCalls),
	               shifted(r.getY(), shiftDrawCalls), r.getWidth(),
	               r.getHeight());
      }
    else
      walkPath(s.getPathIterator(null), shiftDrawCalls);
    cairoStroke();
  }

  public void fill(Shape s)
  {
    cairoNewPath();
    if (s instanceof Rectangle2D)
      {
	Rectangle2D r = (Rectangle2D) s;
	cairoRectangle(r.getX(), r.getY(), r.getWidth(), r.getHeight());
      }
    else
      walkPath(s.getPathIterator(null), false);

    cairoFill();
  }

  /**
   * Note that the rest of the drawing methods go via fill() or draw() for the drawing,
   * although subclasses may with to overload these methods where context-specific 
   * optimizations are possible (e.g. bitmaps and fillRect(int, int, int, int)
   */

  public void clearRect(int x, int y, int width, int height)
  {
    if (bg != null)
      cairoSetRGBAColor(bg.getRed() / 255.0, bg.getGreen() / 255.0,
			bg.getBlue() / 255.0, 1.0);
    fillRect(x, y, width, height);
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
                          (double) arcAngle, Arc2D.OPEN));
  }

  public void fillRect(int x, int y, int width, int height)
  {
    fill(new Rectangle(x, y, width, height));
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
    draw(new Polygon(xPoints, yPoints, nPoints));
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

    if( width < 0 || height < 0 )
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

  /**
   * FIXME- support better
   */
  public void setRenderingHint(RenderingHints.Key hintKey, Object hintValue)
  {
    hints.put(hintKey, hintValue);

    if (hintKey.equals(RenderingHints.KEY_INTERPOLATION)
        || hintKey.equals(RenderingHints.KEY_ALPHA_INTERPOLATION))
      {
	if (hintValue.equals(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR))
	  cairoSurfaceSetFilter(0);

	else if (hintValue.equals(RenderingHints.VALUE_INTERPOLATION_BILINEAR))
	  cairoSurfaceSetFilter(1);

	else if (hintValue.equals(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED))
	  cairoSurfaceSetFilter(2);

	else if (hintValue.equals(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY))
	  cairoSurfaceSetFilter(3);

	else if (hintValue.equals(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT))
	  cairoSurfaceSetFilter(4);
      }

    shiftDrawCalls = hints.containsValue(RenderingHints.VALUE_STROKE_NORMALIZE)
      || hints.containsValue(RenderingHints.VALUE_STROKE_DEFAULT);
  }

  public Object getRenderingHint(RenderingHints.Key hintKey)
  {
    return hints.get(hintKey);
  }

  public void setRenderingHints(Map hints)
  {
    this.hints = new RenderingHints(getDefaultHints());
    this.hints.add(new RenderingHints(hints));

    if (hints.containsKey(RenderingHints.KEY_INTERPOLATION))
      {
	if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR))
	  cairoSurfaceSetFilter(0);

	else if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR))
	  cairoSurfaceSetFilter(1);
      }

    if (hints.containsKey(RenderingHints.KEY_ALPHA_INTERPOLATION))
      {
	if (hints.containsValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED))
	  cairoSurfaceSetFilter(2);

	else if (hints.containsValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY))
	  cairoSurfaceSetFilter(3);

	else if (hints.containsValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT))
	  cairoSurfaceSetFilter(4);
      }

    shiftDrawCalls = hints.containsValue(RenderingHints.VALUE_STROKE_NORMALIZE)
      || hints.containsValue(RenderingHints.VALUE_STROKE_DEFAULT);
  }

  public void addRenderingHints(Map hints)
  {
    this.hints.add(new RenderingHints(hints));
  }

  public RenderingHints getRenderingHints()
  {
    return hints;
  }

  ///////////////////////// IMAGE. METHODS ///////////////////////////////////

  protected boolean drawImage(Image img, AffineTransform xform,
                            Color bgcolor, ImageObserver obs)
  {
    if (img == null)
      return false;

    // In this case, xform is an AffineTransform that transforms bounding
    // box of the specified image from image space to user space. However
    // when we pass this transform to cairo, cairo will use this transform
    // to map "user coordinates" to "pixel" coordinates, which is the 
    // other way around. Therefore to get the "user -> pixel" transform 
    // that cairo wants from "image -> user" transform that we currently
    // have, we will need to invert the transformation matrix.
    AffineTransform invertedXform = new AffineTransform();

    try
      {
	invertedXform = xform.createInverse();
      }
    catch (NoninvertibleTransformException e)
      {
	throw new ImagingOpException("Unable to invert transform "
				     + xform.toString());
      }

    // Unrecognized image - convert to a BufferedImage and come back.
    if( !(img instanceof BufferedImage) )
      return this.drawImage(Toolkit.getDefaultToolkit().
			    createImage(img.getSource()),
			    xform, bgcolor, obs);

    BufferedImage b = (BufferedImage) img;
    DataBuffer db;
    double[] i2u = new double[6];
    int width = b.getWidth();
    int height = b.getHeight();

    // If this BufferedImage has a BufferedImageGraphics object, 
    // use the cached CairoSurface that BIG is drawing onto
    if( BufferedImageGraphics.bufferedImages.get( b ) != null )
      db = (DataBuffer)BufferedImageGraphics.bufferedImages.get( b );
    else
      db = b.getRaster().getDataBuffer();

    invertedXform.getMatrix(i2u);

    if(db instanceof CairoSurface)
      {
	((CairoSurface)db).drawSurface(this, i2u);
	return true;
      }
	    
    if( bgcolor != null )
      {
	// Fill a rectangle with the background color 
	// to composite the image onto.
	Paint oldPaint = paint;
	AffineTransform oldTransform = transform;
	setPaint( bgcolor );
	setTransform( invertedXform );
	fillRect(0, 0, width, height);
	setTransform( oldTransform );
	setPaint( oldPaint );
      }

    int[] pixels;

    // Shortcut for easy color models.
    if( b.getColorModel().equals(rgb32) )
      {
	pixels = ((DataBufferInt)db).getData();
	for(int i = 0; i < pixels.length; i++)
	  pixels[i] |= 0xFF000000;
      }
    else if( b.getColorModel().equals(argb32) )
      {
	pixels = ((DataBufferInt)db).getData();
      }
    else
      {
	pixels = b.getRGB(0, 0, width, height,
			  null, 0, width);
      }

    drawPixels(pixels, width, height, width, i2u);

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
    Image filtered = op.filter(image, null);
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
    
    setClip( cx, cy, cw, ch );

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

  ///////////////////////// TEXT METHODS ////////////////////////////////////

  public void drawString(String str, float x, float y)
  {
    if (str == null || str.length() == 0)
      return;

    drawGlyphVector(getFont().createGlyphVector(null, str), x, y);
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
    int n = gv.getNumGlyphs ();
    int[] codes = gv.getGlyphCodes (0, n, null);
    float[] positions = gv.getGlyphPositions (0, n, null);
    
    setFont (gv.getFont ());
    cairoDrawGlyphVector( (GdkFontPeer)getFont().getPeer(), x, y, n, codes, positions);
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
    // the reason we go via the toolkit here is to try to get
    // a cached object. the toolkit keeps such a cache.
    return Toolkit.getDefaultToolkit().getFontMetrics(f);
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

    drawPixels(pixels, r.getWidth(), r.getHeight(), r.getWidth(), i2u);

    // Cairo seems to lose the current color which must be restored.
    updateColor();
    
    return true;
  }

  /**
   * Shifts coordinates by 0.5.
   */
  private double shifted(double coord, boolean doShift)
  {
    if (doShift)
      return Math.floor(coord) + 0.5;
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

    cairoSetFillRule(p.getWindingRule());
    for (; ! p.isDone(); p.next())
      {
	int seg = p.currentSegment(coords);
	switch (seg)
	  {
	  case PathIterator.SEG_MOVETO:
	    x = shifted(coords[0], doShift);
	    y = shifted(coords[1], doShift);
	    cairoMoveTo(x, y);
	    break;
	  case PathIterator.SEG_LINETO:
	    x = shifted(coords[0], doShift);
	    y = shifted(coords[1], doShift);
	    cairoLineTo(x, y);
	    break;
	  case PathIterator.SEG_QUADTO:
	    // splitting a quadratic bezier into a cubic:
	    // see: http://pfaedit.sourceforge.net/bezier.html
	    double x1 = x + (2.0 / 3.0) * (shifted(coords[0], doShift) - x);
	    double y1 = y + (2.0 / 3.0) * (shifted(coords[1], doShift) - y);

	    double x2 = x1 + (1.0 / 3.0) * (shifted(coords[2], doShift) - x);
	    double y2 = y1 + (1.0 / 3.0) * (shifted(coords[3], doShift) - y);

	    x = shifted(coords[2], doShift);
	    y = shifted(coords[3], doShift);
	    cairoCurveTo(x1, y1, x2, y2, x, y);
	    break;
	  case PathIterator.SEG_CUBICTO:
	    x = shifted(coords[4], doShift);
	    y = shifted(coords[5], doShift);
	    cairoCurveTo(shifted(coords[0], doShift),
	                 shifted(coords[1], doShift),
	                 shifted(coords[2], doShift),
	                 shifted(coords[3], doShift), x, y);
	    break;
	  case PathIterator.SEG_CLOSE:
	    cairoClosePath();
	    break;
	  }
      }
  }

  /**
   * Used by setRenderingHints()
   */
  private Map getDefaultHints()
  {
    HashMap defaultHints = new HashMap();

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
}
