/* GdkGraphics2D.java --
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import gnu.classpath.Configuration;
import gnu.java.awt.ClasspathToolkit;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
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
import java.awt.TexturePaint;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
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

public class GdkGraphics2D extends Graphics2D
{
  //////////////////////////////////////
  ////// State Management Methods //////
  //////////////////////////////////////

  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      System.loadLibrary("gtkpeer");

    if (GtkToolkit.useGraphics2D())
      initStaticState();
  }
  
  static native void initStaticState();
  
  private final int native_state = GtkGenericPeer.getUniqueInteger();  

  private Paint paint;
  private Stroke stroke;
  private Color fg;
  private Color bg;
  private Shape clip;
  private AffineTransform transform;
  private GtkComponentPeer component;
  private Font font;
  private RenderingHints hints;
  private BufferedImage bimage;
  private boolean pixelConversionRequired;
  private int[] pixelBuffer;
  private Composite comp;
  private Stack stateStack;

  private native void initState(GtkComponentPeer component);
  private native void initState(int width, int height);
  private native void initState(int[] pixes, int width, int height);
  private native void copyState(GdkGraphics2D g);
  public native void dispose();
  private native void cairoSurfaceSetFilter(int filter);
  native void connectSignals(GtkComponentPeer component);

  public void finalize()
  {
    dispose();
  }

  public Graphics create()
  {
    return new GdkGraphics2D(this);
  }

  public Graphics create(int x, int y, int width, int height)
  {
    return new GdkGraphics2D(width, height);
  }

  GdkGraphics2D(GdkGraphics2D g)
  {
    paint = g.paint;
    stroke = g.stroke;
    setRenderingHints(g.hints);

    if (g.fg.getAlpha() != -1)
      fg = new Color(g.fg.getRed(), g.fg.getGreen(), g.fg.getBlue(),
                     g.fg.getAlpha());
    else
      fg = new Color(g.fg.getRGB());

    if (g.bg.getAlpha() != -1)
      bg = new Color(g.bg.getRed(), g.bg.getGreen(), g.bg.getBlue(),
                     g.bg.getAlpha());
    else
      bg = new Color(g.bg.getRGB());

    if (g.clip == null)
      clip = null;
    else
      clip = new Rectangle(g.getClipBounds());

    if (g.transform == null)
      transform = new AffineTransform();
    else
      transform = new AffineTransform(g.transform);

    font = g.font;
    component = g.component;
    copyState(g);

    setColor(fg);
    setBackground(bg);
    setPaint(paint);
    setStroke(stroke);
    setTransform(transform);
    setClip(clip);
    stateStack = new Stack();
  }

  GdkGraphics2D(int width, int height)
  {
    initState(width, height);

    setColor(Color.black);
    setBackground(Color.black);
    setPaint(getColor());
    setFont(new Font("SansSerif", Font.PLAIN, 12));
    setTransform(new AffineTransform());
    setStroke(new BasicStroke());
    setRenderingHints(getDefaultHints());

    stateStack = new Stack();
  }

  GdkGraphics2D(GtkComponentPeer component)
  {
    this.component = component;
    
    if (component.isRealized())
      initComponentGraphics2D();
    else
      connectSignals(component);
  }

  void initComponentGraphics2D()
  {
    initState(component);

    setColor(component.awtComponent.getForeground());
    setBackground(component.awtComponent.getBackground());
    setPaint(getColor());
    setTransform(new AffineTransform());
    setStroke(new BasicStroke());
    setRenderingHints(getDefaultHints());
    setFont(new Font("SansSerif", Font.PLAIN, 12));

    stateStack = new Stack();
  }

  GdkGraphics2D(BufferedImage bimage)
  {
    this.bimage = bimage;
    this.pixelBuffer = findSimpleIntegerArray(bimage.getColorModel(),
                                              bimage.getRaster());
    if (this.pixelBuffer == null)
      {
	this.pixelBuffer = new int[bimage.getRaster().getWidth() * bimage.getRaster()
	                                                                 .getHeight()];
	this.pixelConversionRequired = true;
      }
    else
      {
        this.pixelConversionRequired = false;
      }

    initState(this.pixelBuffer, bimage.getWidth(), bimage.getHeight());

    setColor(Color.black);
    setBackground(Color.black);
    setPaint(getColor());
    setFont(new Font("SansSerif", Font.PLAIN, 12));
    setTransform(new AffineTransform());
    setStroke(new BasicStroke());
    setRenderingHints(getDefaultHints());

    stateStack = new Stack();

    // draw current buffered image to the pixmap associated 
    // with it, if the image is not equal to our paint buffer.
    if (pixelConversionRequired)
      drawImage(bimage, new AffineTransform(1, 0, 0, 1, 0, 0), bg, null);
  }

  ////////////////////////////////////
  ////// Native Drawing Methods //////
  ////////////////////////////////////

  // GDK drawing methods
  private native void gdkDrawDrawable(GdkGraphics2D other, int x, int y);

  // drawing utility methods
  private native void drawPixels(int[] pixels, int w, int h, int stride,
                                 double[] i2u);
  private native void setTexturePixels(int[] pixels, int w, int h, int stride);
  private native void setGradient(double x1, double y1, double x2, double y2,
                                  int r1, int g1, int b1, int a1, int r2,
                                  int g2, int b2, int a2, boolean cyclic);

  // simple passthroughs to cairo
  private native void cairoSave();
  private native void cairoRestore();
  private native void cairoSetMatrix(double[] m);
  private native void cairoSetOperator(int cairoOperator);
  private native void cairoSetRGBColor(double red, double green, double blue);
  private native void cairoSetAlpha(double alpha);
  private native void cairoSetFillRule(int cairoFillRule);
  private native void cairoSetLineWidth(double width);
  private native void cairoSetLineCap(int cairoLineCap);
  private native void cairoSetLineJoin(int cairoLineJoin);
  private native void cairoSetDash(double[] dashes, int ndash, double offset);

  private native void cairoSetMiterLimit(double limit);
  private native void cairoNewPath();
  private native void cairoMoveTo(double x, double y);
  private native void cairoLineTo(double x, double y);
  private native void cairoCurveTo(double x1, double y1, double x2, double y2,
                                   double x3, double y3);
  private native void cairoRelMoveTo(double dx, double dy);
  private native void cairoRelLineTo(double dx, double dy);
  private native void cairoRelCurveTo(double dx1, double dy1, double dx2,
                                      double dy2, double dx3, double dy3);
  private native void cairoRectangle(double x, double y, double width,
                                     double height);
  private native void cairoClosePath();
  private native void cairoStroke();
  private native void cairoFill();
  private native void cairoClip();

  /////////////////////////////////////////////
  ////// General Drawing Support Methods //////
  /////////////////////////////////////////////

  private class DrawState
  {
    private Paint paint;
    private Stroke stroke;
    private Color fg;
    private Color bg;
    private Shape clip;
    private AffineTransform transform;
    private Font font;
    private Composite comp;

    DrawState(GdkGraphics2D g)
    {
      this.paint = g.paint;
      this.stroke = g.stroke;
      this.fg = g.fg;
      this.bg = g.bg;
      this.clip = g.clip;
      if (g.transform != null)
	this.transform = (AffineTransform) g.transform.clone();
      this.font = g.font;
      this.comp = g.comp;
    }

    public void restore(GdkGraphics2D g)
    {
      g.paint = this.paint;
      g.stroke = this.stroke;
      g.fg = this.fg;
      g.bg = this.bg;
      g.clip = this.clip;
      g.transform = this.transform;
      g.font = this.font;
      g.comp = this.comp;
    }
  }

  private void stateSave()
  {
    stateStack.push(new DrawState(this));
    cairoSave();
  }

  private void stateRestore()
  {
    ((DrawState) (stateStack.pop())).restore(this);
    cairoRestore();
  }

  // Some operations (drawing rather than filling) require that their
  // coords be shifted to land on 0.5-pixel boundaries, in order to land on
  // "middle of pixel" coordinates and light up complete pixels.
  private boolean shiftDrawCalls = false;

  private double shifted(double coord, boolean doShift)
  {
    if (doShift)
      return Math.floor(coord) + 0.5;
    else
      return coord;
  }

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

  private void updateBufferedImage()
  {
    if (bimage != null && pixelConversionRequired)
      {
        int height = bimage.getHeight();
        int width = bimage.getWidth();
        for (int y = 0; y < height; ++y)
          for (int x = 0; x < width; ++x)
            bimage.setRGB(x, y, pixelBuffer[y*width+height]);
      }
  }

  private boolean drawImage(Image img, AffineTransform xform,
                            Color bgcolor, ImageObserver obs)
  {
    if (img == null)
      return false;

    if (img instanceof GtkOffScreenImage
        && img.getGraphics() instanceof GdkGraphics2D
        && (xform == null || xform.getType() == AffineTransform.TYPE_IDENTITY
        || xform.getType() == AffineTransform.TYPE_TRANSLATION))
      {
	// we are being asked to flush a double buffer from Gdk
	GdkGraphics2D g2 = (GdkGraphics2D) img.getGraphics();
	gdkDrawDrawable(g2, (int) xform.getTranslateX(),
	                (int) xform.getTranslateY());

	updateBufferedImage();

	return true;
      }
    else
      {
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
	    if (img instanceof BufferedImage)
	      {
		// draw an image which has actually been loaded 
		// into memory fully
		BufferedImage b = (BufferedImage) img;
		return drawRaster(b.getColorModel(), b.getData(),
		                  invertedXform, bgcolor);
	      }
	    else
	      return this.drawImage(GdkPixbufDecoder.createBufferedImage(img
	                                                                 .getSource()),
	                            xform, bgcolor, obs);
	  }
	catch (NoninvertibleTransformException e)
	  {
	    throw new ImagingOpException("Unable to invert transform "
	                                 + xform.toString());
	  }
      }
  }

  //////////////////////////////////////////////////
  ////// Implementation of Graphics2D Methods //////
  //////////////////////////////////////////////////

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

    updateBufferedImage();
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

    updateBufferedImage();
  }

  public void clip(Shape s)
  {
    // update it
    if (clip == null || s == null)
      clip = s;
    else if (s instanceof Rectangle2D && clip instanceof Rectangle2D)
      {
	Rectangle2D r = (Rectangle2D) s;
	Rectangle2D curr = (Rectangle2D) clip;
	clip = curr.createIntersection(r);
      }
    else
      throw new UnsupportedOperationException();

    // draw it
    if (clip != null)
      {
	cairoNewPath();
	if (clip instanceof Rectangle2D)
	  {
	    Rectangle2D r = (Rectangle2D) clip;
	    cairoRectangle(r.getX(), r.getY(), r.getWidth(), r.getHeight());
	  }
	else
	  walkPath(clip.getPathIterator(null), false);

	// cairoClosePath ();
	cairoClip();
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

	double scaleX = width / (double) img.getWidth();
	double scaleY = width / (double) img.getHeight();

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

  public void translate(double tx, double ty)
  {
    transform(AffineTransform.getTranslateInstance(tx, ty));
  }

  public void translate(int x, int y)
  {
    translate((double) x, (double) y);
  }

  public void shear(double shearX, double shearY)
  {
    transform(AffineTransform.getShearInstance(shearX, shearY));
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
	cairoSetLineCap(bs.getEndCap());
	cairoSetLineWidth(bs.getLineWidth());
	cairoSetLineJoin(bs.getLineJoin());
	cairoSetMiterLimit(bs.getMiterLimit());
	float[] dashes = bs.getDashArray();
	if (dashes != null)
	  {
	    double[] double_dashes = new double[dashes.length];
	    for (int i = 0; i < dashes.length; i++)
	      double_dashes[i] = dashes[i];
	    cairoSetDash(double_dashes, double_dashes.length,
	                 (double) bs.getDashPhase());
	  }
      }
  }

  ////////////////////////////////////////////////
  ////// Implementation of Graphics Methods //////
  ////////////////////////////////////////////////

  public void setPaintMode()
  {
    setComposite(java.awt.AlphaComposite.SrcOver);
  }

  public void setXORMode(Color c)
  {
    setComposite(new gnu.java.awt.BitwiseXORComposite(c));
  }

  public void setColor(Color c)
  {
    if (c == null)
      c = Color.BLACK;

    fg = c;
    paint = c;
    cairoSetRGBColor(fg.getRed() / 255.0, fg.getGreen() / 255.0,
                     fg.getBlue() / 255.0);
    cairoSetAlpha((fg.getAlpha() & 255) / 255.0);
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
    setClip(new Rectangle2D.Double((double) x, (double) y, (double) width,
                                   (double) height));
  }

  public void setClip(Shape s)
  {
    clip = s;
    if (s != null)
      {
	cairoNewPath();
	if (s instanceof Rectangle2D)
	  {
	    Rectangle2D r = (Rectangle2D) s;
	    cairoRectangle(r.getX(), r.getY(), r.getWidth(), r.getHeight());
	  }
	else
	  walkPath(s.getPathIterator(null), false);

	// cairoClosePath ();
	cairoClip();
      }
  }

  private static BasicStroke draw3DRectStroke = new BasicStroke();

  public void draw3DRect(int x, int y, int width, int height, boolean raised)
  {
    Stroke tmp = stroke;
    setStroke(draw3DRectStroke);
    super.draw3DRect(x, y, width, height, raised);
    setStroke(tmp);
    updateBufferedImage();
  }

  public void fill3DRect(int x, int y, int width, int height, boolean raised)
  {
    Stroke tmp = stroke;
    setStroke(draw3DRectStroke);
    super.fill3DRect(x, y, width, height, raised);
    setStroke(tmp);
    updateBufferedImage();
  }

  public void drawRect(int x, int y, int width, int height)
  {
    draw(new Rectangle(x, y, width, height));
  }

  public void fillRect(int x, int y, int width, int height)
  {
    cairoNewPath();
    cairoRectangle(x, y, width, height);
    cairoFill();
  }

  public void clearRect(int x, int y, int width, int height)
  {
    cairoSetRGBColor(bg.getRed() / 255.0, bg.getGreen() / 255.0,
                     bg.getBlue() / 255.0);
    cairoSetAlpha(1.0);
    cairoNewPath();
    cairoRectangle(x, y, width, height);
    cairoFill();
    setColor(fg);

    updateBufferedImage();
  }

  public void setBackground(Color c)
  {
    bg = c;
  }

  public Color getBackground()
  {
    return bg;
  }

  private void doPolygon(int[] xPoints, int[] yPoints, int nPoints,
                         boolean close, boolean fill)
  {
    if (nPoints < 1)
      return;
    GeneralPath gp = new GeneralPath(PathIterator.WIND_EVEN_ODD);
    gp.moveTo((float) xPoints[0], (float) yPoints[0]);
    for (int i = 1; i < nPoints; i++)
      gp.lineTo((float) xPoints[i], (float) yPoints[i]);

    if (close)
      gp.closePath();

    Shape sh = gp;
    if (fill == false && stroke != null && ! (stroke instanceof BasicStroke))
      {
	sh = stroke.createStrokedShape(gp);
	fill = true;
      }

    if (fill)
      fill(sh);
    else
      draw(sh);
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    int[] xp = new int[2];
    int[] yp = new int[2];

    xp[0] = x1;
    xp[1] = x2;
    yp[0] = y1;
    yp[1] = y2;

    doPolygon(xp, yp, 2, false, false);
  }

  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    doPolygon(xPoints, yPoints, nPoints, true, true);
  }

  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    doPolygon(xPoints, yPoints, nPoints, true, false);
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints)
  {
    doPolygon(xPoints, yPoints, nPoints, false, false);
  }

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

    updateBufferedImage();

    return true;
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    drawRaster(image.getColorModel(), image.getData(), xform, bg);
  }

  public void drawRenderableImage(RenderableImage image, AffineTransform xform)
  {
    drawRenderedImage(image.createRendering(new RenderContext(xform)), xform);
  }

  public boolean drawImage(Image img, AffineTransform xform, ImageObserver obs)
  {
    return drawImage(img, xform, bg, obs);
  }

  public void drawImage(BufferedImage image, BufferedImageOp op, int x, int y)
  {
    Image filtered = op.filter(image, null);
    drawImage(filtered, new AffineTransform(1f, 0f, 0f, 1f, x, y), bg, null);
  }

  public boolean drawImage(Image img, int x, int y, ImageObserver observer)
  {
    return drawImage(img, new AffineTransform(1f, 0f, 0f, 1f, x, y), bg,
                     observer);
  }

  ///////////////////////////////////////////////
  ////// Unimplemented Stubs and Overloads //////
  ///////////////////////////////////////////////

  public boolean hit(Rectangle rect, Shape text, boolean onStroke)
  {
    throw new java.lang.UnsupportedOperationException();
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    throw new java.lang.UnsupportedOperationException();
  }

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
      throw new java.lang.UnsupportedOperationException();
  }

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

  public Composite getComposite()
  {
    if (comp == null)
      return AlphaComposite.SrcOver;
    else
      return comp;
  }

  public FontRenderContext getFontRenderContext()
  {
    return new FontRenderContext(transform, true, true);
  }

  public void copyArea(int x, int y, int width, int height, int dx, int dy)
  {
    throw new java.lang.UnsupportedOperationException();
  }

  public void drawArc(int x, int y, int width, int height, int startAngle,
                      int arcAngle)
  {
    draw(new Arc2D.Double((double) x, (double) y, (double) width,
                          (double) height, (double) startAngle,
                          (double) arcAngle, Arc2D.OPEN));
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

    return drawImage(img, new AffineTransform(scaleX, 0f, 0f, scaleY, x, y),
                     bgcolor, observer);
  }

  public boolean drawImage(Image img, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    return drawImage(img, x, y, width, height, bg, observer);
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2, Color bgcolor,
                           ImageObserver observer)
  {
    if (img == null)
      return false;

    Image subImage;

    int sourceWidth = sx2 - sx1;
    int sourceHeight = sy2 - sy1;

    int destWidth = dx2 - dx1;
    int destHeight = dy2 - dy1;

    double scaleX = destWidth / (double) sourceWidth;
    double scaleY = destHeight / (double) sourceHeight;

    // Get the subimage of the source enclosed in the 
    // rectangle specified by sx1, sy1, sx2, sy2
	
    if (img instanceof BufferedImage)
      {
	BufferedImage b = (BufferedImage) img;
	subImage = b.getSubimage(sx1, sy1, sx2, sy2);
      }
    else
      {
	// FIXME: This code currently doesn't work. Null Pointer 
	// exception is thrown in this case. This happens 
	// because img.getSource() always returns null, since source gets 
	// never initialized when it is created with the help of 
	// createImage(int width, int height). 
	CropImageFilter filter = new CropImageFilter(sx1, sx2, sx2, sy2);
	FilteredImageSource src = new FilteredImageSource(img.getSource(),
	                                                  filter);

	subImage = Toolkit.getDefaultToolkit().createImage(src);
      }

    return drawImage(subImage,
                     new AffineTransform(scaleX, 0, 0, scaleY, dx1, dy1),
                     bgcolor, observer);
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           ImageObserver observer)
  {
    return drawImage(img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, bg, observer);
  }

  public void drawOval(int x, int y, int width, int height)
  {
    drawArc(x, y, width, height, 0, 360);
  }

  public void drawRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    if (arcWidth > width)
      arcWidth = width;
    if (arcHeight > height)
      arcHeight = height;

    int xx = x + width - arcWidth;
    int yy = y + height - arcHeight;

    drawArc(x, y, arcWidth, arcHeight, 90, 90);
    drawArc(xx, y, arcWidth, arcHeight, 0, 90);
    drawArc(xx, yy, arcWidth, arcHeight, 270, 90);
    drawArc(x, yy, arcWidth, arcHeight, 180, 90);

    int y1 = y + arcHeight / 2;
    int y2 = y + height - arcHeight / 2;
    drawLine(x, y1, x, y2);
    drawLine(x + width, y1, x + width, y2);

    int x1 = x + arcWidth / 2;
    int x2 = x + width - arcWidth / 2;
    drawLine(x1, y, x2, y);
    drawLine(x1, y + height, x2, y + height);
  }

  // these are the most accelerated painting paths
  native void cairoDrawGlyphVector(GdkFontPeer font, 
                                   float x, float y, int n, 
                                   int[] codes, float[] positions);

  native void cairoDrawGdkTextLayout(GdkTextLayout gl, 
                                     float x, float y);

  GdkFontPeer getFontPeer()
  {
    return (GdkFontPeer) getFont().getPeer();
  }

  public void drawGdkTextLayout(GdkTextLayout gl, float x, float y)
  {
    cairoDrawGdkTextLayout (gl, x, y);
    updateBufferedImage ();
  }

  public void drawString(String str, float x, float y)
  {
    drawGlyphVector(getFont().createGlyphVector(null, str), x, y);
    updateBufferedImage ();
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
    cairoDrawGlyphVector (getFontPeer(), x, y, n, codes, positions);
    updateBufferedImage ();
  }

  public void drawString(AttributedCharacterIterator ci, float x, float y)
  {
    GlyphVector gv = getFont().createGlyphVector(getFontRenderContext(), ci);
    drawGlyphVector(gv, x, y);
  }

  public void fillArc(int x, int y, int width, int height, int startAngle,
                      int arcAngle)
  {
    fill(new Arc2D.Double((double) x, (double) y, (double) width,
                          (double) height, (double) startAngle,
                          (double) arcAngle, Arc2D.OPEN));
  }

  public void fillOval(int x, int y, int width, int height)
  {
    fillArc(x, y, width, height, 0, 360);
  }

  public void fillRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    if (arcWidth > width)
      arcWidth = width;
    if (arcHeight > height)
      arcHeight = height;

    int xx = x + width - arcWidth;
    int yy = y + height - arcHeight;

    fillArc(x, y, arcWidth, arcHeight, 90, 90);
    fillArc(xx, y, arcWidth, arcHeight, 0, 90);
    fillArc(xx, yy, arcWidth, arcHeight, 270, 90);
    fillArc(x, yy, arcWidth, arcHeight, 180, 90);

    fillRect(x, y + arcHeight / 2, width, height - arcHeight + 1);
    fillRect(x + arcWidth / 2, y, width - arcWidth + 1, height);
  }

  public Font getFont()
  {
    if (font == null)
      return new Font("SansSerif", Font.PLAIN, 12);
    return font;
  }

  // Until such time as pango is happy to talk directly to cairo, we
  // actually need to redirect some calls from the GtkFontPeer and
  // GtkFontMetrics into the drawing kit and ask cairo ourselves.

  static native void releasePeerGraphicsResource(GdkFontPeer f);

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
    if (f.getPeer() instanceof GdkFontPeer)
      font = f;
    else
      font = 
        ((ClasspathToolkit)(Toolkit.getDefaultToolkit()))
        .getFont(f.getName(), f.getAttributes());    
  }

  public String toString()
  {
    return  (getClass().getName()
             + "[font=" + getFont().toString()
             + ",color=" + fg.toString()
	     + "]");
  }
}
