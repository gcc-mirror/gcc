/* GdkGraphics2D.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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

import java.awt.*;
import java.awt.geom.*;
import java.awt.font.*;
import java.awt.color.*;
import java.awt.image.*;
import java.awt.image.renderable.*;
import java.util.HashMap;
import java.util.Map;

import java.text.AttributedCharacterIterator;
import java.util.Map;
import java.util.Stack;
import java.lang.Integer;
import gnu.java.awt.ClasspathToolkit;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.classpath.Configuration;

public class GdkGraphics2D extends Graphics2D
{

  //////////////////////////////////////
  ////// State Management Methods //////
  //////////////////////////////////////

  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("gtkpeer");
      }

    if (GtkToolkit.useGraphics2D ())
      initStaticState ();
  }
  native static void initStaticState ();
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

  private Stack stateStack;
  
  native private int[] initState (GtkComponentPeer component);
  native private void initState (int width, int height);
  native private void copyState (GdkGraphics2D g);
  native public void dispose ();

  public void finalize ()
  {
    dispose();
  }

  public Graphics create ()
  {
    return new GdkGraphics2D (this);
  }

  public Graphics create (int x, int y, int width, int height)
  {
    return new GdkGraphics2D (width, height);
  }

  GdkGraphics2D (GdkGraphics2D g)
  {
    paint = g.paint;
    stroke = g.stroke;
    hints = g.hints;

    if (g.fg.getAlpha() != -1)
      fg = new Color (g.fg.getRed (), g.fg.getGreen (), 
                      g.fg.getBlue (), g.fg.getAlpha ());
    else 
      fg = new Color (g.fg.getRGB ());

    if (g.bg.getAlpha() != -1)
      bg = new Color(g.bg.getRed (), g.bg.getGreen (), 
                     g.bg.getBlue (), g.bg.getAlpha ());
    else
      bg = new Color (g.bg.getRGB ());

    if (g.clip == null)
      clip = null;
    else
      clip = new Rectangle (g.getClipBounds ());

    if (g.transform == null)
      transform = new AffineTransform ();
    else
      transform = new AffineTransform (g.transform);

    font = g.font;
    component = g.component;
    copyState (g);

    setColor (fg);
    setBackground (bg);
    setPaint (paint);
    setStroke (stroke);
    setClip (clip);
    setTransform (transform);
    stateStack = new Stack();
  }

  GdkGraphics2D (int width, int height)
  {
    initState (width, height);

    setColor(Color.black);
    setBackground (Color.black);
    setPaint (getColor());
    setFont (new Font("SansSerif", Font.PLAIN, 12));
    setTransform (new AffineTransform ());
    setStroke (new BasicStroke ());
    setRenderingHints (new HashMap ());

    stateStack = new Stack();
  }

  GdkGraphics2D (GtkComponentPeer component)
  {
    this.component = component;
    int rgb[] = initState (component);

    setColor (new Color (rgb[0], rgb[1], rgb[2]));
    setBackground (new Color (rgb[3], rgb[4], rgb[5]));
    setPaint (getColor());
    setFont (new Font("SansSerif", Font.PLAIN, 12));
    setTransform (new AffineTransform ());
    setStroke (new BasicStroke ());
    setRenderingHints (new HashMap ());

    stateStack = new Stack ();
  }


  ////////////////////////////////////
  ////// Native Drawing Methods //////
  ////////////////////////////////////

  // GDK drawing methods
  private native void gdkDrawDrawable (GdkGraphics2D other, int x, int y);

  // drawing utility methods
  private native void drawPixels (int pixels[], int w, int h, int stride, double i2u[]);
  private native void setTexturePixels (int pixels[], int w, int h, int stride);
  private native void setGradient (double x1, double y1,
                                   double x2, double y2,
                                   int r1, int g1, int b1, int a1,
                                   int r2, int g2, int b2, int a2,
                                   boolean cyclic);

  // simple passthroughs to cairo
  private native void cairoSave ();
  private native void cairoRestore ();
  private native void cairoSetMatrix (double m[]);
  private native void cairoSetFont (GdkClasspathFontPeer peer);
  private native void cairoShowGlyphs (int codes[],
                                       float positions[]);
  private native void cairoSetOperator (int cairoOperator);
  private native void cairoSetRGBColor (double red, double green, double blue);
  private native void cairoSetAlpha (double alpha);
  private native void cairoSetFillRule (int cairoFillRule);
  private native void cairoSetLineWidth (double width);
  private native void cairoSetLineCap (int cairoLineCap);
  private native void cairoSetLineJoin (int cairoLineJoin);
  private native void cairoSetDash (double dashes[], int ndash, double offset);
  private native void cairoSetMiterLimit (double limit);
  private native void cairoNewPath ();
  private native void cairoMoveTo (double x, double y);
  private native void cairoLineTo (double x, double y);
  private native void cairoCurveTo (double x1, double y1,
                                    double x2, double y2,
                                    double x3, double y3);  
  private native void cairoRelMoveTo (double dx, double dy);
  private native void cairoRelLineTo (double dx, double dy);
  private native void cairoRelCurveTo (double dx1, double dy1,
                                       double dx2, double dy2,
                                       double dx3, double dy3);
  private native void cairoRectangle (double x, double y, 
                                   double width, double height);
  private native void cairoClosePath ();
  private native void cairoStroke ();
  private native void cairoFill ();
  private native void cairoClip ();


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
	DrawState (GdkGraphics2D g)
	{
	    this.paint = g.paint;
	    this.stroke = g.stroke;
	    this.fg = g.fg;
	    this.bg = g.bg;
	    this.clip = g.clip;
	    if (g.transform != null)
		this.transform = (AffineTransform) g.transform.clone();
	    this.font = g.font;
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
	}
    }
    
    private void stateSave ()
    {
	stateStack.push (new DrawState (this));
	cairoSave ();
    }

    private void stateRestore ()
    {
	((DrawState)(stateStack.pop ())).restore (this);
	cairoRestore ();
    }


  double x;
  double y;
  private void setPos (double nx, double ny)
  {
    x = nx;
    y = ny;
  }

  private void walkPath(PathIterator p)
  {
    double coords[] = new double[6];

    cairoSetFillRule (p.getWindingRule ());
    for ( ; ! p.isDone (); p.next())
      {
        int seg = p.currentSegment (coords);
        switch(seg)
          {

          case PathIterator.SEG_MOVETO:
            setPos(coords[0], coords[1]);
            cairoMoveTo (coords[0], coords[1]);
            break;

          case PathIterator.SEG_LINETO:
            setPos(coords[0], coords[1]);
            cairoLineTo (coords[0], coords[1]);
            break;

          case PathIterator.SEG_QUADTO:

            // splitting a quadratic bezier into a cubic:
            // see: http://pfaedit.sourceforge.net/bezier.html

            double x1 = x + (2.0/3.0) * (coords[0] - x);
            double y1 = y + (2.0/3.0) * (coords[1] - y);
            
            double x2 = x1 + (1.0/3.0) * (coords[2] - x);
            double y2 = y1 + (1.0/3.0) * (coords[3] - y);

            setPos(coords[2], coords[3]);
            cairoCurveTo (x1, y1,
                          x2, y2,
                          coords[2], coords[3]);
            break;

          case PathIterator.SEG_CUBICTO:
            setPos(coords[4], coords[5]);
            cairoCurveTo (coords[0], coords[1],
                          coords[2], coords[3],
                          coords[4], coords[5]);
            break;

          case PathIterator.SEG_CLOSE:
            cairoClosePath ();
            break;
          }
      }    
  }


  private Map getDefaultHints()
  {
    HashMap defaultHints = new HashMap ();
    
    defaultHints.put (RenderingHints.KEY_TEXT_ANTIALIASING, 
                      RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT);
		      
    defaultHints.put (RenderingHints.KEY_STROKE_CONTROL, 
                      RenderingHints.VALUE_STROKE_DEFAULT);    
		      
    defaultHints.put (RenderingHints.KEY_FRACTIONALMETRICS, 
                      RenderingHints.VALUE_FRACTIONALMETRICS_OFF);    
		      
    defaultHints.put (RenderingHints.KEY_ANTIALIASING, 
                      RenderingHints.VALUE_ANTIALIAS_OFF);    
		      
    defaultHints.put (RenderingHints.KEY_RENDERING,  
                      RenderingHints.VALUE_RENDER_DEFAULT);
    
    return defaultHints;
    
  }

  //////////////////////////////////////////////////
  ////// Implementation of Graphics2D Methods //////
  //////////////////////////////////////////////////

  public void draw (Shape s)
  {

    if (stroke != null &&
        !(stroke instanceof BasicStroke))
      {
        fill (stroke.createStrokedShape (s));
        return;
      }

    stateSave ();
    cairoNewPath ();

    boolean normalize;
    normalize = hints.containsValue (RenderingHints.VALUE_STROKE_NORMALIZE)
                || hints.containsValue (RenderingHints.VALUE_STROKE_DEFAULT);

    if (normalize)
      translate (0.5,0.5);      
    
    if (s instanceof Rectangle2D)
      {
        Rectangle2D r = (Rectangle2D)s;
        cairoRectangle (r.getX (), r.getY (), r.getWidth (), r.getHeight ());
      }
    else      
      walkPath (s.getPathIterator (null));
    cairoStroke ();
    
    if (normalize)
      translate (-0.5,-0.5);
      
    stateRestore ();
  }

  public void fill (Shape s)
  {
    stateSave();
    cairoNewPath ();
    if (s instanceof Rectangle2D)
      {
        Rectangle2D r = (Rectangle2D)s;
        cairoRectangle (r.getX (), r.getY (), r.getWidth (), r.getHeight ());
      }
    else      
      walkPath (s.getPathIterator (null));
    cairoFill ();
    stateRestore ();
  }

  public void clip (Shape s)
  {
      // update it

      if (clip == null || s == null)
	  clip = s;
      else if (s instanceof Rectangle2D
	       && clip instanceof Rectangle2D)
	  {
	      Rectangle2D r = (Rectangle2D)s;
	      Rectangle2D curr = (Rectangle2D)clip;
	      clip = curr.createIntersection (r);
	  }
      else
	  throw new UnsupportedOperationException ();

      // draw it
      if (clip != null)
	  {
	      cairoNewPath ();
	      if (clip instanceof Rectangle2D)
		  {
		      Rectangle2D r = (Rectangle2D)clip;
		      cairoRectangle (r.getX (), r.getY (), 
				      r.getWidth (), r.getHeight ());
		  }
	      else
		  walkPath (clip.getPathIterator (null));
	      cairoClosePath ();
	      cairoClip ();
	  }
  }

  public Paint getPaint ()
  {
    return paint;
  }

  public AffineTransform getTransform ()
  {
      return (AffineTransform) transform.clone ();
  }

  public void setPaint (Paint p)
  {
    if (paint == null)
        return;
      
    paint = p;
    if (paint instanceof Color)
      {
        setColor ((Color) paint);
      }
    else if (paint instanceof TexturePaint)
      {
        TexturePaint tp = (TexturePaint) paint;
        BufferedImage img = tp.getImage ();
        int pixels[] = img.getRGB(0, 0, img.getWidth (), 
                                  img.getHeight (), null, 
                                  0, img.getWidth ());
        setTexturePixels (pixels, img.getWidth (), 
                          img.getHeight (), img.getWidth ());
      }
    else if (paint instanceof GradientPaint)
      {
        GradientPaint gp = (GradientPaint) paint;
        Point2D p1 = gp.getPoint1 ();
        Point2D p2 = gp.getPoint2 ();
        Color c1 = gp.getColor1 ();
        Color c2 = gp.getColor2 ();        
        setGradient (p1.getX (), p1.getY (),
                     p2.getX (), p2.getY (),
                     c1.getRed (), c1.getGreen (), 
                     c1.getBlue (), c1.getAlpha (),
                     c2.getRed (), c2.getGreen (), 
                     c2.getBlue (), c2.getAlpha (),
                     gp.isCyclic ());
      }
    else
      throw new java.lang.UnsupportedOperationException ();
  }

  public void setTransform (AffineTransform tx)
  {
    transform = tx;
    if (transform != null)
      {
        double m[] = new double[6];
        transform.getMatrix (m);
        cairoSetMatrix (m);
      }
  }

  public void transform (AffineTransform tx)
  {
    if (transform == null)
      transform = new AffineTransform (tx);
    else
      transform.concatenate (tx);
    setTransform (transform);
  }

  public void rotate(double theta)
  {
    transform (AffineTransform.getRotateInstance (theta));
  }

  public void rotate(double theta, double x, double y)
  {
    transform (AffineTransform.getRotateInstance (theta, x, y));
  }

  public void scale(double sx, double sy)
  {
    transform (AffineTransform.getScaleInstance (sx, sy));
  }

  public void translate (double tx, double ty)
  {
    transform (AffineTransform.getTranslateInstance (tx, ty));
  }

  public void translate (int x, int y)
  {
    translate ((double) x, (double) y);
  }

  public void shear(double shearX, double shearY)
  {
    transform (AffineTransform.getShearInstance (shearX, shearY));
  }

  public Stroke getStroke()
  {
    return stroke;
  }

  public void setStroke (Stroke st)
  {
    stroke = st;
    if (stroke instanceof BasicStroke)
      {
        BasicStroke bs = (BasicStroke) stroke;
        cairoSetLineCap (bs.getEndCap());
        cairoSetLineWidth (bs.getLineWidth() / 2.0);
        cairoSetLineJoin (bs.getLineJoin());
        cairoSetMiterLimit (bs.getMiterLimit());
        float dashes[] = bs.getDashArray();
        if (dashes != null)
          {
            double double_dashes[] = new double[dashes.length];
            for (int i = 0; i < dashes.length; i++)
              double_dashes[i] = dashes[i];
            cairoSetDash (double_dashes, double_dashes.length, 
                          (double) bs.getDashPhase ());        
          }
      }
  }


  ////////////////////////////////////////////////
  ////// Implementation of Graphics Methods //////
  ////////////////////////////////////////////////

  public void setPaintMode () 
  { 
    setComposite (java.awt.AlphaComposite.SrcOver); 
  }

  public void setXORMode (Color c) 
  { 
    setComposite (new gnu.java.awt.BitwiseXORComposite(c)); 
  }

  public void setColor (Color c)
  {
    fg = c;
    paint = c;
    cairoSetRGBColor (fg.getRed() / 255.0, 
                      fg.getGreen() / 255.0, 
                      fg.getBlue() / 255.0);
    cairoSetAlpha ((fg.getAlpha() & 255) / 255.0);
  }

  public Color getColor ()
  {
    return fg;
  }

  public void clipRect (int x, int y, int width, int height)
  {
      clip (new Rectangle (x, y, width, height));
  }

  public Shape getClip ()
  {
    return getClipInDevSpace ();
  }

  public Rectangle getClipBounds ()
  {
    if (clip == null)
      return null;
    else
      return clip.getBounds ();
  }

    protected Rectangle2D getClipInDevSpace ()
    {
	Rectangle2D uclip = clip.getBounds2D ();
	if (transform == null)
	    return uclip;
	else
	    {
		Point2D pos = transform.transform (new Point2D.Double(uclip.getX (), 
								      uclip.getY ()), 
						   (Point2D)null);		
		Point2D extent = transform.deltaTransform (new Point2D.Double(uclip.getWidth (), 
									      uclip.getHeight ()), 
							   (Point2D)null);
		return new Rectangle2D.Double (pos.getX (), pos.getY (),
					       extent.getX (), extent.getY ());	      
	    }
    }

  public void setClip (int x, int y, int width, int height)
  {
      cairoNewPath ();
      cairoRectangle (x, y, width, height);
      cairoClosePath ();
      cairoClip ();
      clip = new Rectangle2D.Double ((double)x, (double)y, 
				     (double)width, (double)height);
  }

  public void setClip (Shape s)
  {
    clip (s);
  }

  public void draw3DRect(int x, int y, int width, 
                         int height, boolean raised)
  {
    Color std = fg;
    Color light = std.brighter();
    Color dark = std.darker();

    if (!raised)
      {
        Color t = light;
        light = dark;
        dark = t;
      }
    
    double x1 = (double) x;
    double x2 = (double) x + width;

    double y1 = (double) y;
    double y2 = (double) y + height;

    stateSave ();
    
    cairoNewPath ();
    
    boolean normalize;
    normalize = hints.containsValue (RenderingHints.VALUE_STROKE_NORMALIZE)
                || hints.containsValue (RenderingHints.VALUE_STROKE_DEFAULT);
		
    if (normalize) 
      {
    	x1 += 0.5;
	y1 += 0.5; 
	x2 += 0.5;
	y2 += 0.5; 
      }
    
    setColor (light);
    cairoMoveTo (x1, y1);
    cairoLineTo (x2, y1);
    cairoLineTo (x2, y2);
    cairoStroke ();
    
    cairoNewPath ();
    setColor (dark);
    cairoMoveTo (x1, y1);
    cairoLineTo (x1, y2);
    cairoLineTo (x2, y2);
    cairoStroke ();
    
    stateRestore ();    
  }

  public void fill3DRect(int x, int y, int width, 
                         int height, boolean raised)
  {
    double step = 1.0;
    if (stroke != null && stroke instanceof BasicStroke)
      {
        BasicStroke bs = (BasicStroke) stroke;
        step = bs.getLineWidth();
      }

    Color bright = fg.brighter ();
    Color dark = fg.darker ();
      
    draw3DRect (x, y, width, height, raised);
    
    stateSave ();
    translate (step/2.0, step/2.0);
    cairoNewPath ();
    cairoRectangle ((double) x, (double) y, 
                    ((double) width) - step, 
                    ((double) height) - step );
    cairoClosePath ();
    cairoFill ();
    stateRestore ();
  }


  public void drawRect (int x, int y, int width, int height)
  {
    draw(new Rectangle (x, y, width, height));
  }

  public void fillRect (int x, int y, int width, int height)
  {
    fill(new Rectangle (x, y, width, height));
  }

  public void clearRect (int x, int y, int width, int height)
  {
    stateSave ();
    cairoSetRGBColor (bg.getRed() / 255.0, 
                      bg.getGreen() / 255.0, 
                      bg.getBlue() / 255.0);
    cairoSetAlpha (1.0);
    cairoNewPath ();
    cairoRectangle (x, y, width, height);
    cairoClosePath ();
    cairoFill ();
    stateRestore ();
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
    GeneralPath gp = new GeneralPath (PathIterator.WIND_EVEN_ODD);
    gp.moveTo ((float)xPoints[0], (float)yPoints[0]);
    for (int i = 1; i < nPoints; i++)
      gp.lineTo ((float)xPoints[i], (float)yPoints[i]);
    
    if (close)
      gp.closePath ();

    Shape sh = gp;
    if (fill == false &&
        stroke != null &&
        !(stroke instanceof BasicStroke))
      {
        sh = stroke.createStrokedShape (gp);
        fill = true;
      }
    
    if (fill) 
      fill (sh);
    else 
      draw (sh);
  }

  public void drawLine (int x1, int y1, int x2, int y2)
  {
    int xp[] = new int[2];
    int yp[] = new int[2];

    xp[0] = x1;
    xp[1] = x2;
    yp[0] = y1;
    yp[1] = y2;
    
    doPolygon (xp, yp, 2, false, false);
  }

  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    doPolygon (xPoints, yPoints, nPoints, true, true);
  }
  
  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {    
    doPolygon (xPoints, yPoints, nPoints, true, false);
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints)
  {
    doPolygon (xPoints, yPoints, nPoints, false, false);
  }

  private boolean drawRaster (ColorModel cm, Raster r, 
                              AffineTransform imageToUser)
  {
    if (r == null)
      return false;

    SampleModel sm = r.getSampleModel ();
    DataBuffer db = r.getDataBuffer ();

    if (db == null || sm == null)
      return false;

    if (cm == null)
      cm = ColorModel.getRGBdefault ();

    double[] i2u = new double[6];
    if (imageToUser != null)
      imageToUser.getMatrix(i2u);
    else
      {
        i2u[0] = 1; i2u[1] = 0;
        i2u[2] = 0; i2u[3] = 1;
        i2u[2] = 0; i2u[3] = 0;
      }

    int pixels[] = null;

    if (sm.getDataType () == DataBuffer.TYPE_INT &&
        db instanceof DataBufferInt &&
        db.getNumBanks () == 1)
      {
        // single bank, ARGB-ints buffer in sRGB space
        DataBufferInt dbi = (DataBufferInt)db;
        pixels = dbi.getData ();
      }
    else
      pixels = r.getPixels (0, 0, r.getWidth (), r.getHeight (), pixels);
    
    ColorSpace cs = cm.getColorSpace ();
    if (cs != null && 
        cs.getType () != ColorSpace.CS_sRGB)
      {
        int pixels2[] = new int[pixels.length];        
        for (int i = 0; i < pixels2.length; i++)
          pixels2[i] = cm.getRGB (pixels[i]);        
        pixels = pixels2;
      }
    
    stateSave ();
    translate (x, y);
    drawPixels (pixels, r.getWidth (), r.getHeight (), r.getWidth (), i2u);
    stateRestore ();    
    return true;
  }

  public void drawRenderedImage(RenderedImage image,
                                AffineTransform xform)
  {
    drawRaster (image.getColorModel(), image.getData(), xform);
  }
  
  public void drawRenderableImage(RenderableImage image,
                                  AffineTransform xform)
  {
    drawRenderedImage (image.createRendering (new RenderContext (xform)), xform);
  }
  
  public boolean drawImage(Image img, 
                           AffineTransform xform,
                           ImageObserver obs)
  {
    if (img instanceof GtkOffScreenImage &&
        img.getGraphics () instanceof GdkGraphics2D &&            
        (xform == null 
         || xform.getType () == AffineTransform.TYPE_IDENTITY 
         || xform.getType () == AffineTransform.TYPE_TRANSLATION)
        ) 
      {
        // we are being asked to flush a double buffer from Gdk
        GdkGraphics2D g2 = (GdkGraphics2D) img.getGraphics ();
        gdkDrawDrawable (g2, (int)xform.getTranslateX(), (int)xform.getTranslateY());
        return true;
      }
    else
      {
        if (img instanceof BufferedImage)
          {
            // draw an image which has actually been loaded into memory fully
            BufferedImage b = (BufferedImage) img;
            return drawRaster (b.getColorModel (), b.getData (), xform);
          }        
        else
          {
            // begin progressive loading in a separate thread
            new PainterThread (this, img, xform);
            return false;
          }
      }
  }

  public void drawImage(BufferedImage image,
                        BufferedImageOp op,
                        int x,
                        int y)
  {
    Image filtered = op.filter(image, null);
    drawImage(filtered, new AffineTransform(1f,0f,0f,1f,x,y), null);
  }

  public boolean drawImage (Image img, int x, int y, 
                            ImageObserver observer)
  {
    return drawImage(img, new AffineTransform(1f,0f,0f,1f,x,y), observer);    
  }


  ////////////////////////////////////////
  ////// Supporting Private Classes //////
  ////////////////////////////////////////
  
  private class PainterThread implements Runnable, ImageConsumer
  {

    // this is a helper which is spun off when someone tries to do
    // Graphics2D.drawImage on an image we cannot determine to be either
    // one of our own offscreen images or a BufferedImage; that is, when
    // someone wants to draw an image which is possibly still loading over
    // a network or something. you run it in a separate thread and it
    // writes through to the underlying Graphics2D as pixels becomg
    // available.

    GdkGraphics2D gr;
    Image image;
    ColorModel defaultModel;
    AffineTransform xform;

    public PainterThread (GdkGraphics2D g, Image im, AffineTransform xf)
    {
      image = im;
      xform = xf;
      this.gr = (GdkGraphics2D) g.create ();
      new Thread (this).start ();
    }
    
    public void imageComplete (int status)
    {
    }
    
    public void setColorModel (ColorModel model) 
    {
      defaultModel = model;
    }
    
    public void setDimensions (int width, int height)
    {
    }
    
    public void setHints (int hintflags)
    {
    }
    
    public void setPixels (int x, int y, int w, int h, ColorModel model, 
                           byte[] pixels, int off, int scansize)
    {
    }
    
    public void setPixels (int x, int y, int w, int h, ColorModel model, 
                           int[] pixels, int off, int scansize)
      {
        gr.stateSave ();
        gr.translate (x, y);

        if (model == null)
          model = defaultModel;

        int pixels2[];
        if (model != null)
          {
            pixels2 = new int[pixels.length];
            for (int yy = 0; yy < h; yy++)
              for (int xx = 0; xx < w; xx++)
                {
                  int i = yy * scansize + xx;
                  pixels2[i] = model.getRGB (pixels[i]);
                }
          }
        else
          pixels2 = pixels;

        double[] xf = new double[6];
        xform.getMatrix(xf);        
        gr.drawPixels (pixels2, w, h, scansize, xf);
        gr.stateRestore ();
      }

    public void setProperties (java.util.Hashtable props)
    {
    }
    
    public void run ()
    {
      image.getSource ().startProduction (this);
      gr.dispose ();
    }
  }



  ///////////////////////////////////////////////
  ////// Unimplemented Stubs and Overloads //////
  ///////////////////////////////////////////////

  
    
  public boolean hit(Rectangle rect, Shape text,
                     boolean onStroke)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public void setComposite(Composite comp)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public void setRenderingHint(RenderingHints.Key hintKey,
                               Object hintValue)
  {
    hints.put (hintKey, hintValue);    
  }

  public Object getRenderingHint(RenderingHints.Key hintKey)
  {
    return hints.get (hintKey);
  }
  
  public void setRenderingHints(Map hints)
  {
    this.hints = new RenderingHints (getDefaultHints ());
    this.hints.add (new RenderingHints (hints));
  }

  public void addRenderingHints(Map hints)
  {
    this.hints.add (new RenderingHints (hints));
  }

  public RenderingHints getRenderingHints()
  {
    return hints;
  }

  public Composite getComposite()
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public FontRenderContext getFontRenderContext ()
  {
    return new FontRenderContext (transform, true, true);
  }

  public void drawGlyphVector (GlyphVector g, float x, float y)
  {    
    stateSave ();
    setFont (g.getFont ());
    translate ((double)x, (double)y);
    cairoMoveTo (0, 0);
    int nglyphs = g.getNumGlyphs ();
    int codes[] = g.getGlyphCodes (0, nglyphs, (int []) null);
    float posns[] = g.getGlyphPositions (0, nglyphs, (float []) null);
    cairoShowGlyphs (codes, posns);
    stateRestore ();
  }

  public void copyArea (int x, int y, int width, int height, int dx, int dy)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public void drawArc (int x, int y, int width, int height,
                       int startAngle, int arcAngle)
  {
    draw (new Arc2D.Double((double)x, (double)y, 
                           (double)width, (double)height,
                           (double)startAngle, (double)arcAngle,
                           Arc2D.OPEN));
  }

  public boolean drawImage (Image img, int x, int y, Color bgcolor, 
                            ImageObserver observer)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public boolean drawImage (Image img, int x, int y, int width, int height, 
                            Color bgcolor, ImageObserver observer)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public boolean drawImage (Image img, int x, int y, int width, int height, 
                            ImageObserver observer)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public boolean drawImage (Image img, int dx1, int dy1, int dx2, int dy2, 
                            int sx1, int sy1, int sx2, int sy2, 
                            Color bgcolor, ImageObserver observer)
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public boolean drawImage (Image img, int dx1, int dy1, int dx2, int dy2, 
                            int sx1, int sy1, int sx2, int sy2, 
                            ImageObserver observer) 
  {
    throw new java.lang.UnsupportedOperationException ();
  }

  public void drawOval(int x, int y, int width, int height)
  {
    drawArc (x, y, width, height, 0, 360);
  }

  public void drawRoundRect(int x, int y, int width, int height, 
                            int arcWidth, int arcHeight)
  {
    int x1 = x + arcWidth, x2 = x + width - arcWidth;
    int y1 = y + arcHeight, y2 = y + height - arcHeight;
    fillRect (x1, y, x2 - x1, height);
    fillRect (x, y1, width, y2 - y1);
    fillArc (x, y, arcWidth, arcHeight, 90, 90);
    fillArc (x1, y, arcWidth, arcHeight, 0, 90);
    fillArc (x2, y2, arcWidth, arcHeight, 270, 90);
    fillArc (x, y2, arcWidth, arcHeight, 180, 90);
  }

  public void drawString (String str, int x, int y)
  {
    drawString (str, (float)x, (float)y);
  }

  public void drawString (String str, float x, float y)
  {
    GlyphVector gv = font.createGlyphVector (getFontRenderContext(), str);
    drawGlyphVector (gv, x, y);
  }

  public void drawString (AttributedCharacterIterator ci, int x, int y)
  {
    drawString (ci, (float)x, (float)y);
  }

  public void drawString (AttributedCharacterIterator ci, float x, float y)
  {
    GlyphVector gv = font.createGlyphVector (getFontRenderContext(), ci);
    drawGlyphVector (gv, x, y);
  }

  public void fillArc (int x, int y, int width, int height, 
                       int startAngle, int arcAngle)
  {
    fill (new Arc2D.Double((double)x, (double)y, 
                           (double)width, (double)height,
                           (double)startAngle, (double)arcAngle,
                           Arc2D.OPEN));
  }

  public void fillOval(int x, int y, int width, int height)
  {
    fillArc (x, y, width, height, 0, 360);
  }

  public void fillRoundRect (int x, int y, int width, int height, 
                             int arcWidth, int arcHeight)
  {
    int x1 = x + arcWidth, x2 = x + width - arcWidth;
    int y1 = y + arcHeight, y2 = y + height - arcHeight;
    fillRect (x1, y, x2 - x1, height);
    fillRect (x, y1, width, y2 - y1);
    fillArc (x, y, arcWidth, arcHeight, 90, 90);
    fillArc (x1, y, arcWidth, arcHeight, 0, 90);
    fillArc (x2, y2, arcWidth, arcHeight, 270, 90);
    fillArc (x, y2, arcWidth, arcHeight, 180, 90);
  }

  public Font getFont ()
  {
    return font;
  }

  public FontMetrics getFontMetrics ()
  {
    return Toolkit.getDefaultToolkit ().getFontMetrics (font);
  }

  public FontMetrics getFontMetrics (Font f)
  {
    return Toolkit.getDefaultToolkit ().getFontMetrics (f);
  }

  public void setFont (Font f)
  {
    if (f.getPeer() instanceof GdkClasspathFontPeer)
      font = f;
    else
      font = 
        ((ClasspathToolkit)(Toolkit.getDefaultToolkit ()))
        .getFont (f.getName(), f.getAttributes ());

    if (f != null && 
        f.getPeer() instanceof GdkClasspathFontPeer)
      cairoSetFont ((GdkClasspathFontPeer) f.getPeer());
  }

  public String toString()
  {
    throw new java.lang.UnsupportedOperationException ();
  }

}
