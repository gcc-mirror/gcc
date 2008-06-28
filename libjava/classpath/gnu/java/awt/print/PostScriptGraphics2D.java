/* PostScriptGraphics2D.java -- AWT printer rendering class.
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

package gnu.java.awt.print;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Paint;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.TextLayout;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.renderable.RenderableImage;
import java.awt.image.RenderedImage;
import java.awt.image.ImageObserver;
import java.awt.image.PixelGrabber;
import java.awt.print.PageFormat;
import java.awt.print.Pageable;
import java.awt.print.Paper;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.text.AttributedCharacterIterator;
import java.util.Map;

/**
 * Class PostScriptGraphics2D - Class that implements the Graphics2D object,
 * writing the output to a PostScript or EPS file
 *
 * @author Sven de Marothy
 *
 */
class PostScriptGraphics2D extends Graphics2D
{
  /**
   * The associated printer job.
   */
  private PrinterJob printerJob;

  /**
   * Output file.
   */
  private PrintWriter out;

  // Graphics data
  private AffineTransform currentTransform = new AffineTransform();
  private AffineTransform pageTransform;
  private RenderingHints renderingHints;
  private Paint currentPaint = null;
  private Shape clipShape = null;
  private Font currentFont = null;
  private Color currentColor = Color.black;
  private Color backgroundColor = Color.white;
  private Stroke currentStroke = null;
  private static Stroke ordinaryStroke = new BasicStroke(0.0f,
                                                         BasicStroke.CAP_BUTT,
                                                         BasicStroke.JOIN_MITER);
  private float cx; // current drawing position
  private float cy; // current drawing position
  private boolean currentFontIsPS; // set if currentFont is one of the above

  // settings
  private double pageX = 595;
  private double pageY = 842;
  private double Y = pageY;
  private boolean gradientOn = false;

  /** 
   * Constructor
   *
   */
  public PostScriptGraphics2D( PrinterJob pg )
  {
    printerJob = pg;
    // create transform objects
    pageTransform = new AffineTransform();
    currentTransform = new AffineTransform();

    /*
      Create Rendering hints
      No text aliasing
      Quality color and rendering
      Bicubic interpolation
      Fractional metrics supported
    */
    renderingHints = new RenderingHints(null);
    renderingHints.put(RenderingHints.KEY_RENDERING,
                       RenderingHints.VALUE_RENDER_QUALITY);
    renderingHints.put(RenderingHints.KEY_TEXT_ANTIALIASING,
                       RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
    renderingHints.put(RenderingHints.KEY_INTERPOLATION,
                       RenderingHints.VALUE_INTERPOLATION_BICUBIC);
    renderingHints.put(RenderingHints.KEY_FRACTIONALMETRICS,
                       RenderingHints.VALUE_FRACTIONALMETRICS_ON);
    renderingHints.put(RenderingHints.KEY_COLOR_RENDERING,
                       RenderingHints.VALUE_COLOR_RENDER_QUALITY);
  }

  /**
   * Spool a document to PostScript.
   * If Pageable is non-null, it will print that, otherwise it will use
   * the supplied printable and pageFormat.
   */
  public SpooledDocument spoolPostScript(Printable printable, 
					 PageFormat pageFormat,
					 Pageable pageable)
    throws PrinterException
  {
    try 
      {
	// spool to a temporary file
	File temp = File.createTempFile("cpspool", ".ps");
	temp.deleteOnExit();
	
	out = new PrintWriter(new BufferedWriter
			      (new OutputStreamWriter
			       (new FileOutputStream(temp), 
				"ISO8859_1"), 1000000));
	
	writePSHeader();
	
	if(pageable != null)
	  {
	    for(int index = 0; index < pageable.getNumberOfPages(); index++)
	      spoolPage(out, pageable.getPrintable(index),
			pageable.getPageFormat(index), index);
	  }
	else
	  {
	    int index = 0;
	    while(spoolPage(out, printable, pageFormat, index++) ==
		  Printable.PAGE_EXISTS)
              ;
	  }
	out.println("%%Trailer");
	out.println("%%EOF");
	out.close();
	return new SpooledDocument( temp );
      } 
    catch (IOException e) 
      {
	PrinterException pe = new PrinterException();
	pe.initCause(e);
	throw pe;
      }
  }

  //--------------------------------------------------------------------------

  /** 
   * Write the postscript file header,
   * setup the page format and transforms. 
   */
  private void writePSHeader()
  {
    out.println("%!PS-Adobe-3.0");      
    out.println("%%Title: "+printerJob.getJobName());
    out.println("%%Creator: GNU Classpath ");
    out.println("%%DocumentData: Clean8Bit");

    out.println("%%DocumentNeededResources: font Times-Roman Helvetica Courier");
    out.println("%%EndComments");
    
    out.println("%%BeginProlog");
    out.println("%%EndProlog");
    out.println("%%BeginSetup");
    
    out.println("%%EndFeature");
    setupFonts();
    out.println("%%EndSetup");
 
    // set default fonts and colors
    setFont( new Font("Dialog", Font.PLAIN, 12) );
    currentColor = Color.white;
    currentStroke = new BasicStroke();
    setPaint(currentColor);
    setStroke(currentStroke);
  }

  /**
   * setupFonts - set up the font dictionaries for
   * helvetica, times and courier
   */
  private void setupFonts()
  {
    out.println("/helveticaISO");
    out.println("/Helvetica findfont dup length dict begin");
    out.println("{ 1 index /FID eq { pop pop } { def } ifelse } forall");
    out.println("/Encoding ISOLatin1Encoding def");
    out.println("currentdict end definefont pop");

    out.println("/timesISO");
    out.println("/Times-Roman findfont dup length dict begin");
    out.println("{ 1 index /FID eq { pop pop } { def } ifelse } forall");
    out.println("/Encoding ISOLatin1Encoding def");
    out.println("currentdict end definefont pop");

    out.println("/courierISO");
    out.println("/Courier findfont dup length dict begin");
    out.println("{ 1 index /FID eq { pop pop } { def } ifelse } forall");
    out.println("/Encoding ISOLatin1Encoding def");
    out.println("currentdict end definefont pop");
  }

  /**
   * Spools a single page, returns NO_SUCH_PAGE unsuccessful,
   * PAGE_EXISTS if it was.
   */
  public int spoolPage(PrintWriter out,
		       Printable printable, 
		       PageFormat pageFormat, 
		       int index) throws IOException, PrinterException
  {
    out.println("%%BeginPageSetup");

    Paper p = pageFormat.getPaper();
    pageX = p.getWidth();
    pageY = p.getHeight();

    if( pageFormat.getOrientation() == PageFormat.PORTRAIT )
      out.println( "%%Orientation: Portrait" );
    else
      {
	out.println( "%%Orientation: Landscape" );
	double t = pageX;
	pageX = pageY;
	pageY = t;
      }
      
    setClip(0, 0, (int)pageX, (int)pageY);

    out.println("gsave % first save");
    
    // 595x842; 612x792 respectively
    out.println("<< /PageSize [" +pageX + " "+pageY+ "] >> setpagedevice");

    if( pageFormat.getOrientation() != PageFormat.LANDSCAPE )
      {
	pageTransform.translate(pageX, 0);
	pageTransform.scale(-1.0, 1.0);
      }

    // save the original CTM
    pushCTM();
    concatCTM(pageTransform);
    setTransform(new AffineTransform());

    out.println("%%EndPageSetup");

    out.println("gsave");

    if( printable.print(this, pageFormat, index) == Printable.NO_SUCH_PAGE )
      return Printable.NO_SUCH_PAGE;
    
    out.println("grestore");
    out.println("showpage");

    return Printable.PAGE_EXISTS;
  }

  /** push the Current Transformation Matrix onto the PS stack */
  private void pushCTM()
  {
    out.println("matrix currentmatrix   % pushCTM()");
  }

  /** pop the Current Transformation Matrix from the PS stack */
  private void popCTM()
  {
    out.println("setmatrix % restore CTM");
  }

  ///////////////////////////////////////////////////////////////////////////

  public Graphics create()
  {
    return null;
  }

  public void drawOval(int x, int y, int width, int height)
  {
    out.println("% drawOval()");
    setStroke(ordinaryStroke);
    draw(new Ellipse2D.Double(x, y, width, height));
    setStroke(currentStroke);
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints)
  {
    if (nPoints <= 0 || xPoints.length < nPoints || yPoints.length < nPoints)
      return;
    out.println("newpath % drawPolyLine()");
    out.println(xPoints[0] + " " + yPoints[0] + " moveto");
    for (int i = 1; i < nPoints; i++)
      out.println(xPoints[i] + " " + yPoints[i] + " lineto");
    out.println("closepath");
    out.println("stroke");
  }

  public void drawRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    out.println("% drawRoundRect()");
    RoundRectangle2D.Double rr = new RoundRectangle2D.Double(x, y, width,
                                                             height, arcWidth,
                                                             arcHeight);
    setStroke(ordinaryStroke);
    draw(rr);
    setStroke(currentStroke);
  }

  public void fillRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    out.println("% fillRoundRect()");
    RoundRectangle2D.Double rr = new RoundRectangle2D.Double(x, y, width,
                                                             height, arcWidth,
                                                             arcHeight);
    fill(rr);
  }

  public void drawArc(int x, int y, int width, int height, int startAngle,
                      int arcAngle)
  {
    setStroke(ordinaryStroke);
    draw(new Arc2D.Double(x, y, width, height, startAngle, arcAngle, Arc2D.OPEN));
    setStroke(currentStroke);
  }

  public void fillArc(int x, int y, int width, int height, int startAngle,
                      int arcAngle)
  {
    fill(new Arc2D.Double(x, y, width, height, startAngle, arcAngle, Arc2D.PIE));
  }

  public void fillOval(int x, int y, int width, int height)
  {
    out.println("% fillOval()");
    fill( new Ellipse2D.Double(x, y, width, height) );
  }

  public void fillPolygon(int[] x, int[] y, int nPoints)
  {
    out.println("% fillPolygon()");
    fill( new Polygon(x, y, nPoints) );
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    out.println("% drawLine()");
    setStroke(ordinaryStroke);
    out.println("newpath");
    out.println(x1 + " " + (y1) + " moveto");
    out.println(x2 + " " + (y2) + " lineto");
    out.println("stroke");
    setStroke(currentStroke);
  }

  //--------------- Image drawing ------------------------------------------   
  public boolean drawImage(Image img, int x, int y, Color bgcolor,
                           ImageObserver observer)
  {
    int w = img.getWidth(null);
    int h = img.getHeight(null);

    return drawImage(img, x, y, x + w, y + h, 0, 0, w - 1, h - 1, bgcolor,
		     observer);
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2, Color bgcolor,
                           ImageObserver observer)
  {
    int n = 0;
    boolean flipx = false;
    boolean flipy = false;

    // swap X and Y's
    if (sx1 > sx2)
      {
	n = sx1;
	sx1 = sx2;
	sx2 = n;
	flipx = ! flipx;
      }
    if (sy1 > sy2)
      {
	n = sy1;
	sy1 = sy2;
	sy2 = n;
	flipy = ! flipy;
      }
    if (dx1 > dx2)
      {
	n = dx1;
	dx1 = dx2;
	dx2 = n;
	flipx = ! flipx;
      }
    if (dy1 > dy2)
      {
	n = dy1;
	dy1 = dy2;
	dy2 = n;
	flipy = ! flipy;
      }
    n = 0;
    int sw = sx2 - sx1; // source width
    int sh = sy2 - sy1; // source height
    int[] pixels = new int[sw * sh]; // pixel buffer
    int dw = dx2 - dx1; // destination width
    int dh = dy2 - dy1; // destination height
    double x_scale = ((double) dw) / ((double) sw);
    double y_scale = ((double) dh) / ((double) sh);

    out.println("% drawImage() 2");
    out.println("gsave");
    out.println(dx1 + " " + dy1 + " translate");
    out.println(dw + " " + dh + " scale");
    out.println(sw + " " + sh + " 8 [" + (flipx ? -sw : sw) + " 0 0 "
                + (flipy ? -sh : sh) + " " + (flipx ? sw : 0) + " "
                + (flipy ? sh : 0) + " ]");
    out.println("{currentfile 3 string readhexstring pop} bind");
    out.println("false 3 colorimage");

    PixelGrabber pg = new PixelGrabber(img, sx1, sy1, sw, sh, pixels, 0, sw);
    try
      {
	pg.grabPixels();
      }
    catch (InterruptedException e)
      {
	System.err.println("interrupted waiting for pixels!");
	return (false);
      }

    if ((pg.getStatus() & ImageObserver.ABORT) != 0)
      {
	System.err.println("image fetch aborted or errored");
	return (false);
      }

    for (int j = 0; j < sh; j++)
      {
	for (int i = 0; i < sw; i++)
	  {
	    out.print(colorTripleHex(new Color(pixels[j * sw + i])));
	    if (((++n) % 11) == 0)
	      out.println();
	  }
      }

    out.println();
    out.println("%%EOF");
    out.println("grestore");
    return true;
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           ImageObserver observer)
  {
    return drawImage(img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, null,
		     observer);
  }

  public boolean drawImage(Image img, int x, int y, ImageObserver observer)
  {
    return drawImage(img, x, y, null, observer);
  }

  public boolean drawImage(Image img, int x, int y, int width, int height,
			   Color bgcolor, ImageObserver observer)
  {
    int sw = img.getWidth(null);
    int sh = img.getHeight(null);
    return drawImage(img, x, y, x + width, y + height, /* destination */
		     0, 0, sw - 1, sh - 1, /* source */
		     bgcolor, observer);
    // correct?
  }

  public boolean drawImage(Image img, int x, int y, int width, int height,
			   ImageObserver observer)
  {
    return drawImage(img, x, y, width, height, null, observer);
  }

  /** Renders a BufferedImage that is filtered with a BufferedImageOp. */
  public void drawImage(BufferedImage img, BufferedImageOp op, int x, int y)
  {
    BufferedImage result = op.filter(img, null);
    drawImage(result, x, y, null);
  }

  /** Renders an image, applying a transform from image space
      into user space before drawing. */
  public boolean drawImage(Image img, AffineTransform xform, ImageObserver obs)
  {
    AffineTransform oldTransform = new AffineTransform(currentTransform);
    boolean ret;

    transform(xform);
    ret = drawImage(img, 0, 0, null, obs);
    setTransform(oldTransform);

    return ret;
  }

  /** Renders a RenderableImage, applying a transform from image
      space into user space before drawing. */
  public void drawRenderableImage(RenderableImage img, AffineTransform xform)
  {
    // FIXME
  }

  /** Renders a RenderedImage, applying a transform from
      image space into user space before drawing. */
  public void drawRenderedImage(RenderedImage img, AffineTransform xform)
  {
    // FIXME
  }

  //-------------------------------------------------------------------------
  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    setStroke(ordinaryStroke);
    draw(new Polygon(xPoints, yPoints, nPoints));
    setStroke(currentStroke);
  }

  public void drawString(String str, int x, int y)
  {
    drawString(str, (float) x, (float) y);
  }

  public void drawString(String str, float x, float y)
  {
    if( str.trim().equals("") )
      return; // don't draw whitespace, silly!

    if( currentFontIsPS )
      {
	drawStringPSFont(str, x, y);
	return;
      }

    TextLayout text = new TextLayout(str, currentFont, getFontRenderContext());
    Shape s = text.getOutline(AffineTransform.getTranslateInstance(x, y));
    drawStringShape(s);
  }

  private void drawStringPSFont(String str, float x, float y)
  {
    out.println("% drawString PS font");
    out.println(x + " " + y + " moveto");
    saveAndInvertAxis();
    out.println("(" + str + ") show");
    restoreAxis();
  }

  private void saveAndInvertAxis()
  {
    // Invert the Y axis of the CTM.
    popCTM();
    pushCTM();

    double[] test = 
      {
	pageTransform.getScaleX(), pageTransform.getShearY(),
	pageTransform.getShearX(), pageTransform.getScaleY(),
	pageTransform.getTranslateX(),
	-pageTransform.getTranslateY() + pageY
      };

    double[] test2 = 
      {
	currentTransform.getScaleX(),
	currentTransform.getShearY(),
	-currentTransform.getShearX(),
	-currentTransform.getScaleY(),
	currentTransform.getTranslateX(),
	currentTransform.getTranslateY()
      };

    AffineTransform total = new AffineTransform(test);
    total.concatenate(new AffineTransform(test2));
    concatCTM(total);
  }

  private void restoreAxis()
  {
    // reset the CTM
    popCTM();
    pushCTM();
    AffineTransform total = new AffineTransform(pageTransform);
    total.concatenate(currentTransform);
    concatCTM(total);
  }

  /**
   * special drawing routine for string shapes,
   * which need to be drawn with the Y axis uninverted.
   */
  private void drawStringShape(Shape s)
  {
    saveAndInvertAxis();

    // draw the shape s with an inverted Y axis.
    PathIterator pi = s.getPathIterator(null);
    float[] coords = new float[6];

    while (! pi.isDone())
      {
	switch (pi.currentSegment(coords))
	  {
	  case PathIterator.SEG_MOVETO:
	    out.println((coords[0]) + " " + (Y - coords[1]) + " moveto");
	    cx = coords[0];
	    cy = coords[1];
	    break;
	  case PathIterator.SEG_LINETO:
	    out.println((coords[0]) + " " + (Y - coords[1]) + " lineto");
	    cx = coords[0];
	    cy = coords[1];
	    break;
	  case PathIterator.SEG_QUADTO:
	    // convert to cubic bezier points
	    float x1 = (cx + 2 * coords[0]) / 3;
	    float y1 = (cy + 2 * coords[1]) / 3;
	    float x2 = (2 * coords[2] + coords[0]) / 3;
	    float y2 = (2 * coords[3] + coords[1]) / 3;

	    out.print((x1) + " " + (Y - y1) + " ");
	    out.print((x2) + " " + (Y - y2) + " ");
	    out.println((coords[2]) + " " + (Y - coords[3]) + " curveto");
	    cx = coords[2];
	    cy = coords[3];
	    break;
	  case PathIterator.SEG_CUBICTO:
	    out.print((coords[0]) + " " + (Y - coords[1]) + " ");
	    out.print((coords[2]) + " " + (Y - coords[3]) + " ");
	    out.println((coords[4]) + " " + (Y - coords[5]) + " curveto");
	    cx = coords[4];
	    cy = coords[5];
	    break;
	  case PathIterator.SEG_CLOSE:
	    out.println("closepath");
	    break;
	  }
	pi.next();
      }
    out.println("fill");

    restoreAxis();
  }

  public void setColor(Color c)
  {
    /* don't set the color if it's already set */
    if (c.equals(currentColor))
      return;
    gradientOn = false;
    currentColor = c;
    currentPaint = c; // Graphics2D extends colors to paint

    out.println(colorTriple(c) + " setrgbcolor");
  }

  public void clearRect(int x, int y, int width, int height)
  {
    out.println("% clearRect");
    Color c = currentColor;
    setColor(backgroundColor);
    fill(new Rectangle2D.Double(x, y, width, height));
    setColor(c);
  }

  public void clipRect(int x, int y, int width, int height)
  {
    clip(new Rectangle2D.Double(x, y, width, height));
  }

  public void copyArea(int x, int y, int width, int height, int dx, int dy)
  {
    // FIXME
  }

  public void fillRect(int x, int y, int width, int height)
  {
    fill(new Rectangle2D.Double(x, y, width, height));
  }

  public void dispose()
  {
  }

  public void setClip(int x, int y, int width, int height)
  {
    out.println("% setClip()");
    setClip(new Rectangle2D.Double(x, y, width, height));
  }

  public void setClip(Shape s)
  {
    clip(s);
  }

  public Shape getClip()
  {
    return clipShape;
  }

  public Rectangle getClipBounds()
  {
    return clipShape.getBounds();
  }

  public Color getColor()
  {
    return currentColor;
  }

  public Font getFont()
  {
    return currentFont;
  }

  public FontMetrics getFontMetrics()
  {
    return getFontMetrics(currentFont);
  }

  public FontMetrics getFontMetrics(Font f)
  {
    // FIXME
    return null;
  }

  public void setFont(Font font)
  {
    out.println("% setfont()");
    if (font == null)
      // use the default font
      font = new Font("Dialog", Font.PLAIN, 12);
    currentFont = font;
    setPSFont(); // set up the PostScript fonts
  }

  /**
   * Setup the postscript font if the current font is one
   */
  private void setPSFont()
  {
    currentFontIsPS = false;

    String s = currentFont.getName();
    out.println("% setPSFont: Fontname: " + s);
    if (s.equalsIgnoreCase("Helvetica") || s.equalsIgnoreCase("SansSerif"))
      out.print("/helveticaISO findfont ");
    else if (s.equalsIgnoreCase("Times New Roman"))
      out.print("/timesISO findfont ");
    else if (s.equalsIgnoreCase("Courier"))
      out.print("/courierISO findfont ");
    else
      return;

    currentFontIsPS = true;

    out.print(currentFont.getSize() + " scalefont ");
    out.println("setfont");
  }

  /** XOR mode is not supported */
  public void setPaintMode()
  {
  }

  /** XOR mode is not supported */
  public void setXORMode(Color c1)
  {
  }

  public void close()
  {
    out.println("showpage");
    out.println("%%Trailer");
    out.println("grestore % restore original stuff");
    out.println("%%EOF");

    try
      {
	out.close();
      }
    catch (Exception e)
      {
      }
    out = null;
  }

  //----------------------------------------------------------------
  // Graphics2D stuff ----------------------------------------------

  /**  Sets the values of an arbitrary number of
       preferences for the rendering algorithms. */
  public void addRenderingHints(Map hints)
  {
    /* rendering hint changes are disallowed */
  }

  /** write a shape to the file */
  private void writeShape(Shape s)
  {
    PathIterator pi = s.getPathIterator(null);
    float[] coords = new float[6];

    while (! pi.isDone())
      {
	switch (pi.currentSegment(coords))
	  {
	  case PathIterator.SEG_MOVETO:
	    out.println(coords[0] + " " + (coords[1]) + " moveto");
	    cx = coords[0];
	    cy = coords[1];
	    break;
	  case PathIterator.SEG_LINETO:
	    out.println(coords[0] + " " + (coords[1]) + " lineto");
	    cx = coords[0];
	    cy = coords[1];
	    break;
	  case PathIterator.SEG_QUADTO:
	    // convert to cubic bezier points
	    float x1 = (cx + 2 * coords[0]) / 3;
	    float y1 = (cy + 2 * coords[1]) / 3;
	    float x2 = (2 * coords[2] + coords[0]) / 3;
	    float y2 = (2 * coords[3] + coords[1]) / 3;

	    out.print(x1 + " " + (Y - y1) + " ");
	    out.print(x2 + " " + (Y - y2) + " ");
	    out.println(coords[2] + " " + (Y - coords[3]) + " curveto");
	    cx = coords[2];
	    cy = coords[3];
	    break;
	  case PathIterator.SEG_CUBICTO:
	    out.print(coords[0] + " " + coords[1] + " ");
	    out.print(coords[2] + " " + coords[3] + " ");
	    out.println(coords[4] + " " + coords[5] + " curveto");
	    cx = coords[4];
	    cy = coords[5];
	    break;
	  case PathIterator.SEG_CLOSE:
	    out.println("closepath");
	    break;
	  }
	pi.next();
      }
  }

  /** Intersects the current Clip with the interior of
      the specified Shape and sets the Clip to the resulting intersection. */
  public void clip(Shape s)
  {
    clipShape = s;
    out.println("% clip INACTIVE");
    //	writeShape(s);
    //	out.println("clip");
  }

  /** Strokes the outline of a Shape using the
      settings of the current Graphics2D context.*/
  public void draw(Shape s)
  {
    if(!(currentStroke instanceof BasicStroke))
      fill(currentStroke.createStrokedShape(s));

    out.println("% draw");
    writeShape(s);
    out.println("stroke");
  }

  /** Renders the text of the specified GlyphVector using the
      Graphics2D context's rendering attributes. */
  public void drawGlyphVector(GlyphVector gv, float x, float y)
  {
    out.println("% drawGlyphVector");
    Shape s = gv.getOutline();
    drawStringShape(AffineTransform.getTranslateInstance(x, y)
		    .createTransformedShape(s));
  }

  /** Renders the text of the specified iterator,
      using the Graphics2D context's current Paint.*/
  public void drawString(AttributedCharacterIterator iterator, float x, float y)
  {
    TextLayout text = new TextLayout(iterator, getFontRenderContext());
    Shape s = text.getOutline(AffineTransform.getTranslateInstance(x, y));
    drawStringShape(s);
  }

  /** Renders the text of the specified iterator,
      using the Graphics2D context's current Paint. */
  public void drawString(AttributedCharacterIterator iterator, int x, int y)
  {
    drawString(iterator, (float) x, (float) y);
  }

  /** Fills the interior of a Shape using the settings of the Graphics2D context. */
  public void fill(Shape s)
  {
    out.println("% fill");
    if (! gradientOn)
      {
	writeShape(s);
	out.println("fill");
      }
    else
      {
	out.println("gsave");
	writeShape(s);
	out.println("clip");
	writeGradient();
	out.println("shfill");
	out.println("grestore");
      }
  }

  /** Returns the background color used for clearing a region. */
  public Color getBackground()
  {
    return backgroundColor;
  }

  /** Returns the current Composite in the Graphics2D context. */
  public Composite getComposite()
  {
    // FIXME
    return null;
  }

  /** Returns the device configuration associated with this Graphics2D. */
  public GraphicsConfiguration getDeviceConfiguration()
  {
    // FIXME
    out.println("% getDeviceConfiguration()");
    return null;
  }

  /** Get the rendering context of the Font within this Graphics2D context. */
  public FontRenderContext getFontRenderContext()
  {
    out.println("% getFontRenderContext()");

    double[] scaling = 
      {
	pageTransform.getScaleX(), 0, 0,
	-pageTransform.getScaleY(), 0, 0
      };

    return (new FontRenderContext(new AffineTransform(scaling), false, true));
  }

  /** Returns the current Paint of the Graphics2D context. */
  public Paint getPaint()
  {
    return currentPaint;
  }

  /** Returns the value of a single preference for the rendering algorithms. */
  public Object getRenderingHint(RenderingHints.Key hintKey)
  {
    return renderingHints.get(hintKey);
  }

  /** Gets the preferences for the rendering algorithms. */
  public RenderingHints getRenderingHints()
  {
    return renderingHints;
  }

  /** Returns the current Stroke in the Graphics2D context. */
  public Stroke getStroke()
  {
    return currentStroke;
  }

  /** Returns a copy of the current Transform in the Graphics2D context. */
  public AffineTransform getTransform()
  {
    return currentTransform;
  }

  /** 
   * Checks whether or not the specified Shape intersects 
   * the specified Rectangle, which is in device space. 
   */
  public boolean hit(Rectangle rect, Shape s, boolean onStroke)
  {
    Rectangle2D.Double r = new Rectangle2D.Double(rect.getX(), rect.getY(),
						  rect.getWidth(),
						  rect.getHeight());
    return s.intersects(r);
  }

  /** Sets the background color for the Graphics2D context.*/
  public void setBackground(Color color)
  {
    out.println("% setBackground(" + color + ")");
    backgroundColor = color;
  }

  /** Sets the Composite for the Graphics2D context.
      Not supported. */
  public void setComposite(Composite comp)
  {
  }

  /** Sets the Paint attribute for the Graphics2D context.*/
  public void setPaint(Paint paint)
  {
    currentPaint = paint;
    gradientOn = false;
    if (paint instanceof Color)
      {
	setColor((Color) paint);
	return;
      }
    if (paint instanceof GradientPaint)
      {
	gradientOn = true;
	return;
      }
  }

  /* get a space seperated 0.0 - 1.0 color RGB triple */
  private String colorTriple(Color c)
  {
    return (((double) c.getRed() / 255.0) + " "
	    + ((double) c.getGreen() / 255.0) + " "
	    + ((double) c.getBlue() / 255.0));
  }

  /**
   * Get a nonsperated hex RGB triple, eg FFFFFF = white
   * used by writeGradient and drawImage 
   */
  private String colorTripleHex(Color c)
  {
    String r = "00" + Integer.toHexString(c.getRed());
    r = r.substring(r.length() - 2);
    String g = "00" + Integer.toHexString(c.getGreen());
    g = g.substring(g.length() - 2);
    String b = "00" + Integer.toHexString(c.getBlue());
    b = b.substring(b.length() - 2);
    return r + g + b;
  }

  /* write the current gradient fill */
  private void writeGradient()
  {
    GradientPaint paint = (GradientPaint) currentPaint;
    out.println("% writeGradient()");

    int n = 1;
    double x;
    double y;
    double dx;
    double dy;
    Point2D p1 = currentTransform.transform(paint.getPoint1(), null);
    Point2D p2 = currentTransform.transform(paint.getPoint2(), null);
    x = p1.getX();
    y = p1.getY();
    dx = p2.getX() - x;
    dy = p2.getY() - y;

    // get number of repetitions
    while (x + n * dx < pageY && y + n * dy < pageX && x + n * dx > 0
	   && y + n * dy > 0)
      n++;

    out.println("<<"); // start
    out.println("/ShadingType 2"); // gradient fill 
    out.println("/ColorSpace [ /DeviceRGB ]"); // RGB colors
    out.print("/Coords [");
    out.print(x + " " + y + " " + (x + n * dx) + " " + (y + n * dy) + " ");
    out.println("]"); // coordinates defining the axis
    out.println("/Function <<");
    out.println("/FunctionType 0");
    out.println("/Order 1");
    out.println("/Domain [ 0 1 ]");
    out.println("/Range [ 0 1  0 1  0 1 ]");
    out.println("/BitsPerSample 8");
    out.println("/Size [ " + (1 + n) + " ]");
    out.print("/DataSource < " + colorTripleHex(paint.getColor1()) + " "
	      + colorTripleHex(paint.getColor2()) + " ");
    for (; n > 1; n--)
      if (paint.isCyclic())
	{
	  if ((n % 2) == 1)
	    out.print(colorTripleHex(paint.getColor1()) + " ");
	  else
	    out.print(colorTripleHex(paint.getColor2()) + " ");
	}
      else
	out.print(colorTripleHex(paint.getColor2()) + " ");
    out.println(">");
    out.println(">>");
    out.println(">>");
  }

  /** Sets the value of a single preference for the rendering algorithms. */
  public void setRenderingHint(RenderingHints.Key hintKey, Object hintValue)
  {
    /* we don't allow the changing of rendering hints. */
  }

  /** Replaces the values of all preferences for the rendering algorithms
      with the specified hints. */
  public void setRenderingHints(Map hints)
  {
    /* we don't allow the changing of rendering hints. */
  }

  /** 
   * Sets the Stroke for the Graphics2D context. BasicStroke fully implemented.
   */
  public void setStroke(Stroke s)
  {
    currentStroke = s;

    if (! (s instanceof BasicStroke))
      return;

    BasicStroke bs = (BasicStroke) s;
    out.println("% setStroke()");
    try
      {
	// set the line width
	out.println(bs.getLineWidth() + " setlinewidth");

	// set the line dash
	float[] dashArray = bs.getDashArray();
	if (dashArray != null)
	  {
	    out.print("[ ");
	    for (int i = 0; i < dashArray.length; i++)
	      out.print(dashArray[i] + " ");
	    out.println("] " + bs.getDashPhase() + " setdash");
	  }
	else
	  out.println("[] 0 setdash"); // set solid

	// set the line cap
	switch (bs.getEndCap())
	  {
	  case BasicStroke.CAP_BUTT:
	    out.println("0 setlinecap");
	    break;
	  case BasicStroke.CAP_ROUND:
	    out.println("1 setlinecap");
	    break;
	  case BasicStroke.CAP_SQUARE:
	    out.println("2 setlinecap");
	    break;
	  }

	// set the line join
	switch (bs.getLineJoin())
	  {
	  case BasicStroke.JOIN_BEVEL:
	    out.println("2 setlinejoin");
	    break;
	  case BasicStroke.JOIN_MITER:
	    out.println("0 setlinejoin");
	    out.println(bs.getMiterLimit() + " setmiterlimit");
	    break;
	  case BasicStroke.JOIN_ROUND:
	    out.println("1 setlinejoin");
	    break;
	  }
      }
    catch (Exception e)
      {
	out.println("% Exception in setStroke()");
      }
  }

  //////////////////// TRANSFORM SETTING /////////////////////////////////////
  private void concatCTM(AffineTransform Tx)
  {
    double[] matrixElements = new double[6];
    Tx.getMatrix(matrixElements);

    out.print("[ ");
    for (int i = 0; i < 6; i++)
      out.print(matrixElements[i] + " ");
    out.println("] concat");
  }

  /** Sets the Transform in the Graphics2D context. */
  public void setTransform(AffineTransform Tx)
  {
    // set the transformation matrix;
    currentTransform = Tx;

    // concatenate the current transform and the page transform
    AffineTransform totalTransform = new AffineTransform(pageTransform);
    totalTransform.concatenate(currentTransform);
    out.println("% setTransform()");
    out.println("% pageTransform:" + pageTransform);
    out.println("% currentTransform:" + currentTransform);
    out.println("% totalTransform:" + totalTransform);

    popCTM();
    pushCTM(); // set the CTM to it's original state
    concatCTM(totalTransform); // apply our transforms
  }

  /** Composes an AffineTransform object with the Transform
      in this Graphics2D according to the rule last-specified-first-applied. */
  public void transform(AffineTransform Tx)
  {
    // concatenate the current transform
    currentTransform.concatenate(Tx);
    // and the PS CTM
    concatCTM(Tx);
  }

  ////////////////////////// TRANSFORMS //////////////////////////////////////

  /** shear transform */
  public void shear(double shx, double shy)
  {
    out.println("% shear()");
    AffineTransform Tx = new AffineTransform();
    Tx.shear(shx, shy);
    transform(Tx);
  }

  /** Translates the origin of the Graphics2D context
      to the point (x, y) in the current coordinate system. */
  public void translate(int x, int y)
  {
    out.println("% translate()");
    AffineTransform Tx = new AffineTransform();
    Tx.translate(x, y);
    transform(Tx);
  }

  /** Translates the origin of the Graphics2D context
      to the point (x, y) in the current coordinate system. */
  public void translate(double x, double y)
  {
    out.println("% translate(" + x + ", " + y + ")");
    AffineTransform Tx = new AffineTransform();
    Tx.translate(x, y);
    transform(Tx);
  }

  /** Concatenates the current Graphics2D Transform with a rotation transform.*/
  public void rotate(double theta)
  {
    out.println("% rotate(" + theta + ")");
    AffineTransform Tx = new AffineTransform();
    Tx.rotate(theta);
    transform(Tx);
  }

  /** Concatenates the current Graphics2D Transform with
      a translated rotation transform.*/
  public void rotate(double theta, double x, double y)
  {
    out.println("% rotate()");
    AffineTransform Tx = new AffineTransform();
    Tx.rotate(theta, x, y);
    transform(Tx);
  }

  /** Concatenates the current Graphics2D Transform with a scaling
      transformation Subsequent rendering is resized according to the
      specified scaling factors relative to the previous scaling.*/
  public void scale(double sx, double sy)
  {
    out.println("% scale(" + sx + ", " + sy + ")");
    AffineTransform Tx = new AffineTransform();
    Tx.scale(sx, sy);
    transform(Tx);
  }
}
