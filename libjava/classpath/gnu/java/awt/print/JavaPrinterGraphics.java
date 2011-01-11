/* JavaPrinterGraphics.java -- AWT printer rendering class.
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

import gnu.java.awt.peer.gtk.CairoSurface;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.image.ImageObserver;
import java.awt.image.PixelGrabber;
import java.awt.print.PageFormat;
import java.awt.print.Pageable;
import java.awt.print.Paper;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterGraphics;
import java.awt.print.PrinterJob;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.text.AttributedCharacterIterator;

/**
 * Graphics context to draw to PostScript.
 *
 * @author Sven de Marothy
 */
public class JavaPrinterGraphics extends Graphics implements PrinterGraphics
{

  /**
   * The used graphics context.
   */
  private Graphics g;

  /**
   * The associated printer job.
   */
  private PrinterJob printerJob;

  /**
   * Rendering resolution
   */
  private static final double DPI = 72.0;

  /**
   * Rendered image size.
   */
  private int xSize, ySize;

  /**
   * The image to render to.
   */
  private Image image;

  public JavaPrinterGraphics( PrinterJob printerJob )
  {
    this.printerJob = printerJob;
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

        PrintWriter out = new PrintWriter
          (new BufferedWriter
            (new OutputStreamWriter
             (new FileOutputStream(temp), "ISO8859_1"), 1000000));

        writePSHeader(out);

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

  /**
   * Spools a single page, returns NO_SUCH_PAGE unsuccessful,
   * PAGE_EXISTS if it was.
   */
  public int spoolPage(PrintWriter out,
                       Printable printable,
                       PageFormat pageFormat,
                       int index) throws IOException, PrinterException
  {
    initImage( pageFormat );
    if(printable.print(this, pageFormat, index) == Printable.NO_SUCH_PAGE)
      return Printable.NO_SUCH_PAGE;
    g.dispose();
    g = null;
    writePage( out, pageFormat );
    return Printable.PAGE_EXISTS;
  }

  private void initImage(PageFormat pageFormat)
  {
    // Create a really big image and draw to that.
    xSize = (int)(DPI*pageFormat.getWidth()/72.0);
    ySize = (int)(DPI*pageFormat.getHeight()/72.0);

    // Swap X and Y sizes if it's a Landscape page.
    if( pageFormat.getOrientation() != PageFormat.PORTRAIT )
      {
        int t = xSize;
        xSize = ySize;
        ySize = t;
      }

    // FIXME: This should at least be BufferedImage.
    // Fix once we have a working B.I.
    // Graphics2D should also be supported of course.
    image = CairoSurface.getBufferedImage(xSize, ySize);

    g = image.getGraphics();
    setColor(Color.white);
    fillRect(0, 0, xSize, ySize);
    setColor(Color.black);
  }

  private void writePSHeader(PrintWriter out)
  {
    out.println("%!PS-Adobe-3.0");
    out.println("%%Title: "+printerJob.getJobName());
    out.println("%%Creator: GNU Classpath ");
    out.println("%%DocumentData: Clean8Bit");

    out.println("%%DocumentNeededResources: font Times-Roman Helvetica Courier");
    //    out.println("%%Pages: "+);  // FIXME # pages.
    out.println("%%EndComments");

    out.println("%%BeginProlog");
    out.println("%%EndProlog");
    out.println("%%BeginSetup");

    // FIXME: Paper name
    // E.g. "A4" "Letter"
    //    out.println("%%BeginFeature: *PageSize A4");

    out.println("%%EndFeature");

    out.println("%%EndSetup");

    //    out.println("%%Page: 1 1");
  }

  private void writePage(PrintWriter out, PageFormat pageFormat)
  {
    out.println("%%BeginPageSetup");

    Paper p = pageFormat.getPaper();
    double pWidth = p.getWidth();
    double pHeight = p.getHeight();

    if( pageFormat.getOrientation() == PageFormat.PORTRAIT )
      out.println( "%%Orientation: Portrait" );
    else
      {
        out.println( "%%Orientation: Landscape" );
        double t = pWidth;
        pWidth = pHeight;
        pHeight = t;
      }

    out.println("gsave % first save");

    // 595x842; 612x792 respectively
    out.println("<< /PageSize [" +pWidth + " "+pHeight+ "] >> setpagedevice");

    // invert the Y axis so that we get screen-like coordinates instead.
    AffineTransform pageTransform = new AffineTransform();
    if( pageFormat.getOrientation() == PageFormat.REVERSE_LANDSCAPE )
      {
        pageTransform.translate(pWidth, pHeight);
        pageTransform.scale(-1.0, -1.0);
      }
    concatCTM(out, pageTransform);
    out.println("%%EndPageSetup");

    out.println("gsave");


    // Draw the image
    out.println(xSize+" "+ySize+" 8 [1 0 0 -1 0 "+ySize+" ]");
    out.println("{currentfile 3 string readhexstring pop} bind");
    out.println("false 3 colorimage");
    int[] pixels = new int[xSize * ySize];
    PixelGrabber pg = new PixelGrabber(image, 0, 0, xSize, ySize, pixels, 0, xSize);

    try {
      pg.grabPixels();
    } catch (InterruptedException e) {
      out.println("% Bug getting pixels!");
    }

    int n = 0;
    for (int j = 0; j < ySize; j++) {
      for (int i = 0; i < xSize; i++) {
        out.print( colorTripleHex(pixels[j * xSize + i]) );
        if(((++n)%11) == 0) out.println();
      }
    }

    out.println();
    out.println("%%EOF");
    out.println("grestore");
    out.println("showpage");
  }

  /**
   * Get a nonsperated hex RGB triple, e.g. FFFFFF = white
   */
  private String colorTripleHex(int num){
    String s = "";

    try {
      s = Integer.toHexString( ( num & 0x00FFFFFF ) );
      if( s.length() < 6 )
        {
          s = "000000"+s;
          return s.substring(s.length()-6);
        }
    } catch (Exception e){
      s = "FFFFFF";
    }

    return s;
  }

  private void concatCTM(PrintWriter out, AffineTransform Tx){
    double[] matrixElements = new double[6];
    Tx.getMatrix(matrixElements);

    out.print("[ ");
    for(int i=0;i<6;i++)
      out.print(matrixElements[i]+" ");
    out.println("] concat");
  }

  //-----------------------------------------------------------------------------
  /**
   * PrinterGraphics method - Returns the printer job associated with this object.
   */
  public PrinterJob getPrinterJob()
  {
    return printerJob;
  }

  /**
   * The rest of the methods here are just pass-throughs to g.
   */
  public void clearRect(int x, int y, int width, int height)
  {
    g.clearRect(x, y, width, height);
  }

  public void clipRect(int x, int y, int width, int height)
  {
    g.clipRect(x, y, width, height);
  }

  public void copyArea(int x, int y, int width, int height, int dx, int dy)
  {
    g.copyArea(x, y, width, height, dx, dy);
  }

  public Graphics create()
  {
    return g.create();
  }

  public void dispose()
  {
  }

  public void drawArc(int x, int y, int width, int height, int startAngle,
                      int arcAngle)
  {
    g.drawArc(x, y, width, height, startAngle, arcAngle);
  }

  public boolean drawImage(Image img, int x, int y, Color bgcolor,
                           ImageObserver observer)
  {
    return g.drawImage(img, x, y, bgcolor, observer);
  }

  public boolean drawImage(Image img, int x, int y, ImageObserver observer)
  {
    return g.drawImage(img, x, y, observer);
  }

  public boolean drawImage(Image img, int x, int y, int width, int height,
                           Color bgcolor, ImageObserver observer)
  {
    return g.drawImage(img, x, y, width, height, bgcolor, observer);
  }

  public boolean drawImage(Image img, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    return g.drawImage(img, x, y, width, height, observer);
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2, Color bgcolor,
                           ImageObserver observer)
  {
    return g.drawImage(img, dx1,  dy1,  dx2,  dy2,
                     sx1,  sy1,  sx2,  sy2, bgcolor, observer);
  }

  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2, ImageObserver observer)
  {
    return g.drawImage(img, dx1,  dy1,  dx2,  dy2,
                     sx1,  sy1,  sx2,  sy2, observer);
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    g.drawLine(x1, y1, x2, y2);
  }

  public void drawOval(int x, int y, int width, int height)
  {
    g.drawOval(x, y, width, height);
  }

  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    g.drawPolygon(xPoints, yPoints, nPoints);
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints)
  {
    g.drawPolyline(xPoints, yPoints, nPoints);
  }

  public void drawRoundRect(int x, int y, int width, int height,
                            int arcWidth, int arcHeight)
  {
    g.drawRoundRect(x, y, width, height, arcWidth, arcHeight);
  }

  public void drawString(AttributedCharacterIterator iterator, int x, int y)
  {
    g.drawString(iterator, x, y);
  }

  public void drawString(String str, int x, int y)
  {
    g.drawString(str, x, y);
  }

  public void fillArc(int x, int y, int width, int height,
                      int startAngle, int arcAngle)
  {
    g.fillArc(x, y, width, height, startAngle, arcAngle);
  }

  public void fillOval(int x, int y, int width, int height)
  {
    g.fillOval(x, y, width, height);
  }

  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    g.fillPolygon(xPoints, yPoints, nPoints);
  }

  public void fillRect(int x, int y, int width, int height)
  {
    g.fillRect(x, y, width, height);
  }

  public void fillRoundRect(int x, int y, int width, int height,
                            int arcWidth, int arcHeight)
  {
    g.fillRoundRect(x, y, width, height, arcWidth, arcHeight);
  }

  public Shape getClip()
  {
    return g.getClip();
  }

  public Rectangle getClipBounds()
  {
    return g.getClipBounds();
  }

  public Color getColor()
  {
    return g.getColor();
  }

  public Font getFont()
  {
    return g.getFont();
  }

  public FontMetrics getFontMetrics(Font f)
  {
    return g.getFontMetrics(f);
  }

  public void setClip(int x, int y, int width, int height)
  {
    g.setClip(x, y, width, height);
  }

  public void setClip(Shape clip)
  {
    g.setClip(clip);
  }

  public void setColor(Color c)
  {
    g.setColor(c);
  }

  public void setFont(Font font)
  {
    g.setFont(font);
  }

  public void setPaintMode()
  {
    g.setPaintMode();
  }

  public void setXORMode(Color c1)
  {
    g.setXORMode(c1);
  }

  public void translate(int x, int y)
  {
    g.translate(x, y);
  }
}
