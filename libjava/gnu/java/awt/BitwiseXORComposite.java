/* BitwiseXORComposite.java -- Composite for emulating old-style XOR.
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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


package gnu.java.awt;

import java.awt.Color;
import java.awt.Composite;
import java.awt.CompositeContext;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;


/**
 * A composite for emulating traditional bitwise XOR of pixel values.
 * Please note that this composite does <i>not</i> implement the Porter-Duff
 * XOR operator, but an exclusive or of overlapping subpixel regions.
 *
 * <p><img src="doc-files/BitwiseXORComposite-1.png" width="545"
 * height="138" alt="A screen shot of BitwiseXORComposite in action"
 * />
 *
 * <p>The above screen shot shows the result of applying six different
 * BitwiseXORComposites. They were constructed with the colors colors
 * white, blue, black, orange, green, and brown, respectively. Each
 * composite was used to paint a fully white rectangle on top of the
 * blue bar in the background.
 * 
 * <p>The purpose of this composite is to support the {@link
 * Graphics#setXORMode(Color)} method in composite-aware graphics
 * implementations. Applications typically would use
 * <code>setXORMode</code> for drawing &#x201c;highlights&#x201d; such
 * as text selections or cursors by inverting colors temporarily and
 * then inverting them back.
 *
 * <p>A concrete <code>Graphics</code> implementation may contain
 * the following code:
 *
 * <p><pre> public void setXORMode(Color xorColor)
 * {
 *   setComposite(new gnu.java.awt.BitwiseXORComposite(xorColor));
 * }
 *
 * public void setPaintMode()
 * {
 *   setComposite(java.awt.AlphaComposite.SrcOver);
 * }</pre>
 *
 * @author Graydon Hoare (graydon@redhat.com)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class BitwiseXORComposite
  implements Composite
{
  /**
   * The color whose RGB value is xor-ed with the values of each
   * pixel.
   */
  protected Color xorColor;

  
  /**
   * Constructs a new composite for xor-ing the pixel value.
   *
   * @param xorColor the color whose pixel value will be bitwise
   * xor-ed with the source and destination pixels.
   */
  public BitwiseXORComposite(Color xorColor)
  {
    this.xorColor = xorColor;
  }


  /**
   * Creates a context object for performing the compositing
   * operation. Several contexts may co-exist for one composite; each
   * context may simultaneously be called from concurrent threads.
   *
   * @param srcColorModel the color model of the source.
   * @param dstColorModel the color model of the destination.
   * @param hints hints for choosing between rendering alternatives.
   */
  public CompositeContext createContext(ColorModel srcColorModel,
                                        ColorModel dstColorModel,
                                        RenderingHints hints)
  {
    if (IntContext.isSupported(srcColorModel, dstColorModel, hints))
      return new IntContext(srcColorModel, xorColor);

    return new GeneralContext(srcColorModel, dstColorModel, xorColor);
  }

  
  /**
   * A fallback CompositeContext that performs bitwise XOR of pixel
   * values with the pixel value of the specified <code>xorColor</code>.
   *
   * <p>Applying this CompositeContext on a 1024x1024 BufferedImage of
   * <code>TYPE_INT_RGB</code> took 611 ms on a lightly loaded 2.4 GHz
   * Intel Pentium 4 CPU running Sun J2SE 1.4.1_01 on GNU/Linux
   * 2.4.20. The timing is the average of ten runs on the same
   * BufferedImage. Since the measurements were taken with {@link
   * System#currentTimeMillis()}, they are rather inaccurate.
   *
   * @author Graydon Hoare (graydon@redhat.com)
   */
  private static class GeneralContext
    implements CompositeContext
  {
    ColorModel srcColorModel;
    ColorModel dstColorModel;
    Color xorColor;

    public GeneralContext(ColorModel srcColorModel,
                          ColorModel dstColorModel,
                          Color xorColor)
    {
      this.srcColorModel = srcColorModel;
      this.dstColorModel = dstColorModel;
      this.xorColor = xorColor;
    }


    public void compose(Raster src, Raster dstIn, WritableRaster dstOut)
    {
      Rectangle srcRect = src.getBounds();
      Rectangle dstInRect = dstIn.getBounds();
      Rectangle dstOutRect = dstOut.getBounds();
      
      int xp = xorColor.getRGB();
      int w = Math.min(Math.min(srcRect.width, dstOutRect.width),
                       dstInRect.width);
      int h = Math.min(Math.min(srcRect.height, dstOutRect.height),
                       dstInRect.height);

      Object srcPix = null, dstPix = null, rpPix = null;

      // Re-using the rpPix object saved 1-2% of execution time in
      // the 1024x1024 pixel benchmark.

      for (int y = 0; y < h; y++)
      {
        for (int x = 0; x < w; x++)
        {
          srcPix = src.getDataElements(x + srcRect.x, y + srcRect.y, srcPix);
          dstPix = dstIn.getDataElements(x + dstInRect.x, y + dstInRect.y,
                                         dstPix);
          int sp = srcColorModel.getRGB(srcPix);
          int dp = dstColorModel.getRGB(dstPix);
          int rp = sp ^ xp ^ dp;
          dstOut.setDataElements(x + dstOutRect.x, y + dstOutRect.y, 
                                 dstColorModel.getDataElements(rp, rpPix));
        }
      }
    }


    /**
     * Disposes any cached resources. The default implementation does
     * nothing because no resources are cached.
     */
    public void dispose()
    {
    }
  }


  /**
   * An optimized CompositeContext that performs bitwise XOR of
   * <code>int</code> pixel values with the pixel value of a specified
   * <code>xorColor</code>.  This CompositeContext working only for
   * rasters whose transfer format is {@link DataBuffer#TYPE_INT}.
   *
   * <p>Applying this CompositeContext on a 1024x1024 BufferedImage of
   * <code>TYPE_INT_RGB</code> took 69 ms on a lightly loaded 2.4 GHz
   * Intel Pentium 4 CPU running Sun J2SE 1.4.1_01 on GNU/Linux
   * 2.4.20. The timing is the average of ten runs on the same
   * BufferedImage. Since the measurements were taken with {@link
   * System#currentTimeMillis()}, they are rather inaccurate.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static class IntContext
    extends GeneralContext
  {
    public IntContext(ColorModel colorModel, Color xorColor)
    {
      super(colorModel, colorModel, xorColor);
    }


    public void compose(Raster src, Raster dstIn,
                        WritableRaster dstOut)
    {
      int aX, bX, dstX, aY, bY, dstY, width, height;
      int xorPixel;
      int[] srcLine, dstLine;

      aX = src.getMinX();
      aY = src.getMinY();
      bX = dstIn.getMinX();
      bY = dstIn.getMinY();
      dstX = dstOut.getMinX();
      dstY = dstOut.getMinY();
      width = Math.min(Math.min(src.getWidth(), dstIn.getWidth()),
                       dstOut.getWidth());
      height = Math.min(Math.min(src.getHeight(), dstIn.getHeight()),
                        dstOut.getHeight());
      if ((width < 1) || (height < 1))
        return;

      srcLine = new int[width];
      dstLine = new int[width];
      
      /* We need an int[] array with at least one element here;
       * srcLine is as good as any other.
       */
      srcColorModel.getDataElements(this.xorColor.getRGB(), srcLine);
      xorPixel = srcLine[0];

      for (int y = 0; y < height; y++)
      {
        src.getDataElements(aX, y + aY, width, 1, srcLine);
        dstIn.getDataElements(bX, y + bY, width, 1, dstLine);

        for (int x = 0; x < width; x++)
          dstLine[x] ^= srcLine[x] ^ xorPixel;

        dstOut.setDataElements(dstX, y + dstY, width, 1, dstLine);
      }
    }

    
    /**
     * Determines whether an instance of this CompositeContext would
     * be able to process the specified color models.
     */
    public static boolean isSupported(ColorModel srcColorModel,
                                      ColorModel dstColorModel,
                                      RenderingHints hints)
    {
      // FIXME: It would be good if someone could review these checks.
      // They probably need to be more restrictive.

      int transferType;

      transferType = srcColorModel.getTransferType();
      if (transferType != dstColorModel.getTransferType())
        return false;

      if (transferType != DataBuffer.TYPE_INT)
        return false;

      return true;
    }
  }
}
