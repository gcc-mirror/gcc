/* AlphaCompositeContext.java -- CompositeContext impl for AlphaComposite
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.java2d;

import java.awt.AWTError;
import java.awt.AlphaComposite;
import java.awt.CompositeContext;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

/**
 * A CompositeContext implementation for {@link AlphaComposite}.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class AlphaCompositeContext
  implements CompositeContext
{

  /**
   * The Composite object for which we perform compositing.
   */
  private AlphaComposite composite;

  /**
   * The source color model.
   */
  private ColorModel srcColorModel;

  /**
   * The destination color model.
   */
  private ColorModel dstColorModel;

  /**
   * The blending factor for the source.
   */
  private float fs;

  /**
   * The blending factor for the destination.
   */
  private float fd;

  /**
   * Creates a new AlphaCompositeContext.
   *
   * @param aComp the AlphaComposite object
   * @param srcCM the source color model
   * @param dstCM the destination color model
   */
  public AlphaCompositeContext(AlphaComposite aComp, ColorModel srcCM,
                               ColorModel dstCM)
  {
    composite = aComp;
    srcColorModel = srcCM;
    dstColorModel = dstCM;


    // Determine the blending factors according to the rule in the
    // AlphaComposite. For some rules the factors must be determined
    // dynamically because they depend on the actual pixel value.
    switch (composite.getRule())
    {
      case AlphaComposite.CLEAR:
        fs = 0.F;
        fd= 0.F;
        break;
      case AlphaComposite.DST:
        fs = 0.F;
        fd= 1.F;
        break;
      case AlphaComposite.DST_ATOP:
        fs = 1.F; // Determined later as 1 - alpha_dst;
        fd = 1.F; // Determined later as alpha_src;
        break;
      case AlphaComposite.DST_IN:
        fs = 0.F;
        fd = 0.F; // Determined later as alpha_src;
        break;
      case AlphaComposite.DST_OUT:
        fs = 0.F;
        fd = 0.F; // Determined later as 1 - alpha_src;
        break;
      case AlphaComposite.DST_OVER:
        fs = 1.F; // Determined later as 1 - alpha_dst.
        fd= 1.F;
        break;
      case AlphaComposite.SRC:
        fs = 1.F;
        fd= 0.F;
        break;
      case AlphaComposite.SRC_ATOP:
        fs = 1.F; // Determined later as alpha_dst;
        fd = 1.F; // Determined later as 1 - alpha_src;
        break;
      case AlphaComposite.SRC_IN:
        fs = 0.F; // Determined later as alpha_dst;
        fd = 0.F;
        break;
      case AlphaComposite.SRC_OUT:
        fs = 0.F; // Determined later as 1 - alpha_dst;
        fd = 0.F;
        break;
      case AlphaComposite.SRC_OVER:
        fs = 1.F;
        fd= 1.F; // Determined later as 1 - alpha_src.
        break;
      case AlphaComposite.XOR:
        fs = 1.F; // Determined later as 1 - alpha_dst.
        fd= 1.F; // Determined later as 1 - alpha_src.
        break;
      default:
        throw new AWTError("Illegal AlphaComposite rule");
    }

  }

  /**
   * Releases all resources held by this composite object.
   */
  public void dispose()
  {
    // Nothing to do here yet.
  }

  /**
   * Performs compositing according to the rules specified in the
   * AlphaComposite from the constructor.
   */
  public void compose(Raster src, Raster dstIn, WritableRaster dstOut)
  {

    // TODO: This implementation is very general and highly inefficient. There
    // are two possible ways to optimize this:
    // 1. Special cased implementations for common ColorModels and transfer
    //    types.
    // 2. Native implementation.

    int x0 = src.getMinX();
    int y0 = src.getMinY();
    int width = src.getWidth();
    int height = src.getHeight();
    int x1 = x0 + width;
    int y1 = y0 + height;

    Object srcPixel = null;
    Object dstPixel = null;

    // Prepare the array that holds the color and alpha components of the
    // source pixels.
    float[] srcComponents;
    int srcComponentsLength = srcColorModel.getNumComponents();
    if (! srcColorModel.hasAlpha())
      srcComponentsLength += 1;
    srcComponents = new float[srcComponentsLength];

    // Prepare the array that holds the color and alpha components of the
    // destination pixels.
    float[] dstComponents;
    int dstComponentsLength = dstColorModel.getNumComponents();
    if (! dstColorModel.hasAlpha())
      dstComponentsLength += 1;
    dstComponents = new float[dstComponentsLength];

    if (srcComponentsLength != dstComponentsLength)
      throw new AWTError("The color models of the source and destination have"
                         + "incompatible number of color components");

    int srcTransferType = srcColorModel.getTransferType();
    int dstTransferType = dstColorModel.getTransferType();

    for (int y = y0; y < y1; y++)
      {
        for (int x = x0; x < x1; x++)
          {
            // Fetch source pixel.
            srcPixel = src.getDataElements(x, y, (int[]) srcPixel);
            // Fetch destination pixel.
            dstPixel = dstIn.getDataElements(x, y, dstPixel);
            // Get normalized components. This is the only type that is
            // guaranteed to be supported by all ColorModels.
            srcComponents =
              srcColorModel.getNormalizedComponents(srcPixel, srcComponents, 0);
            if (! srcColorModel.hasAlpha())
              srcComponents[srcComponentsLength - 1] = 1.0F;
            dstComponents =
              dstColorModel.getNormalizedComponents(dstPixel, dstComponents, 0);
            if (! dstColorModel.hasAlpha())
              dstComponents[dstComponentsLength - 1] = 1.0F;

            // Prepare the input.
            float compositeAlpha = composite.getAlpha();
            srcComponents[srcComponentsLength - 1] *= compositeAlpha;
            if (srcColorModel.isAlphaPremultiplied())
              {
                for (int i = srcComponentsLength - 2; i >= 0; i--)
                  srcComponents[i] *= compositeAlpha;
              }
            else
              {
                for (int i = srcComponentsLength - 2; i >= 0; i--)
                  srcComponents[i] *= srcComponents[srcComponentsLength - 1];
              }
            if (! dstColorModel.isAlphaPremultiplied())
              {
                for (int i = dstComponentsLength - 2; i >= 0; i--)
                  dstComponents[i] *= dstComponents[dstComponents.length - 1];
              }

            // Determine the blending factors according to the rule in the
            // AlphaComposite. For some rules the factors must be determined
            // dynamically because they depend on the actual pixel value.
            float srcAlpha = srcComponents[srcComponentsLength - 1];
            float dstAlpha = dstComponents[dstComponentsLength - 1];
            switch (composite.getRule())
            {
              case AlphaComposite.DST_ATOP:
                fs = 1.F - dstAlpha;
                fd = srcAlpha;
                break;
              case AlphaComposite.DST_IN:
                fd = srcAlpha;
                break;
              case AlphaComposite.DST_OUT:
                fd = 1.F - srcAlpha;
                break;
              case AlphaComposite.DST_OVER:
                fs = 1.F - dstAlpha;
                break;
              case AlphaComposite.SRC_ATOP:
                fs = srcAlpha;
                fd = 1.F - srcAlpha;
                break;
              case AlphaComposite.SRC_IN:
                fs = dstAlpha;
                break;
              case AlphaComposite.SRC_OUT:
                fs = 1.F - dstAlpha;
                break;
              case AlphaComposite.SRC_OVER:
                fd= 1.F - srcAlpha;
                break;
              case AlphaComposite.XOR:
                fs = 1.F - dstAlpha;
                fd= 1.F - srcAlpha;
                break;
              default:
                // For the other cases the factors have already been determined
                // in the constructor.
            }

            // Apply the blending equation to the pixels.
            for (int i = 0; i < srcComponentsLength; i++)
              {
                dstComponents[i] = srcComponents[i] * fs
                                   + dstComponents[i] * fd;
              }

            // Convert the result back when the destination is not
            // alpha-premultiplied.
            dstAlpha = dstComponents[dstComponentsLength - 1];
            if (!dstColorModel.isAlphaPremultiplied() && dstAlpha != 0.F)
              {
                for (int i = 0; i < dstComponentsLength - 1; i++)
                  {
                    dstComponents[i] = dstComponents[i] / dstAlpha;
                  }
              }

            // Store the result in the destination raster.
            dstPixel = dstColorModel.getDataElements(dstComponents, 0,
                                                     dstPixel);
            dstOut.setDataElements(x, y, dstPixel);
          } // End X loop.
      } // End Y loop.
  }

}
