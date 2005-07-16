/* GradientPaintContext.java -- 
   Copyright (C) 2005, Free Software Foundation, Inc.

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

import java.awt.geom.Point2D;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.awt.PaintContext;
import java.awt.Color;

/**
 * A {@link PaintContext} used by the {@link GradientPaint} class.
 */
public class GradientPaintContext implements PaintContext 
{

  // This implementation follows the technique described in 
  // "Java(tm) 2D Graphics" by Jonathan Knudsen (O'Reilly 1999).
    
  /** The x-coordinate of the anchor point for color 1. */
  private final float x1;
    
  /** The y-coordinate of the anchor point for color 1. */
  private final float y1;
    
  /** Color 1. */
  private final Color c1;
    
  /** The x-coordinate of the anchor point for color 2. */
  private final float x2;
    
  /** The y-coordinate of the anchor point for color 2. */
  private final float y2;
    
  /** Color 2. */
  private final Color c2;
    
  /** A flag indicating whether the gradient is cyclic or acyclic. */
  private final boolean cyclic;
    
  /** The length of the gradient line - computed from the two anchor points. */
  private final double length; 

  /**
   * Creates a new instance.
   * 
   * @param x1  the x-coordinate for the anchor point for color 1.
   * @param y1  the y-coordinate for the anchor point for color 1.
   * @param c1  color 1.
   * @param x2  the x-coordinate for the anchor point for color 2.
   * @param y2  the y-coordinate for the anchor point for color 2.
   * @param c2  color 2.
   * @param cyclic  a flag that determines whether the gradient is cyclic
   *                or acyclic.
   */
  public GradientPaintContext(float x1, float y1, Color c1, 
                              float x2, float y2, Color c2, boolean cyclic) 
  {     
    this.x1 = x1;
    this.y1 = y1;
    this.c1 = c1;
    this.x2 = x2;
    this.y2 = y2;
    this.c2 = c2;
    this.cyclic = cyclic;
    length = Point2D.distance(x1, y1, x2, y2);
  }
    
  /**
   * Return the color model of this context. It may be different from the
   * hint specified during createContext, as not all contexts can generate
   * color patterns in an arbitrary model.
   *
   * @return the context color model
   */
  public ColorModel getColorModel() 
  {
    return ColorModel.getRGBdefault();   
  }

  /**
   * Return a raster containing the colors for the graphics operation.
   *
   * @param x the x-coordinate, in device space
   * @param y the y-coordinate, in device space
   * @param w the width, in device space
   * @param h the height, in device space
   * @return a raster for the given area and color
   */
  public Raster getRaster(int x, int y, int w, int h) {
    ColorModel cm = getColorModel();
    WritableRaster raster = cm.createCompatibleWritableRaster(w, h);
    int[] data = new int[w * h * 4];
    double pd2 = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);
    for (int r = 0; r < h; r++) {
      for (int c = 0; c < w; c++) {
        double u = 0.0;
        if (pd2 != 0) 
          u = (((x + c) - x1) * (x2 - x1) + ((y + r) - y1) * (y2 - y1)) 
                  / Math.sqrt(pd2);
        double ratio = u / length;
        if (cyclic)
          ratio = Math.abs(ratio - Math.floor((ratio + 1.0) / 2.0) * 2.0);
        else 
          ratio = Math.max(0.0, Math.min(1.0, ratio));
        int base = (r * w + c) * 4;
        data[base] = (int) (c1.getRed() + ratio * (c2.getRed() - c1.getRed()));
        data[base + 1] 
          = (int) (c1.getGreen() + ratio * (c2.getGreen() - c1.getGreen()));
        data[base + 2] 
          = (int) (c1.getBlue() + ratio * (c2.getBlue() - c1.getBlue()));
        data[base + 3] 
          = (int) (c1.getAlpha() + ratio * (c2.getAlpha() - c1.getAlpha()));
      }
    }
    raster.setPixels(0, 0, w, h, data);
    return raster;
  }

  /**
   * Release the resources allocated for the paint (none in this 
   * implementation).
   */
  public void dispose() {
    // nothing to do    
  }
    
}
