/* ColorPaintContext.java -- context for painting solid colors
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.image.ColorModel;
import java.awt.image.Raster;

/**
 * This class provides a paint context which will fill a rectanglar region of
 * a raster scan with the given color. However, it is not yet completely
 * implemented.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 */
class ColorPaintContext implements PaintContext
{
  /**
   * The color to fill any raster with. Package visible for use in
   * SystemColor.
   */
  final int color;
  final ColorModel colorModel;

  private ColorRaster cachedRaster;

  
  /**
   * Create the context for a given color.
   *
   * @param c The solid color to use.
   */
  ColorPaintContext(int colorRGB)
  {
    this(ColorModel.getRGBdefault(), colorRGB);
  }
  
  /**
   * Create the context for a given color.
   *
   * @param cm The color model of this context. 
   * @param c The solid color to use.
   */
  ColorPaintContext(ColorModel cm,int colorRGB)
  {
    color = colorRGB;
    colorModel = cm;
  }

  /**
   * Release the resources allocated for the paint. As the color is constant,
   * there aren't any resources.
   */
  public void dispose()
  {
  }

  /**
   * Return the color model of this context. 
   *
   * @return the context color model
   */
  public ColorModel getColorModel()
  {
    return colorModel;
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
  public Raster getRaster(int x, int y, int width, int height)
  {
   if(  cachedRaster == null 
       || cachedRaster.getWidth() < width
       || cachedRaster.getHeight() < height)
   {
     cachedRaster = new ColorRaster(colorModel, 0, 0, width, height, color);
   }
   return cachedRaster.createChild(0 ,0 ,width ,height ,x ,y , null);
  }
  
  /**
   * A ColorRaster is a raster that is completely filled with one color. The 
   * data layout is taken from the color model given to the constructor.
   */
  private class ColorRaster extends Raster
  {
    
    /**
     * Create a raster that is compaltible with the given color model and 
     * filled with the given color.
     * @param cm The color model for this raster.
     * @param x The smallest horizontal corrdinate in the raster.
     * @param y The smallest vertical coordinate in the raster.
     * @param width The width of the raster.
     * @param height The height of the raster.
     * @param rgbPixel The RGB value of the color for this raster.
     */
    ColorRaster(ColorModel cm,int x, int y, int width, int height, int rgbPixel)
    {         
      super(cm.createCompatibleSampleModel(width,height),new Point(x,y));
      Object pixel = cm.getDataElements(rgbPixel,null);
      getSampleModel().setDataElements(0, 0,
                                       width, height,
                                       multiplyData(pixel,null,width*height),
                                       dataBuffer);
    }
    
    
    
    private Object multiplyData(Object src, Object dest, int factor)
    {
      Object from;
      int srcLength = 0;
      if (src instanceof byte[])
      {
        srcLength = ((byte[])src).length;
        
        if (dest == null) dest = new byte[factor * srcLength];
      }
      else if (src instanceof short[])
      {
        srcLength = ((short[])src).length;
        if (dest == null) dest = new short[factor * srcLength];
      }
      else if (src instanceof int[])
      {
        srcLength = ((int[]) src).length;
        if (dest == null) dest = new int[factor * srcLength];
      }
      else
      {
        throw new ClassCastException("Unknown data buffer type");
      }
      
      System.arraycopy(src,0,dest,0,srcLength);
      
      int count = 1;
      while(count*2 < factor)
      {
        System.arraycopy(dest, 0, dest, count * srcLength, count*srcLength);
        count *= 2; 
      }
      
      if(factor > count)
        System.arraycopy(dest,0, dest, count * srcLength, 
                         (factor - count) * srcLength );
      
      return dest;
    }
    
  }
  
} // class ColorPaintContext
