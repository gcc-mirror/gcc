/* GrayFilter.java -- Java class for filtering Pixels to produce Gray Pictures
   Copyright (C) 1999, 2004  Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.FilteredImageSource;
import java.awt.image.RGBImageFilter;

/**
 * Produces grayscale images out of colored images. This is used to provide
 * default disabled icons for buttons.
 *
 * @author original author unknown
 */
public class GrayFilter extends RGBImageFilter
{
  private boolean b;
  private double p;

  /**
   * Create a GrayFilter. If b is true then brighten. Also, indicate how much
   * gray.
   *    
   * @param b if brighten
   * @param p percent of gray, 0 - 100
   */
  public GrayFilter(boolean b, int p)
  {
    this.b = b; //FIXME - HANDLE THIS
    this.p = (1. - (p / 100.)) / 3.;
  }

  /**
   * Create grayed image
   *
   * @param src image to gray
   *
   * @return a grayed image
   */
  public static Image createDisabledImage(Image src)
  {
    return (Toolkit.getDefaultToolkit().createImage(new FilteredImageSource(
        src.getSource(), new GrayFilter(true, 0))));
  }
  
  /**
   * Filter RGB to gray
   */
  public int filterRGB(int x, int y, int rgb)
  {
    int alpha = 0xff000000 & rgb;
    int red = (0xff0000 & rgb) >> 16;
    int green = (0xff00 & rgb) >> 8;
    int blue = (0xff & rgb);
    int gray = (int) ((0.299 * red + 0.587 * green + 0.114 * blue) * p);
    if (b)
      gray = Math.min(gray + 128, 255);
    return gray | gray << 8 | gray << 16 | alpha ;
  }
}
