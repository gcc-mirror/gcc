/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/**
 * An X11 color map resource.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public final class Colormap extends XID
{
  Screen screen;
  
  public static final byte FLAG_SHARED = 1;
  public static final byte FLAG_NOT_SHARED = 2;
  
  public Colormap(Screen screen, int xid)
  {
    super(screen.getDisplay(), xid);
    this.screen = screen;
  }
  
  /**
   * Allocate color pixel.
   *
   * @param color The color to be allocated.  If allocation is
   * successful, this object will be modified to reflect the actual
   * color that was allocated.
   *
   * @return the pixel value of the allocated color.
   */
  public native long allocateColorPixel(XColor color);

  /**
   * Allocate a color consisting of the given RGB-triplet.
   *
   * @return a color object describing the allocated color.
   */
  public XColor allocateColor(int r, int g, int b)
  {
    XColor color = new XColor(r, g, b);
    allocateColorPixel(color);
    
    return color;
  }

  /**
   * Get an array of all colors that currently resides in shared (read
   * only) color-cells in this color map.
   */
  public native XColor[] getSharedColors();


  /**
   * Get all colors currently residing in this color map.  Colors that
   * are shared (read only) are marked as such by the color flags.
   * The indexes of the returned array will correspond to the
   * colorcells of the color map.  Given a color <code>XColor
   * color</code> from a given color-cell, the expression
   * <code>color.getFlags() == Colormap.FLAG_SHARED</code> will check
   * whether the color-cell is shared.
   */
  public native XColor[] getXColors();

  /**
   * Convenience method used by native code to create fully
   * initialized arrays of XColor objects.
   */
  private XColor[] newXColorArray(int n)
  {
    XColor[] array = new XColor[n];
    for (int i=0; i<n; i++)
      array[i] = new XColor();
    return array;
  }
}
