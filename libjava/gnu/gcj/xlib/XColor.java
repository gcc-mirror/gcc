/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/**
 * A color or color-cell on the X server.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public final class XColor
{
  public XColor(int r, int g, int b)
  {
    this();
    setRGB(r, g, b);
  }

  public XColor()
  {
    init();
  }

  private native void init();
  protected native void finalize();

  public final native void setRGB(int r, int g, int b);
  public final native int getRed();
  public final native int getGreen();
  public final native int getBlue();
  public final native byte getFlags();
  public final native long getPixelValue();
  
  RawData structure = null;
}
