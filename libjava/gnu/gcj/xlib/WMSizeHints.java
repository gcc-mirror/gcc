/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/** 
 * Size hints for an X11 window in its normal state. This class wraps
 * the Xlib XSizeHints stucture.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class WMSizeHints implements Cloneable
{
  public WMSizeHints()
  {
    init(null);
  }

  private native void init(WMSizeHints copyFrom);
  protected native void finalize();

  public Object clone() {
    WMSizeHints hints = (WMSizeHints) super.clone();
    // In case of an exception before the stucture is copied.
    hints.structure = null;

    hints.init(this);
    return hints;
  }

  public native void applyNormalHints(Window window);

  public native void setMinSize(int width, int height);
  public native void setMaxSize(int width, int height);
  
  RawData structure;
}
