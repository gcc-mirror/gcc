/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;
import java.awt.Rectangle;

/**
 * Describes a clip that is used to constrain drawing using a GC
 * within a specific region. Currently it supports clip regions
 * consisting of the union of multiple rectangles. Other clip forms
 * may be implented later. This class is used internally by the GC
 * class, and wraps a native XRectVector[].
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
final class Clip
{
  public Clip(Rectangle[] rects)
  {
    init(rects);
  }

  private native void init(Rectangle[] rects);

  public native void finalize();

  RawData xrects;
}
