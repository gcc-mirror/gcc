/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

/**
 * Interprets data from an Xlib XUnmapEvent.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class XUnmapEvent extends XEvent
{
  public XUnmapEvent(Display display, Window eventWindow,
		     Window unmappedWindow,
		     boolean fromConfigure)
  {
    super(XAnyEvent.TYPE_UNMAP_NOTIFY, display);
    getXAnyEvent().setWindow(eventWindow);
    setUnmappedWindow(unmappedWindow);
    setFromConfigure(fromConfigure);
  }

  public native void setUnmappedWindow(Window unmappedWindow);
  public native void setFromConfigure(boolean fromConfigure);
}



