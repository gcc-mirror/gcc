/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/** 
 * Mutable event structure that can contain any data from any event
 * type.  Events can be constructed or loaded from the event queue.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public final class XAnyEvent
{
  // Must match the definitions in X.h:
  public static final int TYPE_BUTTON_PRESS     =  4,
                          TYPE_BUTTON_RELEASE   =  5,
	                  TYPE_EXPOSE           = 12,
	                  TYPE_UNMAP_NOTIFY     = 18,
	                  TYPE_MAP_NOTIFY       = 19,
	                  TYPE_REPARENT_NOTIFY  = 21,
	                  TYPE_CONFIGURE_NOTIFY = 22,
	                  TYPE_CLIENT_MESSAGE   = 33;
    
  // Must match the definitions in X.h:
  public final static long MASK_SUBSTRUCTURE_NOTIFY   = 1L<<19,
	                   MASK_SUBSTRUCTURE_REDIRECT = 1L<<20;

  XAnyEvent(Display display)
  {
    this.display = display;
    init();
  }

  private native void init();
  protected native void finalize();

  /**
   * Load next event into the event structure.
   */
  public native void loadNext();

  public native int getType();
  public native void setType(int type);

  public native Window getWindow();
  public native void setWindow(Window window);
  
  /**
   * @returns the number of the last request processed by the server.
   */
  public native long getSerial();

  public native void send(Window destination, boolean propagate,
			  long mask);

  RawData structure;
  Display display;

  public String toString()
  {
    if (structure == null)
      return getClass().getName() + "[no-structure]";

    return getClass().getName() +
      "[type=" + getType() +
      ",window=" + getWindow() + "]";
  }
}
