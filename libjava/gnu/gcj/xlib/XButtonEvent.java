/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

/**
 * Interprets data from an Xlib XButtonEvent into members of java
 * primitive types.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class XButtonEvent extends XEvent
{

  // Must match the definition in X.h:
  public static final int MASK_SHIFT   = 1<<0,
                          MASK_LOCK    = 1<<1,
                          MASK_CONTROL = 1<<2,
                          MASK_MOD1    = 1<<3,
                          MASK_MOD2    = 1<<4,
                          MASK_MOD3    = 1<<5,
                          MASK_MOD4    = 1<<6,
                          MASK_MOD5    = 1<<7;
 
  public XButtonEvent(XAnyEvent event)
  {
    super(event);

    // FIXME: Avoid double checking?
    if ((event.getType() != XAnyEvent.TYPE_BUTTON_PRESS) &&
	(event.getType() != XAnyEvent.TYPE_BUTTON_RELEASE))
      {
	throw new IllegalArgumentException("Wrong event type");
      }
    init();
  }

  native void init();
  
  public long time;
  public int  x;
  public int  y;
  public int  state;
  public int  button;
}



