/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/**
 * Base class for interpreters of specific X event types.  For methods
 * concerning all X events, see XAnyEvent.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class XEvent
{
  public XEvent(XAnyEvent event)
  {
    this.event = event;
  }
  
  public XEvent(int type, Display display)
  {
    this(new XAnyEvent(display));
    event.setType(type);
  }

  XAnyEvent event;

  public XAnyEvent getXAnyEvent()
  {
    return event;
  }

  public String toString()
  {
    if (event == null)
      return super.toString();
    return event.toString();
  }
}
