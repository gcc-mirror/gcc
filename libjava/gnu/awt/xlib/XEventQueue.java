/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.*;

import gnu.gcj.xlib.Display;

/**
 * The only difference here from a standard EventQueue is that the X
 * display connection is flushed before waiting for more events.
 */
public class XEventQueue extends EventQueue
{
  Display display;
  
  public XEventQueue(Display display)
  {
    this.display = display;
  }
  
  public AWTEvent getNextEvent() throws InterruptedException
  {
    if ((peekEvent() == null) && (display != null))
      display.flush();
    return super.getNextEvent();
  }
}
