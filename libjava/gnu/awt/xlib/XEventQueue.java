/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import gnu.gcj.xlib.Display;
import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Container;
import java.awt.EventQueue;
import java.awt.event.ComponentEvent;
import java.awt.event.ContainerEvent;

/**
 * The main difference here from a standard EventQueue is that the X
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
    AWTEvent event = super.getNextEvent();
    if (event != null)
    {
      switch (event.getID ())
      {
        case ContainerEvent.COMPONENT_ADDED:
        {
          /* If a component has been added to a container, it needs to be
           * invalidated, to ensure that it ultimately gets an addNotify.
           * If it's not invalidated, the component will never display in 
           * an already-showing container (probably applies only to CardLayout).
           * Perhaps this code should be in java.awt, but the problem only seems 
           * to happen with xlib peers (not with gtk peers) so it's here instead.
           */
          ContainerEvent ce = (ContainerEvent)event;
          ce.getChild ().invalidate ();
          ce.getContainer ().validate ();
        }
        break;

        case ComponentEvent.COMPONENT_RESIZED:
        {
          ComponentEvent ce = (ComponentEvent)event;
          // FIXME: there may be opportunities to coalesce resize events
          ce.getComponent ().validate ();
        }
        break;

        case ComponentEvent.COMPONENT_SHOWN:
        {
          ComponentEvent ce = (ComponentEvent)event;
          Component comp = ce.getComponent ();
          if (!comp.isValid ())
          {
            /* Try to validate, going up the tree to the highest-level invalid
             * Container.  The idea is to ensure that addNotify gets called for
             * any non-top-level component being shown, to make it create a peer.
             */
            Container parent = comp.getParent ();
            while (parent != null)
            {
              Container next = parent.getParent ();
              if (next == null || next.isValid ())
              {
                parent.validate ();
                break;
              }
              else
                parent = next;
            }
            if (comp instanceof Container)
              comp.validate ();
          }
          comp.repaint ();
        }
        break;
        
        default:
          break;
      }
    }
    return event;
  }
}
