/* AWTEventListenerProxy.java -- wrapper/filter for AWTEventListener
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.awt.event;

import java.awt.AWTEvent;
import java.util.EventListenerProxy;

/**
 * This class allows adding an AWTEventListener which only pays attention to
 * a specific event mask.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Toolkit
 * @see EventListenerProxy
 * @since 1.4
 * @status updated to 1.4
 */
public class AWTEventListenerProxy extends EventListenerProxy
  implements AWTEventListener
{
  /** The event mask. */
  private final long mask;

  /**
   * Construct an AWT Event Listener which only listens to events in the given
   * mask, passing the work on to the real listener.
   *
   * @param eventMask the mask of events to listen to
   * @param listener the wrapped listener
   */
  public AWTEventListenerProxy(long eventMask, AWTEventListener listener)
  {
    super(listener);
    mask = eventMask;
  }

  /**
   * Forwards events on to the delegate if they meet the event mask.
   *
   * @param event the property change event to filter
   * @throws NullPointerException if the delegate this was created with is null
   */
  public void eventDispatched(AWTEvent event)
  {
    int id = event == null ? 0 : event.getID();
    if (((mask & AWTEvent.ACTION_EVENT_MASK) != 0
         && event instanceof ActionEvent)
        || ((mask & AWTEvent.ADJUSTMENT_EVENT_MASK) != 0
            && event instanceof AdjustmentEvent)
        || ((mask & AWTEvent.COMPONENT_EVENT_MASK) != 0
            && event instanceof ComponentEvent
            && (id >= ComponentEvent.COMPONENT_FIRST
                && id <= ComponentEvent.COMPONENT_LAST))
        || ((mask & AWTEvent.CONTAINER_EVENT_MASK) != 0
            && event instanceof ContainerEvent)
        || ((mask & AWTEvent.FOCUS_EVENT_MASK) != 0
            && event instanceof FocusEvent)
        || ((mask & AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK) != 0
            && event instanceof HierarchyEvent
            && (id == HierarchyEvent.ANCESTOR_MOVED
                || id == HierarchyEvent.ANCESTOR_RESIZED))
        || ((mask & AWTEvent.HIERARCHY_EVENT_MASK) != 0
            && event instanceof HierarchyEvent
            && id == HierarchyEvent.HIERARCHY_CHANGED)
        || ((mask & AWTEvent.INPUT_METHOD_EVENT_MASK) != 0
            && event instanceof InputMethodEvent)
        || ((mask & AWTEvent.INVOCATION_EVENT_MASK) != 0
            && event instanceof InvocationEvent)
        || ((mask & AWTEvent.ITEM_EVENT_MASK) != 0
            && event instanceof ItemEvent)
        || ((mask & AWTEvent.KEY_EVENT_MASK) != 0
            && event instanceof KeyEvent)
        || ((mask & AWTEvent.MOUSE_EVENT_MASK) != 0
            && event instanceof MouseEvent
            && (id == MouseEvent.MOUSE_PRESSED
                || id == MouseEvent.MOUSE_RELEASED
                || id == MouseEvent.MOUSE_CLICKED
                || id == MouseEvent.MOUSE_ENTERED
                || id == MouseEvent.MOUSE_EXITED))
        || ((mask & AWTEvent.MOUSE_MOTION_EVENT_MASK) != 0
            && event instanceof MouseEvent
            && (id == MouseEvent.MOUSE_MOVED
                || id == MouseEvent.MOUSE_DRAGGED))
        || ((mask & AWTEvent.MOUSE_WHEEL_EVENT_MASK) != 0
            && event instanceof MouseWheelEvent)
        || ((mask & AWTEvent.PAINT_EVENT_MASK) != 0
            && event instanceof PaintEvent)
        || ((mask & AWTEvent.TEXT_EVENT_MASK) != 0
            && event instanceof TextEvent)
        || ((mask & AWTEvent.WINDOW_EVENT_MASK) != 0
            && event instanceof WindowEvent
            && (id == WindowEvent.WINDOW_OPENED
                || id == WindowEvent.WINDOW_CLOSING
                || id == WindowEvent.WINDOW_CLOSED
                || id == WindowEvent.WINDOW_ICONIFIED
                || id == WindowEvent.WINDOW_DEICONIFIED
                || id == WindowEvent.WINDOW_ACTIVATED
                || id == WindowEvent.WINDOW_DEACTIVATED))
        || ((mask & AWTEvent.WINDOW_FOCUS_EVENT_MASK) != 0
            && event instanceof WindowEvent
            && (id == WindowEvent.WINDOW_GAINED_FOCUS
                || id == WindowEvent.WINDOW_LOST_FOCUS))
        || ((mask & AWTEvent.WINDOW_STATE_EVENT_MASK) != 0
            && event instanceof WindowEvent
            && id == WindowEvent.WINDOW_STATE_CHANGED))
      ((AWTEventListener) getListener()).eventDispatched(event);
  }

  /**
   * This returns the event mask associated with this listener.
   *
   * @return the event mask
   */
  public long getEventMask()
  {
    return mask;
  }
} // class AWTEventListenerProxy
