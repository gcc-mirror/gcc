/* LightweightDispatcher.java -- Dispatches mouse events to lightweights
   Copyright (C) 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package java.awt;

import java.awt.event.MouseEvent;
import java.util.WeakHashMap;

/**
 * Redispatches mouse events to lightweight components. The native peers know
 * nothing about the lightweight components and thus mouse events are always
 * targetted at Windows or heavyweight components. This class listenes directly
 * on the eventqueue and dispatches mouse events to lightweight components.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
class LightweightDispatcher
{

  /**
   * Maps thread groups to lightweight dispatcher instances. We need to
   * have one instance per thread group so that 2 or more applets or otherwise
   * separated applications (like in OSGI) do not interfer with each other.
   */
  private static WeakHashMap instances = new WeakHashMap();

  /**
   * The component that is the start of a mouse dragging. All MOUSE_DRAGGED
   * events that follow the initial press must have the source set to this,
   * as well as the MOUSE_RELEASED event following the dragging.
   */
  private Component dragTarget;
  
  /**
   * Stores the button number which started the drag operation. This is needed
   * because we want to handle only one drag operation and only the button that
   * started the dragging should be able to stop it (by a button release).
   */
  private int dragButton;

  /**
   * The last mouse event target. If the target changes, additional
   * MOUSE_ENTERED and MOUSE_EXITED events must be dispatched.
   */
  private Component lastTarget;

  /**
   * Returns an instance of LightweightDispatcher for the current thread's
   * thread group.
   *
   * @return an instance of LightweightDispatcher for the current thread's
   *         thread group
   */
  static LightweightDispatcher getInstance()
  {
    Thread t = Thread.currentThread();
    ThreadGroup tg = t.getThreadGroup();
    LightweightDispatcher instance = (LightweightDispatcher) instances.get(tg);
    if (instance == null)
      {
        instance = new LightweightDispatcher();
        instances.put(tg, instance);
      }
    return instance;
  }

  /**
   * Creates a new LightweightDispatcher. This is private to prevent access
   * from outside. Use {@link #getInstance()} instead.
   */
  private LightweightDispatcher()
  {
    // Nothing to do here.
  }
  
  /**
   * Receives notification if a mouse event passes along the eventqueue.
   *
   * @param event the event
   */
  public boolean dispatchEvent(AWTEvent event)
  {
    if (event instanceof MouseEvent && event.getSource() instanceof Window)
      {
        MouseEvent mouseEvent = (MouseEvent) event;
        return handleMouseEvent(mouseEvent);
      }
    return false;
  }

  /**
   * Handles all mouse events that are targetted at toplevel containers
   * (Window instances) and dispatches them to the correct lightweight child.
   * 
   * @param ev the mouse event
   * @return whether or not we found a lightweight that handled the event.
   */
  private boolean handleMouseEvent(MouseEvent ev)
  {
    Window window = (Window) ev.getSource();
    // Find the target for the mouse event. We first seach the deepest
    // component at the specified location. The we go up to its parent and
    // try to find a neighbor of the deepest component that is suitable as
    // mouse event target (it must be showing, at that location and have either
    // a MouseListener or MouseMotionListener installed). If no such component
    // is found, then we walk up the container hierarchy and find the next
    // container that has a MouseListener or MouseMotionListener installed.
    Component deepest = window.findComponentAt(ev.getX(), ev.getY());
    if (deepest == null)
      return false;
    Container parent = deepest.getParent();
    Point loc = ev.getPoint();
    loc = convertPointToChild(window, loc, parent);
    Component target = null;
    if (parent != null)
      {
        target = findTarget(parent, loc);
        while (target == null && parent != null)
          {
            if (parent.getMouseListeners().length > 0
                || parent.getMouseMotionListeners().length > 0)
              {
                target = parent;
              }
            else
              parent = parent.getParent();
          }
      }
    if (target == null || target.isLightweight())
      {
        // Dispatch additional MOUSE_EXITED and MOUSE_ENTERED if event target
        // is different from the last event target.
        if (target != lastTarget)
          {
            if (lastTarget != null)
              {
                Point p1 = convertPointToChild(window, ev.getPoint(),
                                               lastTarget);
                MouseEvent mouseExited =
                  new MouseEvent(lastTarget, MouseEvent.MOUSE_EXITED,
                                 ev.getWhen(), ev.getModifiers(), p1.x, p1.y,
                                 ev.getClickCount(), ev.isPopupTrigger());
                lastTarget.dispatchEvent(mouseExited);
              }
            
            // If a target exists dispatch the MOUSE_ENTERED event only if
            // there is currently no component from which a drag operation
            // started (dragTarget == null) or the target is that component
            // (dragTarget == target)
            // That way a user can click and hold on a button (putting it into
            // the armed state), move the cursor above other buttons without
            // affecting their rollover state and get back to the initial
            // button.
            if (target != null && (dragTarget == null || dragTarget == target))
              {
                Point p = convertPointToChild(window, ev.getPoint(), target);
                MouseEvent mouseEntered =
                  new MouseEvent(target, MouseEvent.MOUSE_ENTERED, ev.getWhen(),
                                 ev.getModifiers(), p.x, p.y, ev.getClickCount(),
                                 ev.isPopupTrigger());
                target.dispatchEvent(mouseEntered);
              }
          }
        
        switch (ev.getID())
        {
          case MouseEvent.MOUSE_PRESSED:
            // Handle the start of a drag operation or discard the event if
            // one is already in progress. This prevents focus changes with the
            // other mouse buttons when one is used for dragging.
            if (dragTarget == null)
              {
                lastTarget = dragTarget = target;
                
                // Save the button that started the drag operation.
                dragButton = ev.getButton();
              }
            else
              return false;
            
            break;
          case MouseEvent.MOUSE_RELEASED:
            // Stop the drag operation only when the button that started
            // it was released.
            if (dragTarget != null && dragButton == ev.getButton())
              {
                target = dragTarget;
                dragTarget = null;
              }
            
            lastTarget = target;
            break;
          case MouseEvent.MOUSE_CLICKED:
            // When we receive a MOUSE_CLICKED, we set the target to the
            // previous target, which must have been a MOUSE_RELEASED event.
            // This is necessary for the case when the MOUSE_RELEASED has
            // caused the original target (like an internal component) go
            // away.
            // This line is the reason why it is not possible to move the
            // 'lastTarget = target' assignment before the switch-statement.
            target = lastTarget;
            break;
          case MouseEvent.MOUSE_DRAGGED:
            // We consider only dragTarget for redispatching the event still
            // we have to act in a way that the newly found target component
            // was handled.
            lastTarget = target;
            target = dragTarget;
            break;
          default:
            // Only declare current target as the old value in all other
            // cases.
            lastTarget = target;
            break;
        }

        if (target != null)
          {
            Point targetCoordinates = convertPointToChild(window,
                                                          ev.getPoint(),
                                                          target);
            int dx = targetCoordinates.x - ev.getX();
            int dy = targetCoordinates.y - ev.getY();
            ev.translatePoint(dx, dy);
            ev.setSource(target);
            target.dispatchEvent(ev);
            
            // We reset the event, so that the normal event dispatching is not
            // influenced by this modified event.
            ev.setSource(window);
            ev.translatePoint(-dx, -dy);
          }

	return true;
      }
    else
      return false;
  }

  /**
   * Finds the actual target for a mouseevent, starting at <code>c</code>.
   * This searches through the children of the container and finds the first
   * one which is showing, at the location from the mouse event and has
   * a MouseListener or MouseMotionListener attached. If no such child component
   * is found, null is returned.
   *
   * @param c the container to search through
   * @param loc the mouse event point
   *
   * @return the actual receiver of the mouse event, or null, if no such
   *         component has been found
   */
  private Component findTarget(Container c, Point loc)
  {
    Component[] children = c.getComponents();
    Component target = null;
    if (c != null)
      {
        for (int i = 0; i < children.length; i++)
          {
            Component child = children[i];
            if (child.isShowing())
              {
                if (child.contains(loc.x - child.getX(), loc.y - child.getY())
                    && (child.getMouseListeners().length > 0 
                        || child.getMouseMotionListeners().length > 0))
                  {
                    target = child;
                    break;
                  }
              }
          }
      }
    return target;
  }

  /**
   * Converts a point in the parent's coordinate system to a child coordinate
   * system. The resulting point is stored in the same Point object and
   * returned.
   *
   * @param parent the parent component
   * @param p the point
   * @param child the child component
   *
   * @return the translated point
   */
  private Point convertPointToChild(Component parent, Point p,
                                   Component child)
  {
    int offX = 0;
    int offY = 0;
    Component comp = child;
    while (comp != null && comp != parent)
      {
        offX += comp.getX();
        offY += comp.getY();
        comp = comp.getParent();
      }
    p.x -= offX;
    p.y -= offY;
    return p;
  }
}
