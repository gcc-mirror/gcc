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

import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.peer.LightweightPeer;
import java.util.WeakHashMap;

/**
 * Redispatches mouse events to lightweight components. The native peers know
 * nothing about the lightweight components and thus mouse events are always
 * targetted at Windows or heavyweight components. This class listenes directly
 * on the eventqueue and dispatches mouse events to lightweight components.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
final class LightweightDispatcher
{

  /**
   * Maps thread groups to lightweight dispatcher instances. We need to
   * have one instance per thread group so that 2 or more applets or otherwise
   * separated applications (like in OSGI) do not interfer with each other.
   */
  private static WeakHashMap instances = new WeakHashMap();

  /**
   * The last mouse event target. If the target changes, additional
   * MOUSE_ENTERED and MOUSE_EXITED events must be dispatched.
   */
  private Component lastTarget;

  /**
   * The current mouseEventTarget.
   */
  private Component mouseEventTarget;

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
  public boolean dispatchEvent(final AWTEvent event)
  {
    if (event instanceof MouseEvent)
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
  private boolean handleMouseEvent(final MouseEvent ev)
  {
    Container container = (Container) ev.getSource();
    Component target = findTarget(container, ev.getX(), ev.getY());
    trackEnterExit(target, ev);
    int id = ev.getID();

    // Dont update the mouseEventTarget when dragging. Also, MOUSE_CLICKED
    // must be dispatched to the original target of MOUSE_PRESSED, so don't
    // update in this case either.
    if (! isDragging(ev) && id != MouseEvent.MOUSE_CLICKED)
      mouseEventTarget = (target != container) ? target : null;

    if (mouseEventTarget != null)
      {
        switch (id)
          {
          case MouseEvent.MOUSE_ENTERED:
          case MouseEvent.MOUSE_EXITED:
            // This is already handled in trackEnterExit().
            break;
          case MouseEvent.MOUSE_PRESSED:
          case MouseEvent.MOUSE_RELEASED:
          case MouseEvent.MOUSE_MOVED:
            redispatch(ev, mouseEventTarget, id);
            break;
          case MouseEvent.MOUSE_CLICKED:
            // MOUSE_CLICKED must be dispatched to the original target of
            // MOUSE_PRESSED.
            if (target == mouseEventTarget)
              redispatch(ev, mouseEventTarget, id);
            break;
          case MouseEvent.MOUSE_DRAGGED:
            if (isDragging(ev))
              redispatch(ev, mouseEventTarget, id);
            break;
          case MouseEvent.MOUSE_WHEEL:
            redispatch(ev, mouseEventTarget, id);
          }
        ev.consume();
      }

    return ev.isConsumed();
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
  private Component findTarget(final Container c, final int x, final int y)
  {
    Component target = null;

    // First we check the children of the container.

    // Note: It is important that we use the package private Container
    // fields ncomponents and component here. There are applications
    // that override getComponentCount()
    // and getComponent() to hide internal components, which makes
    // the LightweightDispatcher not work correctly in these cases.
    // As a positive sideeffect this is slightly more efficient.
    int nChildren = c.ncomponents;
    for (int i = 0; i < nChildren && target == null; i++)
      {
        Component child = c.component[i];
        int childX = x - child.x;
        int childY = y - child.y;
        if (child != null && child.visible
            && child.peer instanceof LightweightPeer
            && child.contains(childX, childY))
          {
            // Check if there's a deeper possible target.
            if (child instanceof Container)
              {
                Component deeper = findTarget((Container) child,
                                              childX, childY);
                if (deeper != null)
                  target = deeper;
              }
            // Check if the child itself is interested in mouse events.
            else if (isMouseListening(child))
              target = child;
          }
      }

    // Check the container itself, if we didn't find a target yet.
    if (target == null  && c.contains(x, y) && isMouseListening(c))
      target = c;

    return target;
  }

  /**
   * Checks if the specified component would be interested in a mouse event.
   *
   * @param c the component to check
   *
   * @return <code>true</code> if the component has mouse listeners installed,
   *         <code>false</code> otherwise
   */
  private boolean isMouseListening(final Component c)
  {
    // Note: It is important to NOT check if the component is listening
    // for a specific event (for instance, mouse motion events). The event
    // gets dispatched to the component if the component is listening
    // for ANY mouse event, even when the component is not listening for the
    // specific type of event. There are applications that depend on this
    // (sadly).
    return c.mouseListener != null
           || c.mouseMotionListener != null
           || c.mouseWheelListener != null
           || (c.eventMask & AWTEvent.MOUSE_EVENT_MASK) != 0
           || (c.eventMask & AWTEvent.MOUSE_MOTION_EVENT_MASK) != 0
           || (c.eventMask & AWTEvent.MOUSE_WHEEL_EVENT_MASK) != 0;
  }

  /**
   * Tracks MOUSE_ENTERED and MOUSE_EXIT as well as MOUSE_MOVED and
   * MOUSE_DRAGGED and creates synthetic MOUSE_ENTERED and MOUSE_EXITED for
   * lightweight component.s
   *
   * @param target the current mouse event target
   * @param ev the mouse event
   */
  private void trackEnterExit(final Component target, final MouseEvent ev)
  {
    int id = ev.getID();
    if (target != lastTarget)
      {
        if (lastTarget != null)
          redispatch(ev, lastTarget, MouseEvent.MOUSE_EXITED);
        if (id == MouseEvent.MOUSE_EXITED)
          ev.consume();
        if (target != null)
          redispatch(ev, target, MouseEvent.MOUSE_ENTERED);
        if (id == MouseEvent.MOUSE_ENTERED)
          ev.consume();
        lastTarget = target;
      }

  }

  /**
   * Redispatches the specified mouse event to the specified target with the
   * specified id.
   *
   * @param ev the mouse event
   * @param target the new target
   * @param id the new id
   */
  private void redispatch(MouseEvent ev, Component target, int id)
  {
    Component source = ev.getComponent();
    assert target != null;
    if (target.isShowing())
      {
        // Translate coordinates.
        int x = ev.getX();
        int y = ev.getY();
        for (Component c = target; c != null && c != source; c = c.getParent())
          {
            x -= c.x;
            y -= c.y;
          }

        // Retarget event.
        MouseEvent retargeted;
        if (id == MouseEvent.MOUSE_WHEEL)
          {
            MouseWheelEvent mwe = (MouseWheelEvent) ev;
            retargeted = new MouseWheelEvent(target, id, ev.getWhen(),
                                             ev.getModifiers()
                                             | ev.getModifiersEx(), x, y,
                                             ev.getClickCount(),
                                             ev.isPopupTrigger(),
                                             mwe.getScrollType(),
                                             mwe.getScrollAmount(),
                                             mwe.getWheelRotation());
          }
        else
          {
            retargeted = new MouseEvent(target, id, ev.getWhen(),
                                       ev.getModifiers() | ev.getModifiersEx(),
                                       x, y, ev.getClickCount(),
                                       ev.isPopupTrigger(), ev.getButton());
          }

        if (target == source)
          ((Container) target).dispatchNoLightweight(retargeted);
        else
          target.dispatchEvent(retargeted);
      }
  }

  /**
   * Determines if we are in the middle of a drag operation, that is, if
   * any of the buttons is held down.
   *
   * @param ev the mouse event to check
   *
   * @return <code>true</code> if we are in the middle of a drag operation,
   *         <code>false</code> otherwise
   */
  private boolean isDragging(MouseEvent ev)
  {
    int mods = ev.getModifiersEx();
    int id = ev.getID();
    if (id == MouseEvent.MOUSE_PRESSED || id == MouseEvent.MOUSE_RELEASED)
      {
        switch (ev.getButton())
          {
            case MouseEvent.BUTTON1:
              mods ^= InputEvent.BUTTON1_DOWN_MASK;
              break;
            case MouseEvent.BUTTON2:
              mods ^= InputEvent.BUTTON2_DOWN_MASK;
              break;
            case MouseEvent.BUTTON3:
              mods ^= InputEvent.BUTTON3_DOWN_MASK;
              break;
          }
      }
    return (mods & (InputEvent.BUTTON1_DOWN_MASK
                    | InputEvent.BUTTON2_DOWN_MASK
                    | InputEvent.BUTTON3_DOWN_MASK)) != 0;
  }
}
