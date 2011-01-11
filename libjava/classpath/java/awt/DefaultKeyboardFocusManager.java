/* DefaultKeyboardFocusManager.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

// FIXME: finish documentation
public class DefaultKeyboardFocusManager extends KeyboardFocusManager
{
  /**
   * This class models a request to delay the dispatch of events that
   * arrive after a certain time, until a certain component becomes
   * the focus owner.
   */
  private class EventDelayRequest implements Comparable
  {
    /** A {@link java.util.List} of {@link java.awt.event.KeyEvent}s
        that are being delayed, pending this request's {@link
        Component} receiving the keyboard focus. */
    private LinkedList enqueuedKeyEvents = new LinkedList ();

    /** An event timestamp.  All events that arrive after this time
        should be queued in the {@link #enqueuedKeyEvents} {@link
        java.util.List}. */
    public long timestamp;
    /** When this {@link Component} becomes focused, all events
        between this EventDelayRequest and the next one in will be
        dispatched from {@link #enqueuedKeyEvents}. */
    public Component focusedComp;

    /**
     * Construct a new EventDelayRequest.
     *
     * @param timestamp events that arrive after this time will be
     * delayed
     * @param focusedComp the Component that needs to receive focus
     * before events are dispatched
     */
    public EventDelayRequest (long timestamp, Component focusedComp)
    {
      this.timestamp = timestamp;
      this.focusedComp = focusedComp;
    }

    public int compareTo (Object o)
    {
      if (!(o instanceof EventDelayRequest))
        throw new ClassCastException ();

      EventDelayRequest request = (EventDelayRequest) o;

      if (request.timestamp < timestamp)
        return -1;
      else if (request.timestamp == timestamp)
        return 0;
      else
        return 1;
    }

    public boolean equals (Object o)
    {
      if (!(o instanceof EventDelayRequest) || o == null)
        return false;

      EventDelayRequest request = (EventDelayRequest) o;

      return (request.timestamp == timestamp
              && request.focusedComp == focusedComp);
    }

    public void enqueueEvent (KeyEvent e)
    {
      KeyEvent last = (KeyEvent) enqueuedKeyEvents.getLast ();
      if (last != null && e.getWhen () < last.getWhen ())
        throw new RuntimeException ("KeyEvents enqueued out-of-order");

      if (e.getWhen () <= timestamp)
        throw new RuntimeException ("KeyEvents enqueued before starting timestamp");

      enqueuedKeyEvents.add (e);
    }

    public void dispatchEvents ()
    {
      int size = enqueuedKeyEvents.size ();
      for (int i = 0; i < size; i++)
        {
          KeyEvent e = (KeyEvent) enqueuedKeyEvents.remove (0);
          dispatchKeyEvent (e);
        }
    }

    public void discardEvents ()
    {
      enqueuedKeyEvents.clear ();
    }
  }

  /**
   * This flag indicates for which focus traversal key release event we
   * possibly wait, before letting any more KEY_TYPED events through.
   */
  private AWTKeyStroke waitForKeyStroke = null;

  /** The {@link java.util.SortedSet} of current
   * {@link EventDelayRequest}s. */
  private SortedSet delayRequests = new TreeSet ();

  public DefaultKeyboardFocusManager ()
  {
  }

  public boolean dispatchEvent (AWTEvent e)
  {
    if (e instanceof WindowEvent)
      {
        Window target = (Window) e.getSource ();

        if (e.id == WindowEvent.WINDOW_ACTIVATED)
          setGlobalActiveWindow (target);
        else if (e.id == WindowEvent.WINDOW_GAINED_FOCUS)
          {
            setGlobalFocusedWindow (target);
            FocusTraversalPolicy p = target.getFocusTraversalPolicy();
            Component toFocus = p.getInitialComponent(target);
            if (toFocus != null)
              toFocus.requestFocusInWindow();
          }
        else if (e.id != WindowEvent.WINDOW_LOST_FOCUS
                 && e.id != WindowEvent.WINDOW_DEACTIVATED)
          return false;

        redispatchEvent(target, e);
        return true;
      }
    else if (e instanceof FocusEvent)
      {
        FocusEvent fe = (FocusEvent) e;
        Component target = fe.getComponent ();

        boolean retval = false;
        if (e.id == FocusEvent.FOCUS_GAINED)
          {
            retval = handleFocusGained(fe);
          }
        else if (e.id == FocusEvent.FOCUS_LOST)
          {
            retval = handleFocusLost(fe);
          }
        return true;
      }
    else if (e instanceof KeyEvent)
      {
        // Loop through all registered KeyEventDispatchers, giving
        // each a chance to handle this event.
        Iterator i = getKeyEventDispatchers().iterator();

        while (i.hasNext ())
          {
            KeyEventDispatcher dispatcher = (KeyEventDispatcher) i.next ();
            if (dispatcher.dispatchKeyEvent ((KeyEvent) e))
              return true;
          }

        // processKeyEvent checks if this event represents a focus
        // traversal key stroke.
        Component focusOwner = getGlobalPermanentFocusOwner ();

        if (focusOwner != null)
          processKeyEvent (focusOwner, (KeyEvent) e);

        if (e.isConsumed ())
          return true;

        if (enqueueKeyEvent ((KeyEvent) e))
          // This event was enqueued for dispatch at a later time.
          return true;
        else
          // This event wasn't handled by any of the registered
          // KeyEventDispatchers, and wasn't enqueued for dispatch
          // later, so send it to the default dispatcher.
          return dispatchKeyEvent ((KeyEvent) e);
      }

    return false;
  }

  /**
   * Handles FOCUS_GAINED events in {@link #dispatchEvent(AWTEvent)}.
   *
   * @param fe the focus event
   */
  private boolean handleFocusGained(FocusEvent fe)
  {
    Component target = fe.getComponent ();

    // If old focus owner != new focus owner, notify old focus
    // owner that it has lost focus.
    Component oldFocusOwner = getGlobalFocusOwner();
    if (oldFocusOwner != null && oldFocusOwner != target)
      {
        FocusEvent lost = new FocusEvent(oldFocusOwner,
                                         FocusEvent.FOCUS_LOST,
                                         fe.isTemporary(), target);
        oldFocusOwner.dispatchEvent(lost);
      }

     setGlobalFocusOwner (target);
     if (target != getGlobalFocusOwner())
       {
         // Focus transfer was rejected, like when the target is not
         // focusable.
         dequeueKeyEvents(-1, target);
         // FIXME: Restore focus somehow.
       }
     else
       {
         if (! fe.isTemporary())
           {
             setGlobalPermanentFocusOwner (target);
             if (target != getGlobalPermanentFocusOwner())
               {
                 // Focus transfer was rejected, like when the target is not
                 // focusable.
                 dequeueKeyEvents(-1, target);
                 // FIXME: Restore focus somehow.
               }
             else
               {
                 redispatchEvent(target, fe);
               }
           }
       }

     return true;
  }

  /**
   * Handles FOCUS_LOST events for {@link #dispatchEvent(AWTEvent)}.
   *
   * @param fe the focus event
   *
   * @return if the event has been handled
   */
  private boolean handleFocusLost(FocusEvent fe)
  {
    Component currentFocus = getGlobalFocusOwner();
    if (currentFocus != fe.getOppositeComponent())
      {
        setGlobalFocusOwner(null);
        if (getGlobalFocusOwner() != null)
          {
            // TODO: Is this possible? If so, then we should try to restore
            // the focus.
          }
        else
          {
            if (! fe.isTemporary())
              {
                setGlobalPermanentFocusOwner(null);
                if (getGlobalPermanentFocusOwner() != null)
                  {
                    // TODO: Is this possible? If so, then we should try to
                    // restore the focus.
                  }
                else
                  {
                    fe.setSource(currentFocus);
                    redispatchEvent(currentFocus, fe);
                  }
              }
          }
      }
    return true;
  }

  private boolean enqueueKeyEvent (KeyEvent e)
  {
    Iterator i = delayRequests.iterator ();
    boolean oneEnqueued = false;
    while (i.hasNext ())
      {
        EventDelayRequest request = (EventDelayRequest) i.next ();
        if (e.getWhen () > request.timestamp)
          {
            request.enqueueEvent (e);
            oneEnqueued = true;
          }
      }
    return oneEnqueued;
  }

  public boolean dispatchKeyEvent (KeyEvent e)
  {
    Component focusOwner = getFocusOwner();
    if (focusOwner == null)
      focusOwner = getFocusedWindow();

    if (focusOwner != null)
      redispatchEvent(focusOwner, e);

    // Loop through all registered KeyEventPostProcessors, giving
    // each a chance to process this event.
    Iterator i = getKeyEventPostProcessors().iterator();

    while (i.hasNext ())
      {
        KeyEventPostProcessor processor = (KeyEventPostProcessor) i.next ();
        if (processor.postProcessKeyEvent (e))
          return true;
      }

    // The event hasn't been consumed yet.  Check if it is an
    // MenuShortcut.
    if (postProcessKeyEvent (e))
      return true;

    // Always return true.
    return true;
  }

  public boolean postProcessKeyEvent (KeyEvent e)
  {
    // Check if this event represents a menu shortcut.

    // MenuShortcuts are activated by Ctrl- KeyEvents, only on KEY_PRESSED.
    int modifiers = e.getModifiersEx ();
    if (e.getID() == KeyEvent.KEY_PRESSED
        && (modifiers & KeyEvent.CTRL_DOWN_MASK) != 0)
      {
        Window focusedWindow = getGlobalFocusedWindow ();
        if (focusedWindow instanceof Frame)
          {
            MenuBar menubar = ((Frame) focusedWindow).getMenuBar ();

            if (menubar != null)
              {
                // If there's a menubar, loop through all menu items,
                // checking whether each one has a shortcut, and if
                // so, whether this key event should activate it.
                int numMenus = menubar.getMenuCount ();

                for (int i = 0; i < numMenus; i++)
                  {
                    Menu menu = menubar.getMenu (i);
                    int numItems = menu.getItemCount ();

                    for (int j = 0; j < numItems; j++)
                      {
                        MenuItem item = menu.getItem (j);
                        MenuShortcut shortcut = item.getShortcut ();

                        if (item.isEnabled() && shortcut != null)
                          {
                            // Dispatch a new ActionEvent if:
                            //
                            //     a) this is a Shift- KeyEvent, and the
                            //        shortcut requires the Shift modifier
                            //
                            // or, b) this is not a Shift- KeyEvent, and the
                            //        shortcut does not require the Shift
                            //        modifier.
                            if (shortcut.getKey () == e.getKeyCode ()
                                && ((shortcut.usesShiftModifier ()
                                     && (modifiers & KeyEvent.SHIFT_DOWN_MASK) != 0)
                                    || (! shortcut.usesShiftModifier ()
                                        && (modifiers & KeyEvent.SHIFT_DOWN_MASK) == 0)))
                              {
                                item.dispatchEvent (new ActionEvent (item,
                                                                     ActionEvent.ACTION_PERFORMED,
                                                                     item.getActionCommand (),
                                                                     modifiers));
                                // The event was dispatched.
                                return true;
                              }
                          }
                      }
                  }
              }
          }
      }
    return false;
  }

  public void processKeyEvent (Component comp, KeyEvent e)
  {
    AWTKeyStroke eventKeystroke = AWTKeyStroke.getAWTKeyStrokeForEvent (e);
    // For every focus traversal keystroke, we need to also consume
    // the other two key event types for the same key (e.g. if
    // KEY_PRESSED TAB is a focus traversal keystroke, we also need to
    // consume KEY_RELEASED and KEY_TYPED TAB key events).
    // consuming KEY_RELEASED is easy, because their keyCodes matches
    // the KEY_PRESSED event. Consuming the intermediate KEY_TYPED is
    // very difficult because their is no clean way that we can know
    // which KEY_TYPED belongs to a focusTraversalKey and which not.
    // To address this problem we swallow every KEY_TYPE between the
    // KEY_PRESSED event that matches a focusTraversalKey and the
    // corresponding KEY_RELEASED.
    AWTKeyStroke oppositeKeystroke = AWTKeyStroke.getAWTKeyStroke (e.getKeyCode (),
                                                                   e.getModifiersEx (),
                                                                   !(e.id == KeyEvent.KEY_RELEASED));

    // Here we check if we are currently waiting for a KEY_RELEASED and
    // swallow all KeyEvents that are to be delivered in between. This
    // should only be the KEY_TYPED events that correspond to the
    // focusTraversalKey's KEY_PRESSED event
    if (waitForKeyStroke != null)
      {
        if (eventKeystroke.equals(waitForKeyStroke))
          // release this lock
          waitForKeyStroke = null;

        // as long as we are waiting for the KEY_RELEASED, we swallow every
        // KeyEvent, including the KEY_RELEASED
        e.consume();
        return;
      }

    Set forwardKeystrokes = comp.getFocusTraversalKeys (KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS);
    Set backwardKeystrokes = comp.getFocusTraversalKeys (KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS);
    Set upKeystrokes = comp.getFocusTraversalKeys (KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS);
    Set downKeystrokes = null;
    if (comp instanceof Container)
      downKeystrokes = comp.getFocusTraversalKeys (KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS);

    if (forwardKeystrokes.contains (eventKeystroke))
      {
        waitForKeyStroke = oppositeKeystroke;
        focusNextComponent (comp);
        e.consume ();
      }
    else if (backwardKeystrokes.contains (eventKeystroke))
      {
        waitForKeyStroke = oppositeKeystroke;
        focusPreviousComponent (comp);
        e.consume ();
      }
    else if (upKeystrokes.contains (eventKeystroke))
      {
        waitForKeyStroke = oppositeKeystroke;
        upFocusCycle (comp);
        e.consume ();
      }
    else if (comp instanceof Container
             && downKeystrokes.contains (eventKeystroke))
      {
        waitForKeyStroke = oppositeKeystroke;
        downFocusCycle ((Container) comp);
        e.consume ();
      }
  }

  protected void enqueueKeyEvents (long after, Component untilFocused)
  {
    delayRequests.add (new EventDelayRequest (after, untilFocused));
  }

  protected void dequeueKeyEvents (long after, Component untilFocused)
  {
    // FIXME: need synchronization on delayRequests and enqueuedKeyEvents.

    // Remove the KeyEvent with the oldest timestamp, which should be
    // the first element in the SortedSet.
    if (after < 0)
      {
        int size = delayRequests.size ();
        if (size > 0)
          delayRequests.remove (delayRequests.first ());
      }
    else
      {
        EventDelayRequest template = new EventDelayRequest (after, untilFocused);
        if (delayRequests.contains (template))
          {
            EventDelayRequest actual = (EventDelayRequest) delayRequests.tailSet (template).first ();
            delayRequests.remove (actual);
            actual.dispatchEvents ();
          }
      }
  }

  protected void discardKeyEvents (Component comp)
  {
    // FIXME: need synchronization on delayRequests and enqueuedKeyEvents.

    Iterator i = delayRequests.iterator ();

    while (i.hasNext ())
      {
        EventDelayRequest request = (EventDelayRequest) i.next ();

        if (request.focusedComp == comp
            || (comp instanceof Container
                && ((Container) comp).isAncestorOf (request.focusedComp)))
          request.discardEvents ();
      }
  }

  public void focusPreviousComponent (Component comp)
  {
    if (comp != null)
      comp.transferFocusBackward();
  }

  public void focusNextComponent (Component comp)
  {
    if (comp != null)
      comp.transferFocus();
  }

  public void upFocusCycle (Component comp)
  {
    if (comp != null)
      comp.transferFocusUpCycle();
  }

  public void downFocusCycle (Container cont)
  {
    if (cont != null)
      cont.transferFocusDownCycle();
  }
} // class DefaultKeyboardFocusManager
