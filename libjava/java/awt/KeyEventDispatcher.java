/* KeyEventDispatcher.java -- dispatches key events
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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


package java.awt;

import java.awt.event.KeyEvent;

/**
 * An instance of this interface coordinates with a KeyboardFocusManager to
 * target and dispatch all key events. This allows retargeting, consuming,
 * changing, or otherwise manipulating the key event before sending it on to
 * a target.
 *
 * <p>By default, the KeyboardFocusManager is the sink for all key events not
 * dispatched by other dispatchers. Therefore, it is unnecessary for the user
 * to register the focus manager as a dispatcher.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see KeyboardFocusManager#addKeyEventDispatcher(KeyEventDispatcher)
 * @see KeyboardFocusManager#removeKeyEventDispatcher(KeyEventDispatcher)
 * @since 1.4
 * @status updated to 1.4
 */
public interface KeyEventDispatcher
{
  /**
   * Called by the KeyboardFocusManager to request that a key event be
   * dispatched. The dispatcher is free to retarget the event, consume it,
   * dispatch it, or make other changes. This is usually done to allow
   * delivery of key events to objects other than the window in focus, such
   * as for navigating non-focusable components. If this dispatcher chooses
   * to dispatch the event itself, it should call <code>redispatchEvent</code>
   * to avoid infinite recursion.
   *
   * <p>If the return value is false, the KeyEvent is passed to the next
   * dispatcher in the chain, ending with the KeyboardFocusManager. If the
   * return value is true, the event has been consumed (although it might
   * have been ignored), and no further action will be taken on the event. Be
   * sure to check whether the event was consumed before dispatching it
   * further.
   *
   * @param e the key event
   * @return true if the event has been consumed
   * @see KeyboardFocusManager#redispatchEvent(Component, AWTEvent)
   */
  boolean dispatchKeyEvent(KeyEvent e);
} // interface KeyEventDispatcher
