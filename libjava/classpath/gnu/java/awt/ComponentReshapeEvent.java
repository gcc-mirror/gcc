/* WindowResizeEvent.java -- Used to synchronize the AWT and peer sizes
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


package gnu.java.awt;

import java.awt.AWTEvent;
import java.awt.Component;

/**
 * This is used to update the AWT's knowledge about a Window's size when
 * the user changes the window bounds.
 *
 * This event is _not_ posted to the eventqueue, but rather dispatched directly
 * via Window.dispatchEvent(). It is the cleanest way we could find to update
 * the AWT's knowledge of the window size. Small testprograms showed the
 * following:
 * - Component.reshape() and its derivatives are _not_ called. This makes sense
 *   as it could end up in loops,because this calls back into the peers.
 * - Intercepting event dispatching for any events in
 *   EventQueue.dispatchEvent() showed that the size is still updated. So it
 *   is not done via an event dispatched over the eventqueue.
 *
 * Possible other candidates for implementation would have been:
 * - Call a (private) callback method in Window/Component from the native
 *   side.
 * - Call a (private) callback method in Window/Component via reflection.
 *
 * Both is uglier than sending this event directly. Note however that this
 * is impossible to test, as Component.dispatchEvent() is final and can't be
 * intercepted from outside code. But this impossibility to test the issue from
 * outside code also means that this shouldn't raise any compatibility issues.
 */
public class ComponentReshapeEvent
  extends AWTEvent
{

  public int x;
  public int y;
  public int width;
  public int height;

  public ComponentReshapeEvent(Component c, int x, int y, int width, int height)
  {
    super(c, 1999);
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }
}
