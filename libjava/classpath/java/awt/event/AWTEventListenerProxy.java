/* AWTEventListenerProxy.java -- wrapper/filter for AWTEventListener
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


package java.awt.event;

import java.awt.AWTEvent;
import java.awt.Toolkit;
import java.util.EventListenerProxy;

/**
 * This class allows adding an AWTEventListener which only pays attention to
 * a specific event mask.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
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
   * Forwards events on to the delegate.
   *
   * @param event the to forward to the delagate listener
   *
   * @throws NullPointerException if the delegate this was created with is null
   */
  public void eventDispatched(AWTEvent event)
  {
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
