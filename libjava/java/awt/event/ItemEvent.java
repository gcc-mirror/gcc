/* ItemEvent.java -- event for item state changes
   Copyright (C) 1999, 2002, 2005  Free Software Foundation, Inc.

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
import java.awt.ItemSelectable;

/**
 * This event is generated when a selection item changes state. This is an
 * abstraction that distills a large number of individual mouse or keyboard
 * events into a simpler "item selected" and "item deselected" events.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see ItemSelectable
 * @see ItemListener
 * @since 1.1
 * @status updated to 1.4
 */
public class ItemEvent extends AWTEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -608708132447206933L;

  /** This is the first id in the event id range used by this class. */
  public static final int ITEM_FIRST = 701;

  /** This is the last id in the event id range used by this class. */
  public static final int ITEM_LAST = 701;

  /** This event id indicates a state change occurred. */
  public static final int ITEM_STATE_CHANGED = 701;

  /** This type indicates that the item was selected. */
  public static final int SELECTED = 1;

  /** This type indicates that the item was deselected. */
  public static final int DESELECTED = 2;

  /**
   * The item affected by this event.
   *
   * @serial the item of the selection
   */
  private final Object item;

  /**
   * The state change direction, one of {@link #SELECTED} or
   * {@link #DESELECTED}.
   *
   * @serial the selection state
   */
  private final int stateChange;

  /**
   * Initializes a new instance of <code>ItemEvent</code> with the specified
   * source, id, and state change constant. Note that an invalid id leads to
   * unspecified results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param item the item affected by the state change
   * @param stateChange one of {@link #SELECTED} or {@link #DESELECTED}
   */
  public ItemEvent(ItemSelectable source, int id, Object item, int stateChange)
  {
    super(source, id);
    this.item = item;
    this.stateChange = stateChange;
  }

  /**
   * This method returns the event source as an <code>ItemSelectable</code>.
   *
   * @return the event source as an <code>ItemSelected</code>
   * @throws ClassCastException if source is changed to a non-ItemSelectable
   */
  public ItemSelectable getItemSelectable()
  {
    return (ItemSelectable) source;
  }

  /**
   * Returns the item affected by this state change.
   *
   * @return the item affected by this state change
   */
  public Object getItem()
  {
    return item;
  }

  /**
   * Returns the type of state change, either {@link #SELECTED} or
   * {@link #DESELECTED}.
   *
   * @return the type of state change
   */
  public int getStateChange()
  {
    return stateChange;
  }

  /**
   * Returns a string identifying this event. This is in the format:
   * <code>"ITEM_STATE_CHANGED,item=" + item + ",stateChange="
   * + (getStateChange() == DESELECTED ? "DESELECTED" : "SELECTED")</code>.
   *
   * @return a string identifying this event
   */
  public String paramString()
  {
    return (id == ITEM_STATE_CHANGED ? "ITEM_STATE_CHANGED,item="
            : "unknown type,item=") + item + ",stateChange="
      + (stateChange == SELECTED ? "SELECTED"
         : stateChange == DESELECTED ? "DESELECTED" : "unknown type");
  }
} // class ItemEvent
