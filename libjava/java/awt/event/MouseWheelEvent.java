/* MouseWheelEvent.java -- a mouse wheel event
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


package java.awt.event;

import java.awt.Component;

/**
 * This event is generated for a mouse wheel rotation. The wheel (the middle
 * mouse button on most modern mice) can be rotated towards or away from the
 * user, and is ofteh used for scrolling.
 *
 * <p>Because of the special use for scrolling components, MouseWheelEvents
 * often affect a different component than the one located at the point of
 * the event. If the component under the mouse cursor does not accept wheel
 * events, the event is passed to the first ancestor container which does. This
 * is often a ScrollPane, which knows how to scroll. If an AWT component is
 * built from a native widget that knows how to use mouse wheel events, that
 * component will consume the event.
 *
 * <p>The two most common scroll types are "units" (lines at a time) or
 * "blocks" (pages at a time). The initial setting is taken from the platform,
 * although the user can adjust the setting at any time.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see MouseWheelListener
 * @see ScrollPane
 * @see ScrollPane#setWheelScrollingEnabled(boolean)
 * @see JScrollPane
 * @see JScrollPane#setWheelScrollingEnabled(boolean)
 * @since 1.4
 * @status updated to 1.4
 */
public class MouseWheelEvent extends MouseEvent
{
  /**
   * Compatible with JDK 1.4+.
   */
  private static final long serialVersionUID = 6459879390515399677L;

  /**
   * Indicates scrolling by units (lines).
   *
   * @see #getScrollType()
   */
  public static final int WHEEL_UNIT_SCROLL = 0;

  /**
   * Indicates scrolling by blocks (pages).
   *
   * @see #getScrollType()
   */
  public static final int WHEEL_BLOCK_SCROLL = 1;

  /**
   * Indicates what scroll type should take place. This should be limited
   * to {@link #WHEEL_UNIT_SCROLL} and {@link #WHEEL_BLOCK_SCROLL}.
   *
   * @serial the scroll type
   */
  private final int scrollType;

  /**
   * Indicates the scroll amount. This is only meaningful if scrollType is
   * WHEEL_UNIT_SCROLL.
   *
   * @serial the number of lines to scroll
   */
  private final int scrollAmount;

  /**
   * Indicates how far the mouse wheel was rotated.
   *
   * @serial the rotation amount
   */
  private final int wheelRotation;

  /**
   * Initializes a new instance of <code>MouseWheelEvent</code> with the
   * specified information. Note that an invalid id leads to unspecified
   * results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param when the timestamp of when the event occurred
   * @param modifiers any modifier bits for this event
   * @param x the X coordinate of the mouse point
   * @param y the Y coordinate of the mouse point
   * @param clickCount the number of mouse clicks for this event
   * @param popupTrigger true if this event triggers a popup menu
   * @param scrollType one of {@link #WHEEL_UNIT_SCROLL},
   *        {@link #WHEEL_BLOCK_SCROLL}
   * @param scrollAmount the number of units to scroll, ignored for block type
   * @param wheelRotation the number of rotation "clicks"
   * @throws IllegalArgumentException if source is null
   * @see MouseEvent#MouseEvent(Component, int, long, int, int, int, int,
   *      boolean) 
   */
  public MouseWheelEvent(Component source, int id, long when, int modifiers,
                         int x, int y, int clickCount, boolean popupTrigger,
                         int scrollType, int scrollAmount, int wheelRotation)
  {
    super(source, id, when, modifiers, x, y, clickCount, popupTrigger);
    this.scrollType = scrollType;
    this.scrollAmount = scrollAmount;
    this.wheelRotation = wheelRotation;
  }

  /**
   * This method returns the scrolling pattern this event requests. Legal
   * values are WHEEL_UNIT_SCROLL and WHEEL_BLOCK_SCROLL.
   *
   * @return the scroll type
   * @see Adjustable#getUnitIncrement()
   * @see Adjustable#getBlockIncrement()
   * @see Scrollable#getScrollableUnitIncrement(Rectangle, int, int)
   * @see Scrollable#getScrollableBlockIncrement(Rectangle, int, int)
   */
  public int getScrollType()
  {
    return scrollType;
  }

  /**
   * Returns the number of units to scroll in response to this event. This
   * only makes sense when the scroll type is WHEEL_UNIT_SCROLL.
   *
   * @return the number of scroll units, if defined
   * @see #getScrollType()
   */
  public int getScrollAmount()
  {
    return scrollAmount;
  }

  /**
   * Gets the number of "clicks" the wheel was rotated. Negative values move
   * up (away) from the user, positive values move down (towards) the user.
   *
   * @return the number of rotation clicks
   */
  public int getWheelRotation()
  {
    return wheelRotation;
  }

  /**
   * This is a convenience method which aids in a common listener for scrolling
   * a scrollpane (although this is already built into ScrollPane and
   * JScrollPane). This method only makes sense when getScrollType() returns
   * WHEEL_UNIT_SCROLL.
   *
   * <p>This accounts for direction of scroll and amount of wheel movement, as
   * interpreted by the platform settings.
   *
   * @return the number of units to scroll
   * @see #getScrollType()
   * @see #getScrollAmount()
   * @see MouseWheelListener
   * @see Adjustable
   * @see Adjustable#getUnitIncrement()
   * @see Scrollable
   * @see Scrollable#getScrollableUnitIncrement(Rectangle, int, int)
   * @see ScrollPane
   * @see ScrollPane#setWheelScrollingEnabled(boolean)
   * @see JScrollPane
   * @see JScrollPane#setWheelScrollingEnabled(boolean)
   */
  public int getUnitsToScroll()
  {
    return wheelRotation * scrollAmount;
  }

  /**
   * Returns a string identifying this event. For mouse wheel events, this
   * is <code>super.paramString() + ",scrollType=WHEEL_" +
   * (getScrollType() == WHEEL_UNIT_SCROLL ? "UNIT" : "BLOCK")
   * + "_SCROLL,scrollAmount=" + getScrollAmount() + ",wheelRotation="
   * + getWheelRotation()</code>.
   *
   * @return a string identifying this event
   */
  public String paramString()
  {
    return super.paramString() + ",scrollType="
      + (scrollType == WHEEL_UNIT_SCROLL ? "WHEEL_UNIT_SCROLL"
         : scrollType == WHEEL_BLOCK_SCROLL ? "WHEEL_BLOCK_SCROLL"
         : "unknown scroll type")
      + ",scrollAmount=" + scrollAmount + ",wheelRotation=" + wheelRotation;
  }
} // class MouseWheelEvent
