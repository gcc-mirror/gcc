/* MouseEvent.java -- a mouse event
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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
import java.awt.Point;
import java.io.IOException;
import java.io.ObjectInputStream;
import gnu.java.awt.EventModifier;

/**
 * This event is generated for a mouse event. There are three main categories
 * of mouse events: Regular events include pressing, releasing, and clicking
 * buttons, as well as moving over the boundary of the unobscured portion of
 * a component. Motion events include movement and dragging. Wheel events are
 * covered separately by the subclass MouseWheelEvent.
 *
 * <p>A mouse event is tied to the unobstructed visible component that the
 * mouse cursor was over at the time of the action. The button that was
 * most recently pressed is the only one that shows up in
 * <code>getModifiers</code>, and is returned by <code>getButton</code>,
 * while all buttons that are down show up in <code>getModifiersEx</code>.
 *
 * <p>Drag events may be cut short if native drag-and-drop operations steal
 * the event. Likewise, if a mouse drag exceeds the bounds of a window or
 * virtual device, some platforms may clip the path to fit in the bounds of
 * the component.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see MouseAdapter
 * @see MouseListener
 * @see MouseMotionAdapter
 * @see MouseMotionListener
 * @see MouseWheelListener
 * @since 1.1
 * @status updated to 1.4
 */
public class MouseEvent extends InputEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -991214153494842848L;

  /** This is the first id in the range of event ids used by this class. */
  public static final int MOUSE_FIRST = 500;

  /** This is the last id in the range of event ids used by this class. */
  public static final int MOUSE_LAST = 507;

  /** This event id indicates that the mouse was clicked. */
  public static final int MOUSE_CLICKED = 500;

  /** This event id indicates that the mouse was pressed. */
  public static final int MOUSE_PRESSED = 501;

  /** This event id indicates that the mouse was released. */
  public static final int MOUSE_RELEASED = 502;

  /** This event id indicates that the mouse was moved. */
  public static final int MOUSE_MOVED = 503;

  /** This event id indicates that the mouse entered a component. */
  public static final int MOUSE_ENTERED = 504;

  /** This event id indicates that the mouse exited a component. */
  public static final int MOUSE_EXITED = 505;

  /**
   * This indicates that no button changed state.
   *
   * @see #getButton()
   * @since 1.4
   */
  public static final int NOBUTTON = 0;

  /**
   * This indicates that button 1 changed state.
   *
   * @see #getButton()
   * @since 1.4
   */
  public static final int BUTTON1 = 1;

  /**
   * This indicates that button 2 changed state.
   *
   * @see #getButton()
   * @since 1.4
   */
  public static final int BUTTON2 = 2;

  /**
   * This indicates that button 3 changed state.
   *
   * @see #getButton()
   * @since 1.4
   */
  public static final int BUTTON3 = 3;

  /** This event id indicates that the mouse was dragged over a component. */
  public static final int MOUSE_DRAGGED = 506;

  /**
   * This event id indicates that the mouse wheel was rotated.
   *
   * @since 1.4
   */
  public static final int MOUSE_WHEEL = 507;

  /**
   * The X coordinate of the mouse cursor at the time of the event.
   *
   * @see #getX()
   * @serial the x coordinate
  */
  private int x;

  /**
   * The Y coordinate of the mouse cursor at the time of the event.
   *
   * @see #getY()
   * @serial the y coordinate
   */
  private int y;

  /**
   * The number of clicks that took place. For MOUSE_CLICKED, MOUSE_PRESSED,
   * and MOUSE_RELEASED, this will be at least 1; otherwise it is 0.
   *
   * see #getClickCount()
   * @serial the number of clicks
   */
  private final int clickCount;

  /**
   * Indicates which mouse button changed state. Can only be one of
   * {@link #NOBUTTON}, {@link #BUTTON1}, {@link #BUTTON2}, or
   * {@link #BUTTON3}.
   *
   * @see #getButton()
   * @since 1.4
   */
  private int button;

  /**
   * Whether or not this event should trigger a popup menu.
   *
   * @see PopupMenu
   * @see #isPopupTrigger()
   * @serial true if this is a popup trigger
   */
  private final boolean popupTrigger;

  /**
   * Initializes a new instance of <code>MouseEvent</code> with the specified
   * information. Note that an invalid id leads to unspecified results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param when the timestamp of when the event occurred
   * @param modifiers the modifier keys during the event, in old or new style
   * @param x the X coordinate of the mouse point
   * @param y the Y coordinate of the mouse point
   * @param clickCount the number of mouse clicks for this event
   * @param popupTrigger true if this event triggers a popup menu
   * @param button the most recent mouse button to change state
   * @throws IllegalArgumentException if source is null or button is invalid
   * @since 1.4
   */
  public MouseEvent(Component source, int id, long when, int modifiers,
                    int x, int y, int clickCount, boolean popupTrigger,
                    int button)
  {
    super(source, id, when, modifiers);
    this.x = x;
    this.y = y;
    this.clickCount = clickCount;
    this.popupTrigger = popupTrigger;
    this.button = button;
    if (button < NOBUTTON || button > BUTTON3)
      throw new IllegalArgumentException();
    if ((modifiers & EventModifier.OLD_MASK) != 0)
      {
        if ((modifiers & BUTTON1_MASK) != 0)
          button = BUTTON1;
        else if ((modifiers & BUTTON2_MASK) != 0)
          button = BUTTON2;
        else if ((modifiers & BUTTON3_MASK) != 0)
          button = BUTTON3;
      }
  }

  /**
   * Initializes a new instance of <code>MouseEvent</code> with the specified
   * information. Note that an invalid id leads to unspecified results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param when the timestamp of when the event occurred
   * @param modifiers the modifier keys during the event, in old or new style
   * @param x the X coordinate of the mouse point
   * @param y the Y coordinate of the mouse point
   * @param clickCount the number of mouse clicks for this event
   * @param popupTrigger true if this event triggers a popup menu
   * @throws IllegalArgumentException if source is null
   */
  public MouseEvent(Component source, int id, long when, int modifiers,
                    int x, int y, int clickCount, boolean popupTrigger)
  {
    this(source, id, when, modifiers, x, y, clickCount, popupTrigger,
         NOBUTTON);
  }

  /**
   * This method returns the X coordinate of the mouse position. This is
   * relative to the source component.
   *
   * @return the x coordinate
   */
  public int getX()
  {
    return x;
  }

  /**
   * This method returns the Y coordinate of the mouse position. This is
   * relative to the source component.
   *
   * @return the y coordinate
   */
  public int getY()
  {
    return y;
  }

  /**
   * This method returns a <code>Point</code> for the x,y position of
   * the mouse pointer. This is relative to the source component.
   *
   * @return a <code>Point</code> for the event position
   */
  public Point getPoint()
  {
    return new Point(x, y);
  }

  /**
   * Translates the event coordinates by the specified x and y offsets.
   *
   * @param dx the value to add to the X coordinate of this event
   * @param dy the value to add to the Y coordiante of this event
   */
  public void translatePoint(int dx, int dy)
  {
    x += dx;
    y += dy;
  }

  /**
   * This method returns the number of mouse clicks associated with this
   * event.
   *
   * @return the number of mouse clicks for this event
   */
  public int getClickCount()
  {
    return clickCount;
  }

  /**
   * Returns which button, if any, was the most recent to change state. This
   * will be one of {@link #NOBUTTON}, {@link #BUTTON1}, {@link #BUTTON2}, or
   * {@link #BUTTON3}.
   *
   * @return the button that changed state
   * @since 1.4
   */
  public int getButton()
  {
    return button;
  }

  /**
   * This method tests whether or not the event is a popup menu trigger. This
   * should be checked in both MousePressed and MouseReleased to be
   * cross-platform compatible, as different systems have different popup
   * triggers.
   *
   * @return true if the event is a popup menu trigger
   */
  public boolean isPopupTrigger()
  {
    return popupTrigger;
  }

  /**
   * Returns a string describing the modifiers, such as "Shift" or
   * "Ctrl+Button1".
   *
   * XXX Sun claims this can be localized via the awt.properties file - how
   * do we implement that?
   *
   * @param modifiers the old-style modifiers to convert to text
   * @return a string representation of the modifiers in this bitmask
   */
  public static String getMouseModifiersText(int modifiers)
  {
    modifiers &= EventModifier.OLD_MASK;
    if ((modifiers & BUTTON2_MASK) != 0)
      modifiers |= BUTTON2_DOWN_MASK;
    if ((modifiers & BUTTON3_MASK) != 0)
      modifiers |= BUTTON3_DOWN_MASK;
    return getModifiersExText(EventModifier.extend(modifiers));
  }

  /**
   * Returns a string identifying this event. This is formatted as the field
   * name of the id type, followed by the (x,y) point, the most recent button
   * changed, modifiers (if any), extModifiers (if any), and clickCount.
   *
   * @return a string identifying this event
   */
  public String paramString()
  {
    StringBuffer s = new StringBuffer();
    switch (id)
      {
      case MOUSE_CLICKED:
        s.append("MOUSE_CLICKED,(");
        break;
      case MOUSE_PRESSED:
        s.append("MOUSE_PRESSED,(");
        break;
      case MOUSE_RELEASED:
        s.append("MOUSE_RELEASED,(");
        break;
      case MOUSE_MOVED:
        s.append("MOUSE_MOVED,(");
        break;
      case MOUSE_ENTERED:
        s.append("MOUSE_ENTERED,(");
        break;
      case MOUSE_EXITED:
        s.append("MOUSE_EXITED,(");
        break;
      case MOUSE_DRAGGED:
        s.append("MOUSE_DRAGGED,(");
        break;
      case MOUSE_WHEEL:
        s.append("MOUSE_WHEEL,(");
        break;
      default:
        s.append("unknown type,(");
      }
    s.append(x).append(',').append(y).append("),button=").append(button);
    if ((modifiers & EventModifier.NEW_MASK) != 0)
      {
        int mod = modifiers;
        if ((mod & (ALT_DOWN_MASK | BUTTON2_DOWN_MASK)) != 0)
          mod |= ALT_DOWN_MASK | BUTTON2_DOWN_MASK;
        if ((mod & (META_DOWN_MASK | BUTTON3_DOWN_MASK)) != 0)
          mod |= META_DOWN_MASK | BUTTON3_DOWN_MASK;
        s.append(",modifiers=").append(getModifiersExText(mod));
      }
    if (modifiers != 0)
      s.append(",extModifiers=").append(getModifiersExText(modifiers));
    return s.append(",clickCount=").append(clickCount).toString();
  }

  /**
   * Reads in the object from a serial stream.
   *
   * @param s the stream to read from
   * @throws IOException if deserialization fails
   * @throws ClassNotFoundException if deserialization fails
   * @serialData default, except that the modifiers are converted to new style
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    if ((modifiers & EventModifier.OLD_MASK) != 0)
      {
        if ((modifiers & BUTTON1_MASK) != 0)
          button = BUTTON1;
        else if ((modifiers & BUTTON2_MASK) != 0)
          button = BUTTON2;
        else if ((modifiers & BUTTON3_MASK) != 0)
          button = BUTTON3;
        modifiers = EventModifier.extend(modifiers);
      }
  }
} // class MouseEvent
