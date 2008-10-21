/* WindowEvent.java -- window change event
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

import gnu.java.lang.CPStringBuilder;

import java.awt.Frame;
import java.awt.Window;

/**
 * This event is generated when there is a change in a window. This includes
 * creation, closing, iconification, activation, and focus changes. There
 * are three listeners, for three types of events: WindowListeners deal with
 * the lifecycle of a window, WindowStateListeners deal with window state
 * like maximization, and WindowFocusListeners deal with focus switching to
 * or from a window. 
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see WindowAdapter
 * @see WindowListener
 * @see WindowFocusListener
 * @see WindowStateListener
 * @since 1.1
 * @status updated to 1.4
 */
public class WindowEvent extends ComponentEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -1567959133147912127L;

  /** This is the first id in the range of event ids used by this class. */
  public static final int WINDOW_FIRST = 200;

  /** This is the id for a window that is opened. */
  public static final int WINDOW_OPENED = 200;

  /** This is the id for a window that is about to close. */
  public static final int WINDOW_CLOSING = 201;

  /** This is the id for a window that finished closing. */
  public static final int WINDOW_CLOSED = 202;

  /** This is the id for a window that is iconified. */
  public static final int WINDOW_ICONIFIED = 203;

  /** This is the id for a window that is de-iconified. */
  public static final int WINDOW_DEICONIFIED = 204;

  /** This is the id for a window that is activated. */
  public static final int WINDOW_ACTIVATED = 205;

  /** This is the id for a window that is de-activated. */
  public static final int WINDOW_DEACTIVATED = 206;

  /**
   * This is the id for a window becoming the focused window.
   *
   * @since 1.4
   */
  public static final int WINDOW_GAINED_FOCUS = 207;

  /**
   * This is the id for a window losing all focus.
   *
   * @since 1.4
   */
  public static final int WINDOW_LOST_FOCUS = 208;

  /**
   * This is the id for a window state change, such as maximization.
   *
   * @since 1.4
   */
  public static final int WINDOW_STATE_CHANGED = 209;

  /** This is the last id in the range of event ids used by this class. */
  public static final int WINDOW_LAST = 209;

  /**
   * The other Window involved in a focus or activation change. For
   * WINDOW_ACTIVATED and WINDOW_GAINED_FOCUS events, this is the window that
   * lost focus; for WINDOW_DEACTIVATED and WINDOW_LOST_FOCUS, this is the
   * window that stole focus; and for other events (or when native
   * implementation does not have the data available), this is null.
   *
   * @see #getOppositeWindow()
   * @serial the opposite window, or null
   * @since 1.4
   */
  private final Window opposite;

  /**
   * The former state of the window.
   *
   * @serial bitmask of the old window state
   * @since 1.4
   */
  private final int oldState;

  /**
   * The present state of the window.
   *
   * @serial bitmask of the new window state
   * @since 1.4
   */
  private final int newState;

  /**
   * Initializes a new instance of <code>WindowEvent</code> with the specified
   * parameters. Note that an invalid id leads to unspecified results.
   *
   * @param source the window that generated this event
   * @param id the event id
   * @param opposite the window that received the opposite event, or null
   * @param oldState the previous state of this window
   * @param newState the new state of this window
   * @throws IllegalArgumentException if source is null
   * @since 1.4
   */
  public WindowEvent(Window source, int id, Window opposite,
                     int oldState, int newState)
  {
    super(source, id);
    this.opposite = opposite;
    this.oldState = oldState;
    this.newState = newState;
  }

  /**
   * Initializes a new instance of <code>WindowEvent</code> with the specified
   * parameters. Note that an invalid id leads to unspecified results.
   *
   * @param source the window that generated this event
   * @param id the event id
   * @param opposite the window that received the opposite event, or null
   * @throws IllegalArgumentException if source is null
   * @since 1.4
   */
  public WindowEvent(Window source, int id, Window opposite)
  {
    this(source, id, opposite, 0, 0);
  }

  /**
   * Initializes a new instance of <code>WindowEvent</code> with the specified
   * parameters. Note that an invalid id leads to unspecified results.
   *
   * @param source the window that generated this event
   * @param id the event id
   * @param oldState the previous state of this window
   * @param newState the new state of this window
   * @throws IllegalArgumentException if source is null
   * @since 1.4
   */
  public WindowEvent(Window source, int id, int oldState, int newState)
  {
    this(source, id, null, oldState, newState);
  }

  /**
   * Initializes a new instance of <code>WindowEvent</code> with the specified
   * parameters. Note that an invalid id leads to unspecified results.
   *
   * @param source the window that generated this event
   * @param id the event id
   * @throws IllegalArgumentException if source is null
   */
  public WindowEvent(Window source, int id)
  {
    this(source, id, null, 0, 0);
  }

  /**
   * Returns the event source as a <code>Window</code>. If the source has
   * subsequently been modified to a non-Window, this returns null.
  *
  * @return the event source as a <code>Window</code>
  */
  public Window getWindow()
  {
    return source instanceof Window ? (Window) source : null;
  }

  /**
   * Returns the opposite window if this window was involved in an activation
   * or focus change. For WINDOW_ACTIVATED and WINDOW_GAINED_FOCUS events,
   * this is the window that lost focus; for WINDOW_DEACTIVATED and
   * WINDOW_LOST_FOCUS, this is the window that stole focus; and for other
   * events (or when native implementation does not have the data available),
   * this is null.
   *
   * @return the opposite window, or null
   * @since 1.4
   */
  public Window getOppositeWindow()
  {
    return opposite;
  }

  /**
   * Returns the state of this window before the event. This is the bitwise
   * or of fields in Frame: NORMAL, ICONIFIED, MAXIMIZED_HORIZ, MAXIMIZED_VERT,
   * and MAXIMIZED_BOTH.
   *
   * @return the former state
   * @see Frame#getExtendedState()
   * @since 1.4
   */
  public int getOldState()
  {
    return oldState;
  }

  /**
   * Returns the state of this window after the event. This is the bitwise
   * or of fields in Frame: NORMAL, ICONIFIED, MAXIMIZED_HORIZ, MAXIMIZED_VERT,
   * and MAXIMIZED_BOTH.
   *
   * @return the updated state
   * @see Frame#getExtendedState()
   * @since 1.4
   */
  public int getNewState()
  {
    return newState;
  }

  /**
   * Returns a string that identifies this event. This is formatted as the
   * field name of the id, followed by the opposite window, old state, and
   * new state.
   *
   * @return a string that identifies this event
   */
  public String paramString()
  {
    CPStringBuilder s = new CPStringBuilder();
    switch (id)
      {
      case WINDOW_OPENED:
        s.append("WINDOW_OPENED,opposite=");
        break;
      case WINDOW_CLOSING:
        s.append("WINDOW_CLOSING,opposite=");
        break;
      case WINDOW_CLOSED:
        s.append("WINDOW_CLOSED,opposite=");
        break;
      case WINDOW_ICONIFIED:
        s.append("WINDOW_ICONIFIED,opposite=");
        break;
      case WINDOW_DEICONIFIED:
        s.append("WINDOW_DEICONIFIED,opposite=");
        break;
      case WINDOW_ACTIVATED:
        s.append("WINDOW_ACTIVATED,opposite=");
        break;
      case WINDOW_DEACTIVATED:
        s.append("WINDOW_DEACTIVATED,opposite=");
        break;
      case WINDOW_GAINED_FOCUS:
        s.append("WINDOW_GAINED_FOCUS,opposite=");
        break;
      case WINDOW_LOST_FOCUS:
        s.append("WINDOW_LOST_FOCUS,opposite=");
        break;
      case WINDOW_STATE_CHANGED:
        s.append("WINDOW_STATE_CHANGED,opposite=");
        break;
      default:
        s.append("unknown type,opposite=");
      }
    return s.append(opposite).append(",oldState=").append(oldState)
      .append(",newState=").append(newState).toString();
  }
} // class WindowEvent
