/* ActionEvent.java -- an action has been triggered
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

import java.awt.AWTEvent;
import java.awt.EventQueue;

/**
 * This event is generated when an action on a component (such as a
 * button press) occurs.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see ActionListener
 * @since 1.1
 * @status updated to 1.4
 */
public class ActionEvent extends AWTEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -7671078796273832149L;

  /** Bit mask indicating the shift key was pressed. */
  public static final int SHIFT_MASK = InputEvent.SHIFT_MASK;

  /** Bit mask indicating the control key was pressed. */
  public static final int CTRL_MASK = InputEvent.CTRL_MASK;

  /** Bit mask indicating the that meta key was pressed. */
  public static final int META_MASK = InputEvent.META_MASK;

  /** Bit mask indicating that the alt key was pressed. */
  public static final int ALT_MASK = InputEvent.ALT_MASK;

  /** The first id number in the range of action id's. */
  public static final int ACTION_FIRST = 1001;

  /** The last id number in the range of action id's. */
  public static final int ACTION_LAST = 1001;

  /** An event id indicating that an action has occurred. */
  public static final int ACTION_PERFORMED = 1001;

  /**
   * A nonlocalized string that gives more specific details of the event cause.
   *
   * @see #getActionCommand()
   * @serial the command for this event
   */
  private final String actionCommand;

  /**
   * The bitmask of the modifiers that were pressed during the action.
   *
   * @see #getModifiers()
   * @serial modifiers for this event
   */
  private final int modifiers;

  /**
   * The timestamp of this event; usually the same as the underlying input
   * event.
   *
   * @see #getWhen()
   * @serial the timestamp of the event
   * @since 1.4
   */
  private final long when;

  /**
   * Initializes a new instance of <code>ActionEvent</code> with the
   * specified source, id, and command. Note that an invalid id leads to
   * unspecified results.
   *
   * @param source the event source
   * @param id the event id
   * @param command the command string for this action
   * @throws IllegalArgumentException if source is null
   */
  public ActionEvent(Object source, int id, String command)
  {
    this(source, id, command, EventQueue.getMostRecentEventTime(), 0);
  }

  /**
   * Initializes a new instance of <code>ActionEvent</code> with the
   * specified source, id, command, and modifiers. Note that an invalid id
   * leads to unspecified results.
   *
   * @param source the event source
   * @param id the event id
   * @param command the command string for this action
   * @param modifiers the bitwise or of modifier keys down during the action
   * @throws IllegalArgumentException if source is null
   */
  public ActionEvent(Object source, int id, String command, int modifiers)
  {
    this(source, id, command, EventQueue.getMostRecentEventTime(), modifiers);
  }

  /**
   * Initializes a new instance of <code>ActionEvent</code> with the
   * specified source, id, command, and modifiers, and timestamp. Note that
   * an invalid id leads to unspecified results.
   *
   * @param source the event source
   * @param id the event id
   * @param command the command string for this action
   * @param when the timestamp of the event
   * @param modifiers the bitwise or of modifier keys down during the action
   * @throws IllegalArgumentException if source is null
   * @since 1.4
   */
  public ActionEvent(Object source, int id, String command, long when,
                     int modifiers)
  {
    super(source, id);
    actionCommand = command;
    this.when = when;
    this.modifiers = modifiers;
  }

  /**
   * Returns the command string associated with this action.
   *
   * @return the command string associated with this action
   */
  public String getActionCommand()
  {
    return actionCommand;
  }

  /**
   * Gets the timestamp of when this action took place. Usually, this
   * corresponds to the timestamp of the underlying InputEvent.
   *
   * @return the timestamp of this action
   * @since 1.4
   */
  public long getWhen()
  {
    return when;
  }

  /**
   * Returns the keys held down during the action.  This value will be a
   * combination of the bit mask constants defined in this class, or 0 if no
   * modifiers were pressed.
   *
   * @return the modifier bits
   */
  public int getModifiers()
  {
    return modifiers;
  }

  /**
   * Returns a string that identifies the action event. This is in the format
   * <code>"ACTION_PERFORMED,cmd=" + getActionCommand() + ",when=" + getWhen()
   * + ",modifiers=" + &lt;modifier string&gt;</code>, where the modifier
   * string is in the order "Meta", "Ctrl", "Alt", "Shift", "Alt Graph", and
   * "Button1", separated by '+', according to the bits set in getModifiers().
   *
   * @return a string identifying the event
   */
  public String paramString()
  {
    CPStringBuilder s = new CPStringBuilder(id == ACTION_PERFORMED
					    ? "ACTION_PERFORMED,cmd="
					    : "unknown type,cmd=");
    s.append(actionCommand).append(",when=").append(when).append(",modifiers");
    int len = s.length();
    s.setLength(len + 1);
    if ((modifiers & META_MASK) != 0)
      s.append("+Meta");
    if ((modifiers & CTRL_MASK) != 0)
      s.append("+Ctrl");
    if ((modifiers & ALT_MASK) != 0)
      s.append("+Alt");
    if ((modifiers & SHIFT_MASK) != 0)
      s.append("+Shift");
    if ((modifiers & InputEvent.ALT_GRAPH_MASK) != 0)
      s.append("+Alt Graph");
    if ((modifiers & InputEvent.BUTTON1_MASK) != 0)
      s.append("+Button1");
    s.setCharAt(len, '=');
    return s.toString();
  }
} // class ActionEvent 
