/* InputEvent.java -- common superclass of component input events
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import gnu.java.awt.EventModifier;

import java.awt.Component;

/**
 * This is the common superclass for all component input classes. These are
 * passed to listeners before the component, so that listeners can consume
 * the event before it does its default behavior.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see KeyEvent
 * @see KeyAdapter
 * @see MouseEvent
 * @see MouseAdapter
 * @see MouseMotionAdapter
 * @see MouseWheelEvent
 * @since 1.1
 * @status updated to 1.4
 */
public abstract class InputEvent extends ComponentEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -2482525981698309786L;

  /**
   * This is the bit mask which indicates the shift key is down. It is
   * recommended that SHIFT_DOWN_MASK be used instead.
   *
   * @see #SHIFT_DOWN_MASK
   */
  public static final int SHIFT_MASK = 1;

  /**
   * This is the bit mask which indicates the control key is down. It is
   * recommended that CTRL_DOWN_MASK be used instead.
   *
   * @see #CTRL_DOWN_MASK
   */
  public static final int CTRL_MASK = 2;

  /**
   * This is the bit mask which indicates the meta key is down. It is
   * recommended that META_DOWN_MASK be used instead.
   *
   * @see #META_DOWN_MASK
   */
  public static final int META_MASK = 4;

  /**
   * This is the bit mask which indicates the alt key is down. It is
   * recommended that ALT_DOWN_MASK be used instead.
   *
   * @see #ALT_DOWN_MASK
   */
  public static final int ALT_MASK = 8;

  /**
   * This is the bit mask which indicates the alt-graph modifier is in effect.
   * It is recommended that ALT_GRAPH_DOWN_MASK be used instead.
   *
   * @see #ALT_GRAPH_DOWN_MASK
   */
  public static final int ALT_GRAPH_MASK = 0x20;

  /**
   * This bit mask indicates mouse button one is down. It is recommended that
   * BUTTON1_DOWN_MASK be used instead.
   *
   * @see #BUTTON1_DOWN_MASK
   */
  public static final int BUTTON1_MASK = 0x10;

  /**
   * This bit mask indicates mouse button two is down. It is recommended that
   * BUTTON2_DOWN_MASK be used instead.
   *
   * @see #BUTTON2_DOWN_MASK
   */
  public static final int BUTTON2_MASK = 8;

  /**
   * This bit mask indicates mouse button three is down. It is recommended
   * that BUTTON3_DOWN_MASK be used instead.
   *
   * @see #BUTTON3_DOWN_MASK
   */
  public static final int BUTTON3_MASK = 4;

  /**
   * The SHIFT key extended modifier.
   *
   * @since 1.4
   */
  public static final int SHIFT_DOWN_MASK = 0x0040;

  /**
   * The CTRL key extended modifier.
   *
   * @since 1.4
   */
  public static final int CTRL_DOWN_MASK = 0x0080;

  /**
   * The META key extended modifier.
   *
   * @since 1.4
   */
  public static final int META_DOWN_MASK = 0x0100;

  /**
   * The ALT key extended modifier.
   *
   * @since 1.4
   */
  public static final int ALT_DOWN_MASK = 0x0200;

  /**
   * The mouse button1 key extended modifier.
   *
   * @since 1.4
   */
  public static final int BUTTON1_DOWN_MASK = 0x0400;

  /**
   * The mouse button2 extended modifier.
   *
   * @since 1.4
   */
  public static final int BUTTON2_DOWN_MASK = 0x0800;

  /**
   * The mouse button3 extended modifier.
   *
   * @since 1.4
   */
  public static final int BUTTON3_DOWN_MASK = 0x1000;

  /**
   * The ALT_GRAPH key extended modifier.
   *
   * @since 1.4
   */
  public static final int ALT_GRAPH_DOWN_MASK = 0x2000;

  /** The mask to convert new to old, package visible for use in subclasses. */
  static final int CONVERT_MASK
    = EventModifier.NEW_MASK & ~(BUTTON2_DOWN_MASK | BUTTON3_DOWN_MASK);

  /**
   * The timestamp when this event occurred.
   *
   * @see #getWhen()
   * @serial the timestamp
   */
  private final long when;

  /**
   * The modifiers in effect for this event. Package visible for use by
   * subclasses. The old style (bitmask 0x3f) should not be mixed with the
   * new style (bitmasks 0xffffffc0).
   *
   * @see #getModifiers()
   * @see MouseEvent
   * @serial the modifier state, stored in the new style
   */
  int modifiers;

  /**
   * Initializes a new instance of <code>InputEvent</code> with the specified
   * source, id, timestamp, and modifiers. Note that an invalid id leads to
   * unspecified results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param when the timestamp when the event occurred
   * @param modifiers the modifiers in effect for this event, old or new style
   * @throws IllegalArgumentException if source is null
   */
  InputEvent(Component source, int id, long when, int modifiers)
  {
    super(source, id);
    this.when = when;
    this.modifiers = EventModifier.extend(modifiers);
  }

  /**
   * This method tests whether or not the shift key was down during the event.
   *
   * @return true if the shift key is down
   */
  public boolean isShiftDown()
  {
    return (modifiers & SHIFT_DOWN_MASK) != 0;
  }

  /**
   * This method tests whether or not the control key was down during the
   * event.
   *
   * @return true if the control key is down
   */
  public boolean isControlDown()
  {
    return (modifiers & CTRL_DOWN_MASK) != 0;
  }

  /**
   * This method tests whether or not the meta key was down during the event.
   *
   * @return true if the meta key is down
   */
  public boolean isMetaDown()
  {
    return (modifiers & META_DOWN_MASK) != 0;
  }

  /**
   * This method tests whether or not the alt key was down during the event.
   *
   * @return true if the alt key is down
   */
  public boolean isAltDown()
  {
    return (modifiers & ALT_DOWN_MASK) != 0;
  }

  /**
   * This method tests whether or not the alt-graph modifier was in effect
   * during the event.
   *
   * @return true if the alt-graph modifier is down
   */
  public boolean isAltGraphDown()
  {
    return (modifiers & ALT_GRAPH_DOWN_MASK) != 0;
  }

  /**
   * This method returns the timestamp when this event occurred.
   *
   * @return the timestamp when this event occurred
   */
  public long getWhen()
  {
    return when;
  }

  /**
   * This method returns the old-style modifiers in effect for this event. 
   * Note that this is ambiguous between button2 and alt, and between
   * button3 and meta. Also, code which generated these modifiers tends to
   * only list the modifier that just changed, even if others were down at
   * the time. Consider using getModifiersEx instead. This will be a union
   * of the bit masks defined in this class that are applicable to the event.
   *
   * @return the modifiers in effect for this event
   * @see #getModifiersEx()
   */
  public int getModifiers()
  {
    return EventModifier.revert(modifiers);
  }

  /**
   * Returns the extended modifiers (new-style) for this event. This represents
   * the state of all modal keys and mouse buttons at the time of the event,
   * and does not suffer from the problems mentioned in getModifiers.
   *
   * <p>For an example of checking multiple modifiers, this code will return
   * true only if SHIFT and BUTTON1 were pressed and CTRL was not:
   * <pre>
   * int onmask = InputEvent.SHIFT_DOWN_MASK | InputEvent.BUTTON1_DOWN_MASK;
   * int offmask = InputEvent.CTRL_DOWN_MASK;
   * return (event.getModifiersEx() & (onmask | offmask)) == onmask;
   * </pre>
   *
   * @return the bitwise or of all modifiers pressed during the event
   * @since 1.4
   */
  public int getModifiersEx()
  {
    return modifiers;
  }

  /**
   * Consumes this event.  A consumed event is not processed further by the AWT
   * system.
   */
  public void consume()
  {
    consumed = true;
  }

  /**
   * This method tests whether or not this event has been consumed.
   *
   * @return true if this event has been consumed
   */
  public boolean isConsumed()
  {
    return consumed;
  }

  /**
   * Convert the extended modifier bitmask into a String, such as "Shift" or
   * "Ctrl+Button1".
   *
   * XXX Sun claims this can be localized via the awt.properties file - how
   * do we implement that?
   *
   * @param modifiers the modifiers
   * @return a string representation of the modifiers in this bitmask
   * @since 1.4
   */
  public static String getModifiersExText(int modifiers)
  {
    modifiers &= EventModifier.NEW_MASK;
    if (modifiers == 0)
      return "";
    StringBuffer s = new StringBuffer();
    if ((modifiers & META_DOWN_MASK) != 0)
      s.append("Meta+");
    if ((modifiers & CTRL_DOWN_MASK) != 0)
      s.append("Ctrl+");
    if ((modifiers & ALT_DOWN_MASK) != 0)
      s.append("Alt+");
    if ((modifiers & SHIFT_DOWN_MASK) != 0)
      s.append("Shift+");
    if ((modifiers & ALT_GRAPH_DOWN_MASK) != 0)
      s.append("Alt Graph+");
    if ((modifiers & BUTTON1_DOWN_MASK) != 0)
      s.append("Button1+");
    if ((modifiers & BUTTON2_DOWN_MASK) != 0)
      s.append("Button2+");
    if ((modifiers & BUTTON3_DOWN_MASK) != 0)
      s.append("Button3+");
    return s.substring(0, s.length() - 1);
  }
} // class InputEvent
