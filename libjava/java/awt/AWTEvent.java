/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

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

/* Written using on-line Java 2 Platform Standard Edition v1.3 API 
 * Specification, as well as "The Java Class Libraries", 2nd edition 
 * (Addison-Wesley, 1998).
 * Status:  Believed complete and correct, except for the java.awt.Event 
 * compatibility constructor.
 */

/**
 * AWTEvent is the root event class for all AWT events in the JDK 1.1 event 
 * model. It supersedes the Event class from JDK 1.0.
 * @author Warren Levy  <warrenl@cygnus.com>
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public abstract class AWTEvent extends java.util.EventObject
{
  /**
   * @serial Indicates whether or not this event has been consumed.
   */
  protected boolean consumed;

  /**
   * @serial The identifier number of this event.
   */
  protected int id;

  /**
  * Mask for selecting component events.
  */
  public static final long COMPONENT_EVENT_MASK = 0x001;

  /**
  * Mask for selecting container events.
  */
  public static final long CONTAINER_EVENT_MASK = 0x002;

  /**
  * Mask for selecting component focus events.
  */
  public static final long FOCUS_EVENT_MASK = 0x004;

  /**
  * Mask for selecting keyboard events.
  */
  public static final long KEY_EVENT_MASK = 0x008;

  /**
  * Mask for mouse button events.
  */
  public static final long MOUSE_EVENT_MASK = 0x010;

  /**
  * Mask for mouse motion events.
  */
  public static final long MOUSE_MOTION_EVENT_MASK = 0x020;

  /**
  * Mask for window events.
  */
  public static final long WINDOW_EVENT_MASK = 0x040;

  /**
  * Mask for action events.
  */
  public static final long ACTION_EVENT_MASK = 0x080;

  /**
  * Mask for adjustment events.
  */
  public static final long ADJUSTMENT_EVENT_MASK = 0x100;

  /**
  * Mask for item events.
  */
  public static final long ITEM_EVENT_MASK = 0x200;

  /**
  * Mask for text events.
  */
  public static final long TEXT_EVENT_MASK = 0x400;

  /**
  * This is the highest number for event ids that are reserved for use by
  * the AWT system itself.
  */
  public static final int RESERVED_ID_MAX = 1999;

  public static final long INPUT_METHOD_EVENT_MASK = 1 << 11;

  /* Additional event selection masks from JDK 1.3 javadocs */
  public static final long PAINT_EVENT_MASK            = 1 << 13,
			   INVOCATION_EVENT_MASK       = 1 << 14,
			   HIERARCHY_EVENT_MASK        = 1 << 15,
			   HIERARCHY_BOUNDS_EVENT_MASK = 1 << 16;

  /**
   * Initializes a new instance of <code>AWTEvent</code> from the
   * specified Java 1.0 event object.
   *
   * @param event The Java 1.0 event to initialize from.
   *
   *
   * Removed this method because we no longer support Java 1.0
   *
   */
  public AWTEvent(Event event)
  {
    // FIXME??
    super(event.target);
    this.id = event.id;
  }

  /**
   * Initializes a new instance of <code>AWTEvent</code> with the specified
   * source and id.
   *
   * @param source The object that caused the event.
   * @param id The event id.
   */
  public AWTEvent(Object source, int id)
  {
    super(source);
    this.id = id;
  }

  /**
   * Returns the id number of this event.
   *
   * @return The id number of this event.
   */
  public int getID()
  {
    return id;
  }

  /**
   * Returns a string representation of this event.
   *
   * @return A string representation of this event.
   */
  public String paramString ()
  {
    return "";
  }

  /**
   * Returns a string representation of this event.
   *
   * @return A string representation of this event.
   */
  public String toString ()
  {
    return getClass().getName() + "[" + paramString() + "] on " + source;
  }

  /**
   * Consumes this event so that it will not be processed in the default
   * manner.
   */
  protected void consume()
  {
    consumed = true;
  }

  /**
   * Tests whether not not this event has been consumed.  A consumed event
   * is not processed in the default manner.
   *
   * @return <code>true</code> if this event has been consumed, 
   * <code>false</code> otherwise.
   */
  protected boolean isConsumed()
  {
    return consumed;
  }
}
