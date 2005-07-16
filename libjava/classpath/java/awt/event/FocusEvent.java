/* FocusEvent.java -- generated for a focus change
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

import java.awt.Component;

/**
 * This class represents an event generated when a focus change occurs for a
 * component. There are both temporary changes, such as when focus is stolen
 * during a sroll then returned, and permanent changes, such as when the user
 * TABs through focusable components.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see FocusAdapter
 * @see FocusListener
 * @since 1.1
 * @status updated to 1.4
 */
public class FocusEvent extends ComponentEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 523753786457416396L;

  /** This is the first id in the range of ids used by this class. */
  public static final int FOCUS_FIRST = 1004;

  /** This is the last id in the range of ids used by this class. */
  public static final int FOCUS_LAST = 1005;

  /** This is the event id for a focus gained event. */
  public static final int FOCUS_GAINED = 1004;

  /** This is the event id for a focus lost event. */
  public static final int FOCUS_LOST = 1005;

  /**
   * Indicates whether or not the focus change is temporary.
   *
   * @see #isTemporary()
   * @serial true if the focus change is temporary
   */
  private final boolean temporary;

  /**
   * The other component which is giving up or stealing focus from this
   * component, if known.
   *
   * @see #getOppositeComponent()
   * @serial the component with the opposite focus event, or null
   * @since 1.4
   */
  private final Component opposite;

  /**
   * Initializes a new instance of <code>FocusEvent</code> with the
   * specified source, id, temporary status, and opposite counterpart. Note
   * that an invalid id leads to unspecified results.
   *
   * @param source the component that is gaining or losing focus
   * @param id the event id
   * @param temporary true if the focus change is temporary
   * @param opposite the component receiving the opposite focus event, or null
   * @throws IllegalArgumentException if source is null
   */
  public FocusEvent(Component source, int id, boolean temporary,
                    Component opposite)
  {
    super(source, id);
    this.temporary = temporary;
    this.opposite = opposite;
  }

  /**
   * Initializes a new instance of <code>FocusEvent</code> with the
   * specified source, id, and temporary status. Note that an invalid id
   * leads to unspecified results.
   *
   * @param source the component that is gaining or losing focus
   * @param id the event id
   * @param temporary true if the focus change is temporary
   * @throws IllegalArgumentException if source is null
   */
  public FocusEvent(Component source, int id, boolean temporary)
  {
    this(source, id, temporary, null);
  }

  /**
   * Initializes a new instance of <code>FocusEvent</code> with the
   * specified source and id. Note that an invalid id leads to unspecified
   * results.
   *
   * @param source the component that is gaining or losing focus
   * @param id the event id
   * @throws IllegalArgumentException if source is null
   */
  public FocusEvent(Component source, int id)
  {
    this(source, id, false, null);
  }

  /**
   * This method tests whether or not the focus change is temporary or
   * permanent.
   *
   * @return true if the focus change is temporary
   */
  public boolean isTemporary()
  {
    return temporary;
  }

  /**
   * Returns the component which received the opposite focus event. If this
   * component gained focus, the opposite lost focus; likewise if this
   * component is giving up focus, the opposite is gaining it. If this
   * information is unknown, perhaps because the opposite is a native
   * application, this returns null.
   *
   * @return the component with the focus opposite, or null
   * @since 1.4
   */
  public Component getOppositeComponent()
  {
    return opposite;
  }

  /**
   * Returns a string identifying this event. This is formatted as:
   * <code>(getID() == FOCUS_GAINED ? "FOCUS_GAINED" : "FOCUS_LOST")
   * + (isTemporary() ? ",temporary," : ",permanent,") + "opposite="
   * + getOppositeComponent()</code>.
   *
   * @return a string identifying this event
   */
  public String paramString()
  {
    return (id == FOCUS_GAINED ? "FOCUS_GAINED"
            : id == FOCUS_LOST ? "FOCUS_LOST" : "unknown type")
      + (temporary ? ",temporary,opposite=" : ",permanent,opposite=")
      + opposite;
  }
} // class FocusEvent
