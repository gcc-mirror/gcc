/* ContainerEvent.java -- components added/removed from a container
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
import java.awt.Container;

/**
 * This event is generated when a component is added or removed from a
 * container.  Applications do not ordinarily need to handle these events
 * since the AWT system handles them internally.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see ContainerAdapter
 * @see ContainerListener
 * @since 1.1
 * @status updated to 1.4
 */
public class ContainerEvent extends ComponentEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = -4114942250539772041L;

  /** This is the first id in the id range used by this class. */
  public static final int CONTAINER_FIRST = 300;

  /** This is the last id in the id range used by this class. */
  public static final int CONTAINER_LAST = 301;

  /** This id indicates a component was added to the container. */
  public static final int COMPONENT_ADDED = 300;

  /** This id indicates a component was removed from the container. */
  public static final int COMPONENT_REMOVED = 301;

  /**
   * The non-null child component that was added or removed.
   *
   * @serial the child component that changed
   */
  private final Component child;

  /**
   * Initializes a new instance of <code>ContainerEvent</code> with the
   * specified source and id.  Additionally, the affected child component
   * is also passed as a parameter. Note that an invalid id leads to
   * unspecified results.
   *
   * @param source the source container of the event
   * @param id the event id
   * @param child the child component affected by this event
   * @throws IllegalArgumentException if source is null
   */
  public ContainerEvent(Component source, int id, Component child)
  {
    super(source, id);
    this.child = child;
  }

  /**
   * Returns the source of this event as a <code>Container</code>.
   *
   * @return the source of the event
   * @throws ClassCastException if the source is changed to a non-Container
   */
  public Container getContainer()
  {
    return (Container) source;
  }

  /**
   * This method returns the child object that was added or removed from
   * the container.
   *
   * @return the child object added or removed
   */
  public Component getChild()
  {
    return child;
  }

  /**
   * This method returns a string identifying this event. It is formatted as:
   * <code>(getID() == COMPONENT_ADDED ? "COMPONENT_ADDED"
   * : "COMPONENT_REMOVED") + ",child=" + getChild().getName()</code>.
   *
   * @return a string identifying this event
   */
  public String paramString()
  {
    // Unlike Sun, we don't throw NullPointerException if child is illegally
    // null.
    return (id == COMPONENT_ADDED ? "COMPONENT_ADDED,child="
            : id == COMPONENT_REMOVED ? "COMPONENT_REMOVED,child="
            : "unknown type,child=") + (child == null ? "" : child.getName());
  }
} // class ContainerEvent
