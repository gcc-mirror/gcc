/* HierarchyEvent.java -- generated for a change in hierarchy
   Copyright (C) 2000, 2002 Free Software Foundation

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
import java.awt.*;

/**
 * This class represents an event generated for an ancestor component which
 * may affect this component. These events normally do not need to be handled
 * by the application, since the AWT system automatically takes care of them.
 *
 * <p>There are two types of hierarchy events. The first type is handled by
 * HierarchyListener, and includes addition or removal of an ancestor, or
 * an ancestor changing its on-screen status (visible and/or displayble). The
 * second type is handled by HierarchyBoundsListener, and includes resizing
 * or moving of an ancestor.
 *
 * @author Bryce McKinlay
 * @see HierarchyListener
 * @see HierarchyBoundsAdapter
 * @see HierarchyBoundsListener
 * @since 1.3
 * @status updated to 1.4
 */
public class HierarchyEvent extends AWTEvent
{
  /**
   * Compatible with JDK 1.3+.
   */
  private static final long serialVersionUID = -5337576970038043990L;

  /** This is the first id in the range of ids used by this class. */
  public static final int HIERARCHY_FIRST = 1400;

  /** This id indicates that the hierarchy tree changed. */
  public static final int HIERARCHY_CHANGED = 1400;

  /** This id indicates that an ancestor was moved. */
  public static final int ANCESTOR_MOVED = 1401;

  /** This id indicates that an ancestor was resized. */
  public static final int ANCESTOR_RESIZED = 1402;

  /** This is the last id in the range of ids used by this class. */
  public static final int HIERARCHY_LAST = 1402;

  /** This indicates that the HIERARCHY_CHANGED is a changed parent. */
  public static final int PARENT_CHANGED = 1;

  /**
   * This indicates that the HIERARCHY_CHANGED is caused by a change in
   * displayability.
   *
   * @see Component#isDisplayable()
   * @see Component#addNotify()
   * @see Component#removeNotify()
   */
  public static final int DISPLAYABILITY_CHANGED = 2;

  /**
   * This indicates that the HIERARCHY_CHANGED is a changed visibility.
   *
   * @see Component#isShowing()
   * @see Component#addNotify()
   * @see Component#removeNotify()
   * @see Component#show()
   * @see Component#hide()
   */
  public static final int SHOWING_CHANGED = 4;

  /**
   * The component at the top of the changed hierarchy.
   *
   * @serial the top component changed
   */
  private final Component changed;

  /**
   * The parent of this component, either before or after the change depending
   * on the type of change.
   *
   * @serial the parent component changed
   */
  private final Container changedParent;

  /**
   * The bitmask of HIERARCHY_CHANGED event types.
   *
   * @serial the change flags
   */
  private final long changeFlags;

  /**
   * Initializes a new instance of <code>HierarchyEvent</code> with the
   * specified parameters. Note that an invalid id leads to unspecified
   * results.
   *
   * @param source the component whose hierarchy changed
   * @param id the event id
   * @param changed the top component in the tree of changed hierarchy
   * @param changedParent the updated parent of this object
   * @throws IllegalArgumentException if source is null
   */
  public HierarchyEvent(Component source, int id, Component changed,
                        Container changedParent)
  {
    this(source, id, changed, changedParent, 0);
  }

  /**
   * Initializes a new instance of <code>HierarchyEvent</code> with the
   * specified parameters. Note that an invalid id leads to unspecified
   * results.
   *
   * @param source the component whose hierarchy changed
   * @param id the event id
   * @param changed the top component in the tree of changed hierarchy
   * @param changedParent the updated parent of this object
   * @param changeFlags the bitmask of specific HIERARCHY_CHANGED events
   * @throws IllegalArgumentException if source is null
   */
  public HierarchyEvent(Component source, int id, Component changed,
                        Container changedParent, long changeFlags)
  {
    super(source, id);
    this.changed = changed;
    this.changedParent = changedParent;
    this.changeFlags = changeFlags;
  }

  /**
   * This method returns the event source as a <code>Component</code>. If the
   * source has subsequently been modified to a non-Component, this returns
   * null.
   *
   * @return the event source as a <code>Component</code>, or null
   */
  public Component getComponent()
  {
    return source instanceof Component ? (Component) source : null;
  }

  /**
   * Returns the component at the top of the hierarchy which changed.
   *
   * @return the top changed component
   */
  public Component getChanged()
  {
    return changed;
  }

  /**
   * Returns the parent of the component listed in <code>getChanged()</code>.
   * If the cause of this event was <code>Container.add</code>, this is the
   * new parent; if the cause was <code>Container.remove</code>, this is the
   * old parent; otherwise it is the unchanged parent.
   *
   * @return the parent container of the changed component
   */
  public Container getChangedParent()
  {
    return changedParent;
  }

  /**
   * If this is a HIERARCHY_CHANGED event, this returns a bitmask of the
   * types of changes that took place.
   *
   * @return the bitwise or of hierarchy change types, or 0
   * @see #PARENT_CHANGED
   * @see #DISPLAYABILITY_CHANGED
   * @see #SHOWING_CHANGED
   */
  public long getChangeFlags()
  {
    return changeFlags;
  }

  /**
   * This method returns a string identifying this event. This is the field
   * name of the id type, followed by a parenthesized listing of the changed
   * component and its parent container. In addition, if the type is
   * HIERARCHY_CHANGED, the flags preceed the changed component, in the
   * order PARENT_CHANGED, DISPLAYABILITY_CHANGED, and SHOWING_CHANGED.
   *
   * @return a string identifying this event
   */
  public String paramString()
  {
    StringBuffer r = new StringBuffer();
    switch (id)
      {
      case HIERARCHY_CHANGED:
        r.append("HIERARCHY_CHANGED (");
        if ((changeFlags & PARENT_CHANGED) != 0)
          r.append("PARENT_CHANGED,");
        if ((changeFlags & DISPLAYABILITY_CHANGED) != 0)
          r.append("DISPLAYABILITY_CHANGED,");
        if ((changeFlags & SHOWING_CHANGED) != 0)
          r.append("SHOWING_CHANGED,");
        break;
      case ANCESTOR_MOVED:
        r.append("ANCESTOR_MOVED (");
        break;
      case ANCESTOR_RESIZED:
        r.append("ANCESTOR_RESIZED (");
        break;
      default:
        return "unknown type";
      }
    r.append(changed).append(',').append(changedParent).append(')');
    return r.toString();
  }
} // class HierarchyEvent
