/* AccessibleState.java -- a state of an accessible object
   Copyright (C) 2002, 2005 Free Software Foundation

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

package javax.accessibility;

import java.awt.Dimension;
import java.util.Locale;

/**
 * A state portion of an accessible object. A combination of states represent
 * the entire object state, in an AccessibleStateSet. For example, this could
 * be "active" or "selected". This strongly typed "enumeration" supports
 * localized strings. If the constants of this class are not adequate, new
 * ones may be added in a similar matter, while avoiding a public constructor.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.2
 * @status updated to 1.4
 */
public class AccessibleState extends AccessibleBundle
{
  /**
   * Indicates an active window, as well as an active child in a list or other
   * collection.
   *
   * @see AccessibleRole#WINDOW
   * @see AccessibleRole#FRAME
   * @see AccessibleRole#DIALOG
   */
  public static final AccessibleState ACTIVE
    = new AccessibleState("active");

  /**
   * Indicates a pushed button, usually when the mouse has been pressed but
   * not released.
   *
   * @see AccessibleRole#PUSH_BUTTON
   */
  public static final AccessibleState PRESSED
    = new AccessibleState("pressed");

  /**
   * Indicates an armed object, usually a button which has been pushed and
   * the mouse has not left the button area.
   *
   * @see AccessibleRole#PUSH_BUTTON
   */
  public static final AccessibleState ARMED
    = new AccessibleState("armed");

  /**
   * Indicates an object is busy, such as a slider, scroll bar, or progress
   * bar in transition.
   *
   * @see AccessibleRole#PROGRESS_BAR
   * @see AccessibleRole#SCROLL_BAR
   * @see AccessibleRole#SLIDER
   */
  public static final AccessibleState BUSY
    = new AccessibleState("busy");

  /**
   * Indicates an object is checked.
   *
   * @see AccessibleRole#TOGGLE_BUTTON
   * @see AccessibleRole#RADIO_BUTTON
   * @see AccessibleRole#CHECK_BOX
   */
  public static final AccessibleState CHECKED
    = new AccessibleState("checked");

  /**
   * Indicates the user can edit the component contents. This is usually for
   * text, as other objects like scroll bars are automatically editable.
   *
   * @see #ENABLED
   */
  public static final AccessibleState EDITABLE
    = new AccessibleState("editable");

  /**
   * Indicates the object allows progressive disclosure of its children,
   * usually in a collapsible tree or other hierachical object.
   *
   * @see #EXPANDED
   * @see #COLLAPSED
   * @see AccessibleRole#TREE
   */
  public static final AccessibleState EXPANDABLE
    = new AccessibleState("expandable");

  /**
   * Indicates that the object is collapsed, usually in a tree.
   *
   * @see #EXPANDABLE
   * @see #EXPANDED
   * @see AccessibleRole#TREE
   */
  public static final AccessibleState COLLAPSED
    = new AccessibleState("collapsed");

  /**
   * Indicates that the object is expanded, usually in a tree.
   *
   * @see #EXPANDABLE
   * @see #COLLAPSED
   * @see AccessibleRole#TREE
   */
  public static final AccessibleState EXPANDED
    = new AccessibleState("expanded");

  /**
   * Indicates that an object is enabled. In the absence of this state,
   * graphics are often grayed out, and cannot be manipulated.
   */
  public static final AccessibleState ENABLED
    = new AccessibleState("enabled");

  /**
   * Indicates that an object can accept focus, which means it will process
   * keyboard events when focused.
   *
   * @see #FOCUSED
   */
  public static final AccessibleState FOCUSABLE
    = new AccessibleState("focusable");

  /**
   * Indicates that an object has keyboard focus.
   *
   * @see #FOCUSABLE
   */
  public static final AccessibleState FOCUSED
    = new AccessibleState("focused");

  /**
   * Indicates that an object is minimized to an icon.
   *
   * @see AccessibleRole#FRAME
   * @see AccessibleRole#INTERNAL_FRAME
   */
  public static final AccessibleState ICONIFIED
    = new AccessibleState("iconified");

  /**
   * Indicates that the state of this particular object is
   * indeterminate.  This commonly occurs when an object is incapable
   * of representing the state by a single value.
   *
   * @since 1.5
   */
  public static final AccessibleState INDETERMINATE
    = new AccessibleState("indeterminate");

  /**
   * Indicates that this particular object manages a number of
   * subcomponents.  This is a common property of structures such as
   * trees and tables, which have a number of sub-elements such as
   * rows and columns.  The subcomponents should be left to the
   * object, and not managed by the application.
   *
   * @since 1.5
   */
  public static final AccessibleState MANAGES_DESCENDANTS
    = new AccessibleState("manages descendants");

  /**
   * Indicates that something must be done in the current object before
   * interaction is allowed on other windows, usually for dialogs.
   *
   * @see AccessibleRole#DIALOG
   */
  public static final AccessibleState MODAL
    = new AccessibleState("modal");

  /**
   * Indicates that all pixels in the object are painted. If this state is not
   * present, then the object has some degree of transparency, letting lower
   * panes show through.
   *
   * @see Accessible#getAccessibleContext()
   * @see AccessibleContext#getAccessibleComponent()
   * @see AccessibleComponent#getBounds()
   */
  public static final AccessibleState OPAQUE
    = new AccessibleState("opaque");

  /**
   * Indicates the size of this object is not fixed.
   *
   * @see Accessible#getAccessibleContext()
   * @see AccessibleContext#getAccessibleComponent()
   * @see AccessibleComponent#getSize()
   * @see AccessibleComponent#setSize(Dimension)
   */
  public static final AccessibleState RESIZABLE
    = new AccessibleState("resizable");

  /**
   * Indicates that multiple children can be selected at once.
   *
   * @see Accessible#getAccessibleContext()
   * @see AccessibleContext#getAccessibleSelection()
   * @see AccessibleSelection
   */
  public static final AccessibleState MULTISELECTABLE
    = new AccessibleState("multiselectable");

  /**
   * Indicates that this child is one which can be selected from its parent.
   *
   * @see #SELECTED
   * @see Accessible#getAccessibleContext()
   * @see AccessibleContext#getAccessibleSelection()
   * @see AccessibleSelection
   */
  public static final AccessibleState SELECTABLE
    = new AccessibleState("selectable");

  /**
   * Indicates that this child has been selected from its parent.
   *
   * @see #SELECTABLE
   * @see Accessible#getAccessibleContext()
   * @see AccessibleContext#getAccessibleSelection()
   * @see AccessibleSelection
   */
  public static final AccessibleState SELECTED
    = new AccessibleState("selected");

  /**
   * Indicates that this object and all its parents are visible, so that it
   * is on the screen. However, something opaque may be on top of it.
   *
   * @see #VISIBLE
   */
  public static final AccessibleState SHOWING
    = new AccessibleState("showing");

  /**
   * Indicates that this particular object is truncated when displayed
   * visually.
   *
   * @since 1.5
   */
  public static final AccessibleState TRUNCATED
    = new AccessibleState("truncated");

  /**
   * Indicates that this object intends to be visible. However, if its
   * parent is invisible, this object is as well.
   *
   * @see #SHOWING
   */
  public static final AccessibleState VISIBLE
    = new AccessibleState("visible");

  /**
   * Indicates that an object has vertical orientation.
   *
   * @see #HORIZONTAL
   * @see AccessibleRole#SCROLL_BAR
   * @see AccessibleRole#SLIDER
   * @see AccessibleRole#PROGRESS_BAR
   */
  public static final AccessibleState VERTICAL
    = new AccessibleState("vertical");

  /**
   * Indicates that an object has horizontal orientation.
   *
   * @see #VERTICAL
   * @see AccessibleRole#SCROLL_BAR
   * @see AccessibleRole#SLIDER
   * @see AccessibleRole#PROGRESS_BAR
   */
  public static final AccessibleState HORIZONTAL
    = new AccessibleState("horizontal");

  /**
   * Indicates that this text object can only hold a single line.
   *
   * @see #MULTI_LINE
   */
  public static final AccessibleState SINGLE_LINE
    = new AccessibleState("single line");

  /**
   * Indicates that this text object can hold multiple lines.
   *
   * @see #SINGLE_LINE
   */
  public static final AccessibleState MULTI_LINE
    = new AccessibleState("multiple line");

  /**
   * Indicates that this object is transient. This means the object is
   * generated for method queries, but will never generate events, because
   * its container (such as a tree, list, or table) does all the work.
   */
  public static final AccessibleState TRANSIENT
    = new AccessibleState("transient");

  /**
   * Create a new constant with a locale independent key. Follow the example,
   * keep the constructor private and make public constants instead.
   *
   * @param key the name of the state
   * @see #toDisplayString(String, Locale)
   */
  protected AccessibleState(String key)
  {
    this.key = key;
  }
} // class AccessibleState
