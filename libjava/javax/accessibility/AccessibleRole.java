/* AccessibleRole.java -- the primary role of an accessible object
   Copyright (C) 2002 Free Software Foundation

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

package javax.accessibility;

/**
 * The role of an accessible object. For example, this could be "button" or
 * "table". This strongly typed "enumeration" supports localized strings. If
 * the constants of this class are not adequate, new ones may be added in a
 * similar matter, while avoiding a public constructor.
 * 
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.2
 * @status updated to 1.4
 */
public class AccessibleRole extends AccessibleBundle
{
  /** The object alerts the user about something. */
  public static final AccessibleRole ALERT
    = new AccessibleRole("alert");

  /** The header for a column of data. */
  public static final AccessibleRole COLUMN_HEADER
    = new AccessibleRole("column header");

  /**
   * The object can be drawn into, and traps events.
   *
   * @see #FRAME
   * @see #GLASS_PANE
   * @see #LAYERED_PANE
   */
  public static final AccessibleRole CANVAS
    = new AccessibleRole("canvas");

  /**
   * A list of choices, which may optionally allow the user to create a new
   * choice.
   */
  public static final AccessibleRole COMBO_BOX
    = new AccessibleRole("combo box");

  /**
   * An iconified frame in a desktop.
   *
   * @see #DESKTOP_PANE
   * @see #INTERNAL_FRAME
   */
  public static final AccessibleRole DESKTOP_ICON
    = new AccessibleRole("desktop icon");

  /**
   * A frame-like object clipped by a desktop pane.
   *
   * @see #DESKTOP_ICON
   * @see #DESKTOP_PANE
   * @see #FRAME
   */
  public static final AccessibleRole INTERNAL_FRAME
    = new AccessibleRole("internal frame");

  /**
   * A pane which supports internal frames and their icons.
   *
   * @see #DESKTOP_ICON
   * @see #INTERNAL_FRAME
   */
  public static final AccessibleRole DESKTOP_PANE
    = new AccessibleRole("desktop pane");

  /**
   * A specialized pane for use in a dialog.
   *
   * @see #DIALOG
   */
  public static final AccessibleRole OPTION_PANE
    = new AccessibleRole("option pane");

  /**
   * A top level window with no title or border.
   *
   * @see #FRAME
   * @see #DIALOG
   */
  public static final AccessibleRole WINDOW
    = new AccessibleRole("window");

  /**
   * A top level window with title, menu bar, border, and so forth. It is
   * often the primary window of an application.
   *
   * @see #DIALOG
   * @see #CANVAS
   * @see #WINDOW
   */
  public static final AccessibleRole FRAME
    = new AccessibleRole("frame");

  /**
   * A top level window title bar and border. It is limited compared to a
   * frame, and is often a secondary window.
   *
   * @see #FRAME
   * @see #WINDOW
   */
  public static final AccessibleRole DIALOG
    = new AccessibleRole("dialog");

  /** A specialized dialog for choosing a color. */
  public static final AccessibleRole COLOR_CHOOSER
    = new AccessibleRole("color chooser");

  /**
   * A pane for navigating through directories.
   *
   * @see #FILE_CHOOSER
   */
  public static final AccessibleRole DIRECTORY_PANE
    = new AccessibleRole("directory pane");

  /**
   * A specialized dialog that allows a user to select a file.
   *
   * @see #DIRECTORY_PANE
   */
  public static final AccessibleRole FILE_CHOOSER
    = new AccessibleRole("file chooser");

  /** An object to fill space between other components. */
  public static final AccessibleRole FILLER
    = new AccessibleRole("filler");

  /** A hypertext anchor. */
  public static final AccessibleRole HYPERLINK
    = new AccessibleRole("hyperlink");

  /** A small picture to decorate components. */
  public static final AccessibleRole ICON
    = new AccessibleRole("icon");

  /** An object to label something in a graphic interface. */
  public static final AccessibleRole LABEL
    = new AccessibleRole("label");

  /**
   * A specialized pane with a glass pane and layered pane as children.
   *
   * @see #GLASS_PANE
   * @see #LAYERED_PANE
   */
  public static final AccessibleRole ROOT_PANE
    = new AccessibleRole("root pane");

  /**
   * A pane guaranteed to be painted on top of panes beneath it.
   *
   * @see #ROOT_PANE
   * @see #LAYERED_PANE
   */
  public static final AccessibleRole GLASS_PANE
    = new AccessibleRole("glass pane");

  /**
   * A specialized pane that allows drawing children in layers. This is often
   * used in menus and other visual components.
   *
   * @see #ROOT_PANE
   * @see #GLASS_PANE
   */
  public static final AccessibleRole LAYERED_PANE
    = new AccessibleRole("layered pane");

  /**
   * An object which presents a list of items for selection. Often contained
   * in a scroll pane.
   *
   * @see #SCROLL_PANE
   * @see #LIST_ITEM
   */
  public static final AccessibleRole LIST
    = new AccessibleRole("list");

  /**
   * An object which represents an item in a list. Often contained in a scroll
   * pane.
   *
   * @see #SCROLL_PANE
   * @see #LIST
   */
  public static final AccessibleRole LIST_ITEM
    = new AccessibleRole("list item");

  /**
   * An object usually at the top of a frame to list available menus.
   *
   * @see #MENU
   * @see #POPUP_MENU
   * @see #LAYERED_PANE
   */
  public static final AccessibleRole MENU_BAR
    = new AccessibleRole("menu bar");

  /**
   * A temporary window with a menu of options, which hides on selection.
   *
   * @see #MENU
   * @see #MENU_ITEM
   */
  public static final AccessibleRole POPUP_MENU
    = new AccessibleRole("popup menu");

  /**
   * An object usually in a menu bar which contains a list of actions to
   * perform. Such actions are usually associated with menu items or submenus.
   *
   * @see #MENU_BAR
   * @see #MENU_ITEM
   * @see #SEPARATOR
   * @see #RADIO_BUTTON
   * @see #CHECK_BOX
   * @see #POPUP_MENU
   */
  public static final AccessibleRole MENU
    = new AccessibleRole("menu");

  /**
   * An object usually in a menu with an action available for the user.
   *
   * @see #MENU_BAR
   * @see #SEPARATOR
   * @see #POPUP_MENU
   */
  public static final AccessibleRole MENU_ITEM
    = new AccessibleRole("menu item");

  /**
   * An object usually in a menu which separates logical sections of items.
   *
   * @see #MENU
   * @see #MENU_ITEM
   */
  public static final AccessibleRole SEPARATOR
    = new AccessibleRole("separator");

  /**
   * An object which presents a series of panels, usually via tabs along the
   * top. Children are all page tabs.
   *
   * @see #PAGE_TAB
   */
  public static final AccessibleRole PAGE_TAB_LIST
    = new AccessibleRole("page tab list");

  /**
   * An object in a page tab list, which contains the panel to display when
   * selected from the list.
   *
   * @see #PAGE_TAB_LIST
   */
  public static final AccessibleRole PAGE_TAB
    = new AccessibleRole("page tab");

  /** A generic container to group objects. */
  public static final AccessibleRole PANEL
    = new AccessibleRole("panel");

  /** An object used to track amount of a task that has completed. */
  public static final AccessibleRole PROGRESS_BAR
    = new AccessibleRole("progress bar");

  /** An object for passwords which should not be shown to the user. */
  public static final AccessibleRole PASSWORD_TEXT
    = new AccessibleRole("password text");

  /**
   * An object that can be manipulated to do something.
   *
   * @see #CHECK_BOX
   * @see #TOGGLE_BUTTON
   * @see #RADIO_BUTTON
   */
  public static final AccessibleRole PUSH_BUTTON
    = new AccessibleRole("push button");

  /**
   * A specialized button which can be on or off, with no separate indicator.
   *
   * @see #PUSH_BUTTON
   * @see #CHECK_BOX
   * @see #RADIO_BUTTON
   */
  public static final AccessibleRole TOGGLE_BUTTON
    = new AccessibleRole("toggle button");

  /**
   * A choice which can be on or off, and has a separate indicator.
   *
   * @see #PUSH_BUTTON
   * @see #TOGGLE_BUTTON
   * @see #RADIO_BUTTON
   */
  public static final AccessibleRole CHECK_BOX
    = new AccessibleRole("check box");

  /**
   * A specialized choice which toggles radio buttons in the group when it
   * is selected.
   *
   * @see #PUSH_BUTTON
   * @see #TOGGLE_BUTTON
   * @see #CHECK_BOX
   */
  public static final AccessibleRole RADIO_BUTTON
    = new AccessibleRole("radio button");

  /** The header for a row of data. */
  public static final AccessibleRole ROW_HEADER
    = new AccessibleRole("row header");

  /**
   * An object which allows an incremental view of a larger pane.
   *
   * @see #SCROLL_BAR
   * @see #VIEWPORT
   */
  public static final AccessibleRole SCROLL_PANE
    = new AccessibleRole("scroll pane");

  /**
   * An object which allows selection of the view in a scroll pane.
   *
   * @see #SCROLL_PANE
   */
  public static final AccessibleRole SCROLL_BAR
    = new AccessibleRole("scroll bar");

  /**
   * An object which represents the visual section in a scroll pane.
   *
   * @see #SCROLL_PANE
   */
  public static final AccessibleRole VIEWPORT
    = new AccessibleRole("viewport");

  /** An object which allows selection in a bounded range. */
  public static final AccessibleRole SLIDER
    = new AccessibleRole("slider");

  /**
   * A specialized pane which presents two other panels, and can often adjust
   * the divider between them.
   */
  public static final AccessibleRole SPLIT_PANE
    = new AccessibleRole("split pane");

  /** An object for presenting data in rows and columns. */
  public static final AccessibleRole TABLE
    = new AccessibleRole("table");

  /**
   * An object which represents text, usually editable by the user.
   *
   * @see #LABEL
   */
  public static final AccessibleRole TEXT
    = new AccessibleRole("text");

  /**
   * An object which represents a hierachical view of data. Subnodes can
   * often be expanded or collapsed.
   */
  public static final AccessibleRole TREE
    = new AccessibleRole("tree");

  /** A bar or pallete with buttons for common actions in an application. */
  public static final AccessibleRole TOOL_BAR
    = new AccessibleRole("tool bar");

  /**
   * An object which provides information about another object. This is often
   * displayed as a "help bubble" when a mouse hovers over the other object.
   */
  public static final AccessibleRole TOOL_TIP
    = new AccessibleRole("tool tip");

  /**
   * An AWT component with nothing else known about it.
   *
   * @see #SWING_COMPONENT
   * @see #UNKNOWN
   */
  public static final AccessibleRole AWT_COMPONENT
    = new AccessibleRole("AWT component");

  /**
   * A swing component with nothing else known about it.
   *
   * @see #AWT_COMPONENT
   * @see #UNKNOWN
   */
  public static final AccessibleRole SWING_COMPONENT
    = new AccessibleRole("SWING component");

  /**
   * An accessible object whose role is unknown.
   *
   * @see #AWT_COMPONENT
   * @see #SWING_COMPONENT
   */
  public static final AccessibleRole UNKNOWN
    = new AccessibleRole("unknown");

  /** A component with multiple labels of status information. */
  public static final AccessibleRole STATUS_BAR
    = new AccessibleRole("statusbar");

  /** A component which allows editing of Date and Time objects. */
  public static final AccessibleRole DATE_EDITOR
    = new AccessibleRole("dateeditor");

  /** A component with spinner arrows for simple numbers. */
  public static final AccessibleRole SPIN_BOX
    = new AccessibleRole("spinbox");

  /** A component for choosing fonts and their attributes. */
  public static final AccessibleRole FONT_CHOOSER
    = new AccessibleRole("fontchooser");

  /** A component with a border to group other components. */
  public static final AccessibleRole GROUP_BOX
    = new AccessibleRole("groupbox");

  /**
   * Create a new constant with a locale independent key. Follow the example,
   * keep the constructor private and make public constants instead.
   *
   * @param key the name of the role
   * @see #toDisplayString(String, Locale)
   */
  protected AccessibleRole(String key)
  {
    this.key = key;
  }
} // class AccessibleRole
