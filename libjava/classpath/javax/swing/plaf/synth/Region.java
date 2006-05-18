/* Region.java -- Describes a region within a component
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.plaf.synth;

/**
 * Describes a region of a component or the complete component.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public class Region
{

  // FIXME: What should ui be for the non-component regions that have
  // subregion==false?

  /**
   * Specifies an arrow button region.
   */
  public static final Region ARROW_BUTTON =
    new Region("ArrowButton", null, false);

  /**
   * Specifies the region of a standard button.
   */
  public static final Region BUTTON =
    new Region("Button", "ButtonUI", false);

  /**
   * Specifies the region of a check box.
   */
  public static final Region CHECK_BOX =
    new Region("CheckBox", "CheckBoxUI", false);

  /**
   * Specifies the region of a check box menu item.
   */
  public static final Region CHECK_BOX_MENU_ITEM =
    new Region("CheckBoxMenuItem", "CheckBoxMenuItemUI", false);

  /**
   * Specifies the region of a colorchooser.
   */
  public static final Region COLOR_CHOOSER =
    new Region("ColorChooser", "ColorChooserUI", false);

  /**
   * Specifies the region of a combo box.
   */
  public static final Region COMBO_BOX =
    new Region("ComboBox", "ComboBoxUI", false);

  /**
   * Specifies the region of a desktop pane.
   */
  public static final Region DESKTOP_PANE =
    new Region("DesktopPane", "DesktopPaneUI", false);

  /**
   * Specifies the region of a desktop icon.
   */
  public static final Region DESKTOP_ICON =
    new Region("DesktopIcon", "DesktopIconUI", false);

  /**
   * Specifies the region of an editor pane.
   */
  public static final Region EDITOR_PANE =
    new Region("EditorPane", "EditorPaneUI", false);

  /**
   * Specifies the region of a file chooser.
   */
  public static final Region FILE_CHOOSER =
    new Region("FileChooser", "FileChooserUI", false);

  /**
   * Specifies the region of a formatted text field.
   */
  public static final Region FORMATTED_TEXT_FIELD =
    new Region("FormattedTextField", "FormattedTextFieldUI", false);

  /**
   * Specifies the region of an internal frame.
   */
  public static final Region INTERNAL_FRAME =
    new Region("InternalFrame", "InternalFrameUI", false);

  /**
   * Specifies the region of the title pane of an internal frame.
   */
  public static final Region INTERNAL_FRAME_TITLE_PANE =
    new Region("InternalFrameTitlePane", "InternalFrameTitlePaneUI", false);

  /**
   * Specifies the region of a label.
   */
  public static final Region LABEL =
    new Region("Label", "LabelUI", false);

  /**
   * Specifies the region of a list.
   */
  public static final Region LIST =
    new Region("List", "ListUI", false);

  /**
   * Specifies the region of a menu.
   */
  public static final Region MENU =
    new Region("Menu", "MenuUI", false);

  /**
   * Specifies the region of a menu bar.
   */
  public static final Region MENU_BAR =
    new Region("MenuBar", "MenuBarUI", false);

  /**
   * Specifies the region of a menu item.
   */
  public static final Region MENU_ITEM =
    new Region("MenuItem", "MenuItemUI", false);

  /**
   * Specifies the region of a menu item accelerator. This is a subregion
   * of menu item.
   */
  public static final Region MENU_ITEM_ACCELERATOR =
    new Region("MenuItemAccelerator", null, true);

  /**
   * Specifies the region of an option pane.
   */
  public static final Region OPTION_PANE =
    new Region("OptionPane", "OptionPaneUI", false);

  /**
   * Specifies the region of a panel.
   */
  public static final Region PANEL =
    new Region("Panel", "PanelUI", false);

  /**
   * Specifies the region of a password field.
   */
  public static final Region PASSWORD_FIELD =
    new Region("PasswordField", "PasswordFieldUI", false);

  /**
   * Specifies the region of a popup menu.
   */
  public static final Region POPUP_MENU =
    new Region("PopupMenu", "PopupMenuUI", false);

  /**
   * Specifies the region of a popup menu separator.
   */
  public static final Region POPUP_MENU_SEPARATOR =
    new Region("PopupMenuSeparator", null, false);

  /**
   * Specifies the region of a progress bar.
   */
  public static final Region PROGRESS_BAR =
    new Region("ProgressBar", "ProgressBarUI", false);

  /**
   * Specifies the region of a radio button.
   */
  public static final Region RADIO_BUTTON =
    new Region("RadioButton", "RadioButtonUI", false);

  /**
   * Specifies the region of a radio button menu item.
   */
  public static final Region RADIO_BUTTON_MENU_ITEM =
    new Region("RadioButtonMenuItem", "RadioButtonMenuItemUI", false);

  /**
   * Specifies the region of a root pane.
   */
  public static final Region ROOT_PANE =
    new Region("RootPane", "RootPaneUI", false);

  /**
   * Specifies the region of a scroll bar.
   */
  public static final Region SCROLL_BAR =
    new Region("ScrollBar", "ScrollBarUI", false);

  /**
   * Specifies the region of a scroll bar track. This is a subregion of
   * scroll bars.
   */
  public static final Region SCROLL_BAR_TRACK =
    new Region("ScrollBarTrack", null, true);

  /**
   * Specifies the region of a scroll bar thumb. This is a subregion of
   * scroll bars.
   */
  public static final Region SCROLL_BAR_THUMB =
    new Region("ScrollBarThumb", null, true);

  /**
   * Specifies the region of a scroll pane.
   */
  public static final Region SCROLL_PANE =
    new Region("ScrollPane", "ScrollPaneUI", false);

  /**
   * Specifies the region of a separator.
   */
  public static final Region SEPARATOR =
    new Region("Separator", "SeparatorUI", false);

  /**
   * Specifies the region of a slider.
   */
  public static final Region SLIDER =
    new Region("Slider", "SliderUI", false);

  /**
   * Specifies the region of a slider track. This is a subregion of a slider.
   */
  public static final Region SLIDER_TRACK =
    new Region("SliderTrack", null, true);

  /**
   * Specifies the region of a slider thumb. This is a subregion of a slider.
   */
  public static final Region SLIDER_THUMB =
    new Region("SliderThumb", null, true);

  /**
   * Specifies the region of a spinner.
   */
  public static final Region SPINNER =
    new Region("Spinner", "SpinnerUI", false);

  /**
   * Specifies the region of a split pane.
   */
  public static final Region SPLIT_PANE =
    new Region("SplitPane", "SplitPaneUI", false);

  /**
   * Specifies the region of a split pane divider. This is a subregion of
   * a split pane.
   */
  public static final Region SPLIT_PANE_DIVIDER =
    new Region("SplitPaneDivider", null, true);

  /**
   * Specifies the region of a tabbed pane.
   */
  public static final Region TABBED_PANE =
    new Region("TabbedPane", "TabbedPaneUI", false);

  /**
   * This specifies the region of a tab of a tabbed pane. This is a subregion
   * of a tabbed pane.
   */
  public static final Region TABBED_PANE_TAB =
    new Region("TabbedPaneTab", null, true);

  /**
   * This specifies the region underneath the tabs of a tabbed pane. This is a
   * subregion of a tabbed pane.
   */
  public static final Region TABBED_PANE_TAB_AREA =
    new Region("TabbedPaneTabArea", null, true);

  /**
   * This specifies the region for the content of a tabbed pane. This is a
   * subregion of a tabbed pane.
   */
  public static final Region TABBED_PANE_CONTENT =
    new Region("TabbedPaneContent", null, true);

  /**
   * Specifies the region of a table.
   */
  public static final Region TABLE =
    new Region("Table", "TableUI", false);

  /**
   * Specifies the region of a table header.
   */
  public static final Region TABLE_HEADER =
    new Region("TableHeader", "TableHeaderUI", false);

  /**
   * Specifies the region of a text area.
   */
  public static final Region TEXT_AREA =
    new Region("TextArea", "TextAreaUI", false);

  /**
   * Specifies the region of a text field.
   */
  public static final Region TEXT_FIELD =
    new Region("TextField", "TextFieldUI", false);

  /**
   * Specifies the region of a text pane.
   */
  public static final Region TEXT_PANE =
    new Region("TextPane", "TextPaneUI", false);

  /**
   * Specifies the region of a toggle button.
   */
  public static final Region TOGGLE_BUTTON =
    new Region("ToggleButton", "ToggleButtonUI", false);

  /**
   * Specifies the region of a tool bar.
   */
  public static final Region TOOL_BAR =
    new Region("ToolBar", "ToolBarUI", false);

  /**
   * Specifies the content region of a tool bar. This is a subregion of a tool
   * bar.
   */
  public static final Region TOOL_BAR_CONTENT =
    new Region("ToolBarContent", null, true);

  /**
   * Specifies the drag window region of a tool bar. This is a subregion of a
   * tool bar.
   */
  public static final Region TOOL_BAR_DRAG_WINDOW =
    new Region("ToolBarDragWindow", null, false);

  /**
   * Specifies the region of a tool tip.
   */
  public static final Region TOOL_TIP =
    new Region("ToolTip", "ToolTipUI", false);

  /**
   * Specifies the region of a separator of a tool bar. This is a subregion of
   * a tool bar.
   */
  public static final Region TOOL_BAR_SEPARATOR =
    new Region("ToolBarSeparator", null, false);

  /**
   * Specifies the region of a tree.
   */
  public static final Region TREE =
    new Region("Tree", "TreeUI", false);

  /**
   * Specifies the region of a tree cell. This is a subregion of a tree.
   */
  public static final Region TREE_CELL =
    new Region("TreeCell", null, true);

  /**
   * Specifies the region of a viewport.
   */
  public static final Region VIEWPORT =
    new Region("Viewport", "ViewportUI", false);


  /**
   * The UI class id for the region. This is package private because this will
   * be used by other classes in that package.
   */
  String ui;

  /**
   * The name of the region.
   */
  private String name;

  /**
   * If this region is a subregion or not.
   */
  private boolean subregion;

  /**
   * Creates a new <code>Region</code> with the specified name and ui ID.
   * The <code>ui</code> must be the same what
   * {@link javax.swing.JComponent#getUIClassID()} returns for toplevel regions. For
   * subregions this should be <code>null</code>.
   *
   * @param name the name of the region
   * @param ui the UI class ID of the region or <code>null</code> for
   *        subregions
   * @param subregion <code>true</code> if this region is a subregion,
   *        <code>false</code> otherwise
   */
  protected Region(String name, String ui, boolean subregion)
  {
    this.name = name;
    this.ui = ui;
    this.subregion = subregion;
  }

  /**
   * Returns <code>true</code> if this region describes a subregion of a
   * component, <code>false</code> if it describes a component region itself.
   *
   * @return <code>true</code> if this region describes a subregion of a
   *         component, <code>false</code> if it describes a component region
   *         itself
   */
  public boolean isSubregion()
  {
    return subregion;
  }

  /**
   * Returns the name of the region.
   *
   * @return the name of the region
   */
  public String getName()
  {
    return name;
  }

  /**
   * Returns the name of the region.
   *
   * @return  the name of the region
   */
  public String toString()
  {
    return name;
  }
}
