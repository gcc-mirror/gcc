/* BasicLookAndFeel.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.UIDefaults;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.text.JTextComponent;

/**
 * BasicLookAndFeel
 * @author Andrew Selkirk
 */
public abstract class BasicLookAndFeel extends LookAndFeel
  implements Serializable
{
  static final long serialVersionUID = -6096995660290287879L;

  /**
   * Constructor BasicLookAndFeel
   */
  public BasicLookAndFeel()
  {
    // TODO
  }

  /**
   * getDefaults
   * @return UIDefaults
   */
  public UIDefaults getDefaults()
  {
    // Variables
    UIDefaults def = new UIDefaults();
    // Initialize Class Defaults
    initClassDefaults(def);
    // Initialize System Colour Defaults
    initSystemColorDefaults(def);
    // Initialize Component Defaults
    initComponentDefaults(def);
    // Return UI Defaults
    return def;
  }

  /**
   * initClassDefaults
   * @param value0 TODO
   */
  protected void initClassDefaults(UIDefaults defaults)
  {
    // Variables
    Object[] uiDefaults;
    // Initialize Class Defaults
    uiDefaults = new Object[] {
      "ButtonUI", "javax.swing.plaf.basic.BasicButtonUI",
      "CheckBoxMenuItemUI", "javax.swing.plaf.basic.BasicCheckBoxMenuItemUI",
      "CheckBoxUI", "javax.swing.plaf.basic.BasicCheckBoxUI",
      "ColorChooserUI", "javax.swing.plaf.basic.BasicColorChooserUI",
      "ComboBoxUI", "javax.swing.plaf.basic.BasicComboBoxUI",
      "DesktopIconUI", "javax.swing.plaf.basic.BasicDesktopIconUI",
      "DesktopPaneUI", "javax.swing.plaf.basic.BasicDesktopPaneUI",
      "EditorPaneUI", "javax.swing.plaf.basic.BasicEditorPaneUI",
      "FormattedTextFieldUI", "javax.swing.plaf.basic.BasicFormattedTextFieldUI",
      "InternalFrameUI", "javax.swing.plaf.basic.BasicInternalFrameUI",
      "LabelUI", "javax.swing.plaf.basic.BasicLabelUI",
      "ListUI", "javax.swing.plaf.basic.BasicListUI",
      "MenuBarUI", "javax.swing.plaf.basic.BasicMenuBarUI",
      "MenuItemUI", "javax.swing.plaf.basic.BasicMenuItemUI",
      "MenuUI", "javax.swing.plaf.basic.BasicMenuUI",
      "OptionPaneUI", "javax.swing.plaf.basic.BasicOptionPaneUI",
      "PanelUI", "javax.swing.plaf.basic.BasicPanelUI",
      "PasswordFieldUI", "javax.swing.plaf.basic.BasicPasswordFieldUI",
      "PopupMenuSeparatorUI", "javax.swing.plaf.basic.BasicPopupMenuSeparatorUI",
      "PopupMenuUI", "javax.swing.plaf.basic.BasicPopupMenuUI",
      "ProgressBarUI", "javax.swing.plaf.basic.BasicProgressBarUI",
      "RadioButtonMenuItemUI", "javax.swing.plaf.basic.BasicRadioButtonMenuItemUI",
      "RadioButtonUI", "javax.swing.plaf.basic.BasicRadioButtonUI",
      "RootPaneUI", "javax.swing.plaf.basic.BasicRootPaneUI",
      "ScrollBarUI", "javax.swing.plaf.basic.BasicScrollBarUI",
      "ScrollPaneUI", "javax.swing.plaf.basic.BasicScrollPaneUI",
      "SeparatorUI", "javax.swing.plaf.basic.BasicSeparatorUI",
      "SliderUI", "javax.swing.plaf.basic.BasicSliderUI",
      "SplitPaneUI", "javax.swing.plaf.basic.BasicSplitPaneUI",
      "SpinnerUI", "javax.swing.plaf.basic.BasicSpinnerUI",
      "StandardDialogUI", "javax.swing.plaf.basic.BasicStandardDialogUI",
      "TabbedPaneUI", "javax.swing.plaf.basic.BasicTabbedPaneUI",
      "TableHeaderUI", "javax.swing.plaf.basic.BasicTableHeaderUI",
      "TableUI", "javax.swing.plaf.basic.BasicTableUI",
      "TextPaneUI", "javax.swing.plaf.basic.BasicTextPaneUI",
      "TextAreaUI", "javax.swing.plaf.basic.BasicTextAreaUI",
      "TextFieldUI", "javax.swing.plaf.basic.BasicTextFieldUI",
      "TextPaneUI", "javax.swing.plaf.basic.BasicTextPaneUI",
      "ToggleButtonUI", "javax.swing.plaf.basic.BasicToggleButtonUI",
      "ToolBarSeparatorUI", "javax.swing.plaf.basic.BasicToolBarSeparatorUI",
      "ToolBarUI", "javax.swing.plaf.basic.BasicToolBarUI",
      "ToolTipUI", "javax.swing.plaf.basic.BasicToolTipUI",
      "TreeUI", "javax.swing.plaf.basic.BasicTreeUI",
      "ViewportUI", "javax.swing.plaf.basic.BasicViewportUI"
    };
    // Add Class Defaults to UI Defaults table
    defaults.putDefaults(uiDefaults);
  }

  /**
   * initSystemColorDefaults
   * @param defaults TODO
   */
  protected void initSystemColorDefaults(UIDefaults defaults)
  {
    Color highLight = new Color(249, 247, 246);
    Color light = new Color(239, 235, 231);
    Color shadow = new Color(139, 136, 134);
    Color darkShadow = new Color(16, 16, 16);

    Object[] uiDefaults;
    uiDefaults = new Object[] {
      "activeCaption", new ColorUIResource(0, 0, 128),
      "activeCaptionBorder", new ColorUIResource(Color.lightGray),
      "activeCaptionText", new ColorUIResource(Color.white),
      "control", new ColorUIResource(light),
      "controlDkShadow", new ColorUIResource(shadow),
      "controlHighlight", new ColorUIResource(highLight),
      "controlLtHighlight", new ColorUIResource(highLight),
      "controlShadow", new ColorUIResource(shadow),
      "controlText", new ColorUIResource(darkShadow),
      "desktop", new ColorUIResource(0, 92, 92),
      "inactiveCaption", new ColorUIResource(Color.gray),
      "inactiveCaptionBorder", new ColorUIResource(Color.lightGray),
      "inactiveCaptionText", new ColorUIResource(Color.lightGray),
      "info", new ColorUIResource(light),
      "infoText", new ColorUIResource(darkShadow),
      "menu", new ColorUIResource(light),
      "menuText", new ColorUIResource(darkShadow),
      "scrollbar", new ColorUIResource(light),
      "text", new ColorUIResource(Color.white),
      "textHighlight", new ColorUIResource(Color.black),
      "textHighlightText", new ColorUIResource(Color.white),
      "textInactiveText", new ColorUIResource(Color.gray),
      "textText", new ColorUIResource(Color.black),
      "window", new ColorUIResource(light),
      "windowBorder", new ColorUIResource(Color.black),
      "windowText", new ColorUIResource(darkShadow)
    };
    defaults.putDefaults(uiDefaults);
  }

  /**
   * loadSystemColors
   * @param defaults TODO
   * @param value1 TODO
   * @param value2 TODO
   */
  protected void loadSystemColors(UIDefaults defaults, String[] value1,
                                  boolean value2)
  {
    // TODO
  }

  /**
   * loadResourceBundle
   * @param defaults TODO
   */
  private void loadResourceBundle(UIDefaults defaults)
  {
    ResourceBundle bundle;
    Enumeration e;
    String key;
    String value;
    bundle = ResourceBundle.getBundle("resources/basic");
    // Process Resources
    e = bundle.getKeys();
    while (e.hasMoreElements())
      {
        key = (String) e.nextElement();
        value = bundle.getString(key);
        defaults.put(key, value);
      }
  }

  /**
   * initComponentDefaults
   * @param defaults TODO
   */
  protected void initComponentDefaults(UIDefaults defaults)
  {
    Object[] uiDefaults;
    
    Color highLight = new Color(249, 247, 246);
    Color light = new Color(239, 235, 231);
    Color shadow = new Color(139, 136, 134);
    Color darkShadow = new Color(16, 16, 16);
    
    uiDefaults = new Object[] {

      "AbstractUndoableEdit.undoText", "Undo",
      "AbstractUndoableEdit.redoText", "Redo",
      "Button.background", new ColorUIResource(light),
      "Button.border",
      new UIDefaults.LazyValue() 
      {
        public Object createValue(UIDefaults table)
        {
          return BasicBorders.getButtonBorder();
        }
      },
      "Button.darkShadow", new ColorUIResource(shadow),
      "Button.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "SPACE",  "pressed",
        "released SPACE", "released"
      }),
      "Button.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Button.foreground", new ColorUIResource(darkShadow),
      "Button.highlight", new ColorUIResource(highLight),
      "Button.light", new ColorUIResource(highLight),
      "Button.margin", new InsetsUIResource(2, 2, 2, 2),
      "Button.shadow", new ColorUIResource(shadow),
      "Button.textIconGap", new Integer(4),
      "Button.textShiftOffset", new Integer(0),
      "CheckBox.background", new ColorUIResource(light),
      "CheckBox.border", new BorderUIResource.CompoundBorderUIResource(null,
                                                                       null),
      "CheckBox.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "SPACE",  "pressed",
        "released SPACE", "released"
      }),
      "CheckBox.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "CheckBox.foreground", new ColorUIResource(darkShadow),
      "CheckBox.icon", BasicIconFactory.getCheckBoxIcon(),
      "CheckBox.margin",new InsetsUIResource(2, 2, 2, 2),
      "CheckBox.textIconGap", new Integer(4),
      "CheckBox.textShiftOffset", new Integer(0),
      "CheckBoxMenuItem.acceleratorFont", new FontUIResource("Dialog",
                                                             Font.PLAIN, 12),
      "CheckBoxMenuItem.acceleratorForeground",
      new ColorUIResource(darkShadow),
      "CheckBoxMenuItem.acceleratorSelectionForeground",
      new ColorUIResource(Color.white),
      "CheckBoxMenuItem.arrowIcon", BasicIconFactory.getMenuItemArrowIcon(),
      "CheckBoxMenuItem.background", new ColorUIResource(light),
      "CheckBoxMenuItem.border", new BasicBorders.MarginBorder(),
      "CheckBoxMenuItem.borderPainted", Boolean.FALSE,
      "CheckBoxMenuItem.checkIcon", BasicIconFactory.getCheckBoxMenuItemIcon(),
      "CheckBoxMenuItem.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "CheckBoxMenuItem.foreground", new ColorUIResource(darkShadow),
      "CheckBoxMenuItem.margin", new InsetsUIResource(2, 2, 2, 2),
      "CheckBoxMenuItem.selectionBackground", new ColorUIResource(Color.black),
      "CheckBoxMenuItem.selectionForeground", new ColorUIResource(Color.white),
      "ColorChooser.background", new ColorUIResource(light),
      "ColorChooser.cancelText", "Cancel",
      "ColorChooser.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "ColorChooser.foreground", new ColorUIResource(darkShadow),
      "ColorChooser.hsbBlueText", "B",
      "ColorChooser.hsbBrightnessText", "B",
      "ColorChooser.hsbGreenText", "G",
      "ColorChooser.hsbHueText", "H",
      "ColorChooser.hsbNameText", "HSB",
      "ColorChooser.hsbRedText", "R",
      "ColorChooser.hsbSaturationText", "S",
      "ColorChooser.okText", "OK",
      "ColorChooser.previewText", "Preview",
      "ColorChooser.resetText", "Reset",
      "ColorChooser.rgbBlueMnemonic", new Integer(66),
      "ColorChooser.rgbBlueText", "Blue",
      "ColorChooser.rgbGreenMnemonic", new Integer(71),
      "ColorChooser.rgbGreenText", "Green",
      "ColorChooser.rgbNameText", "RGB",
      "ColorChooser.rgbRedMnemonic", new Integer(82),
      "ColorChooser.rgbRedText", "Red",
      "ColorChooser.sampleText", "Sample Text  Sample Text",
      "ColorChooser.swatchesDefaultRecentColor", new ColorUIResource(light),
      "ColorChooser.swatchesNameText", "Swatches",
      "ColorChooser.swatchesRecentSwatchSize", new Dimension(10, 10),
      "ColorChooser.swatchesRecentText", "Recent:",
      "ColorChooser.swatchesSwatchSize", new Dimension(10, 10),
      "ComboBox.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ESCAPE", "hidePopup",
        "PAGE_UP", "pageUpPassThrough",
        "PAGE_DOWN", "pageDownPassThrough",
        "HOME",  "homePassThrough",
        "END",  "endPassThrough"
      }),
      "ComboBox.background", new ColorUIResource(light),
      "ComboBox.buttonBackground", new ColorUIResource(light),
      "ComboBox.buttonDarkShadow", new ColorUIResource(shadow),
      "ComboBox.buttonHighlight", new ColorUIResource(highLight),
      "ComboBox.buttonShadow", new ColorUIResource(shadow),
      "ComboBox.disabledBackground", new ColorUIResource(light),
      "ComboBox.disabledForeground", new ColorUIResource(Color.gray),
      "ComboBox.font", new FontUIResource("SansSerif", Font.PLAIN, 12),
      "ComboBox.foreground", new ColorUIResource(Color.black),
      "ComboBox.selectionBackground", new ColorUIResource(Color.black),
      "ComboBox.selectionForeground", new ColorUIResource(Color.white),
      "Desktop.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "KP_LEFT", "left",
        "KP_RIGHT", "right",
        "ctrl F5", "restore",
        "LEFT",  "left",
        "ctrl alt F6", "selectNextFrame",
        "UP",  "up",
        "ctrl F6", "selectNextFrame",
        "RIGHT", "right",
        "DOWN",  "down",
        "ctrl F7", "move",
        "ctrl F8", "resize",
        "ESCAPE", "escape",
        "ctrl TAB", "selectNextFrame",
        "ctrl F9", "minimize",
        "KP_UP", "up",
        "ctrl F4", "close",
        "KP_DOWN", "down",
        "ctrl F10", "maximize",
        "ctrl alt shift F6","selectPreviousFrame"
      }),
      "Desktop.background", new ColorUIResource(0, 92, 92),
      "DesktopIcon.border", new BorderUIResource.CompoundBorderUIResource(null,
                                                                          null),
      "EditorPane.background", new ColorUIResource(Color.white),
      "EditorPane.border", new BasicBorders.MarginBorder(),
      "EditorPane.caretBlinkRate", new Integer(500),
      "EditorPane.caretForeground", new ColorUIResource(Color.black),
      "EditorPane.font", new FontUIResource("Serif", Font.PLAIN, 12),
      "EditorPane.foreground", new ColorUIResource(Color.black),
      "EditorPane.inactiveForeground", new ColorUIResource(Color.gray),
      "EditorPane.keyBindings", new JTextComponent.KeyBinding[] {
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_UP,
                                                             0), "caret-up"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN,
                                                             0), "caret-down"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP,
                                                             0), "page-up"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN,
                                                             0), "page-down"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
                                                             0), "insert-break"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
                                                             0), "insert-tab")
          },
      "EditorPane.margin", new InsetsUIResource(3, 3, 3, 3),
      "EditorPane.selectionBackground", new ColorUIResource(Color.black),
      "EditorPane.selectionForeground", new ColorUIResource(Color.white),
      "FileChooser.acceptAllFileFilterText", "All Files (*.*)",
      "FileChooser.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ESCAPE", "cancelSelection"
      }),
      "FileChooser.cancelButtonMnemonic", new Integer(67),
      "FileChooser.cancelButtonText", "Cancel",
      "FileChooser.cancelButtonToolTipText", "Abort file chooser dialog",
      // XXX Don't use gif
      "FileChooser.detailsViewIcon", new IconUIResource(new ImageIcon("icons/DetailsView.gif")),
      "FileChooser.directoryDescriptionText", "Directory",
      "FileChooser.fileDescriptionText", "Generic File",
      "FileChooser.helpButtonMnemonic", new Integer(72),
      "FileChooser.helpButtonText", "Help",
      "FileChooser.helpButtonToolTipText", "FileChooser help",
      // XXX Don't use gif
      "FileChooser.homeFolderIcon", new IconUIResource(new ImageIcon("icons/HomeFolder.gif")),
      // XXX Don't use gif
      "FileChooser.listViewIcon", new IconUIResource(new ImageIcon("icons/ListView.gif")),
      "FileChooser.newFolderErrorSeparator", ":",
      "FileChooser.newFolderErrorText", "Error creating new folder",
      // XXX Don't use gif
      "FileChooser.newFolderIcon", new IconUIResource(new ImageIcon("icons/NewFolder.gif")),
      "FileChooser.openButtonMnemonic", new Integer(79),
      "FileChooser.openButtonText", "Open",
      "FileChooser.openButtonToolTipText", "Open selected file",
      "FileChooser.saveButtonMnemonic", new Integer(83),
      "FileChooser.saveButtonText", "Save",
      "FileChooser.saveButtonToolTipText", "Save selected file",
      // XXX Don't use gif
      "FileChooser.upFolderIcon", new IconUIResource(new ImageIcon("icons/UpFolder.gif")),
      "FileChooser.updateButtonMnemonic", new Integer(85),
      "FileChooser.updateButtonText", "Update",
      "FileChooser.updateButtonToolTipText", "Update directory listing",
      // XXX Don't use gif
      "FileView.computerIcon", new IconUIResource(new ImageIcon("icons/Computer.gif")),
      // XXX Don't use gif
      "FileView.directoryIcon", new IconUIResource(new ImageIcon("icons/Directory.gif")),
      // XXX Don't use gif
      "FileView.fileIcon", new IconUIResource(new ImageIcon("icons/File.gif")),
      // XXX Don't use gif
      "FileView.floppyDriveIcon", new IconUIResource(new ImageIcon("icons/Floppy.gif")),
      // XXX Don't use gif
      "FileView.hardDriveIcon", new IconUIResource(new ImageIcon("icons/HardDrive.gif")),
      "FocusManagerClassName", "TODO",
      "FormattedTextField.background", new ColorUIResource(light),
      "FormattedTextField.caretForeground", new ColorUIResource(Color.black),
      "FormattedTextField.foreground", new ColorUIResource(Color.black),
      "FormattedTextField.inactiveBackground", new ColorUIResource(light),
      "FormattedTextField.inactiveForeground", new ColorUIResource(Color.gray),
      "FormattedTextField.selectionBackground",
      new ColorUIResource(Color.black),
      "FormattedTextField.selectionForeground",
      new ColorUIResource(Color.white),
      "FormView.resetButtonText", "Reset",
      "FormView.submitButtonText", "Submit Query",
      "InternalFrame.activeTitleBackground", new ColorUIResource(0, 0, 128),
      "InternalFrame.activeTitleForeground", new ColorUIResource(Color.white),
      "InternalFrame.border",
      new BorderUIResource.CompoundBorderUIResource(null, null),
      "InternalFrame.borderColor", new ColorUIResource(light),
      "InternalFrame.borderDarkShadow", new ColorUIResource(shadow),
      "InternalFrame.borderHighlight", new ColorUIResource(highLight),
      "InternalFrame.borderLight", new ColorUIResource(light),
      "InternalFrame.borderShadow", new ColorUIResource(shadow),
      "InternalFrame.closeIcon", BasicIconFactory.createEmptyFrameIcon(),
      // XXX Don't use gif
      "InternalFrame.icon", new IconUIResource(new ImageIcon("icons/JavaCup.gif")),
      "InternalFrame.iconifyIcon", BasicIconFactory.createEmptyFrameIcon(),
      "InternalFrame.inactiveTitleBackground", new ColorUIResource(Color.gray),
      "InternalFrame.inactiveTitleForeground",
      new ColorUIResource(Color.lightGray),
      "InternalFrame.maximizeIcon", BasicIconFactory.createEmptyFrameIcon(),
      "InternalFrame.minimizeIcon", BasicIconFactory.createEmptyFrameIcon(),
      "InternalFrame.titleFont", new FontUIResource("Dialog", Font.PLAIN, 12),
      "InternalFrame.windowBindings", new Object[] {
        "shift ESCAPE", "showSystemMenu",
        "ctrl SPACE",  "showSystemMenu",
        "ESCAPE",  "showSystemMenu"
      },
      "Label.background", new ColorUIResource(light),
      "Label.disabledForeground", new ColorUIResource(Color.white),
      "Label.disabledShadow", new ColorUIResource(shadow),
      "Label.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Label.foreground", new ColorUIResource(darkShadow),
      "List.background", new ColorUIResource(light),
      "List.border", new BasicBorders.MarginBorder(),
      "List.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "PAGE_UP", "scrollUp",
        "ctrl \\", "clearSelection",
        "PAGE_DOWN", "scrollDown",
        "shift PAGE_DOWN","scrollDownExtendSelection",
        "END",  "selectLastRow",
        "HOME",  "selectFirstRow",
        "shift END", "selectLastRowExtendSelection",
        "shift HOME", "selectFirstRowExtendSelection",
        "UP",  "selectPreviousRow",
        "ctrl /", "selectAll",
        "ctrl A", "selectAll",
        "DOWN",  "selectNextRow",
        "shift UP", "selectPreviousRowExtendSelection",
        "ctrl SPACE", "selectNextRowExtendSelection",
        "shift DOWN", "selectNextRowExtendSelection",
        "KP_UP", "selectPreviousRow",
        "shift PAGE_UP","scrollUpExtendSelection",
        "KP_DOWN", "selectNextRow"
      }),
      "List.foreground", new ColorUIResource(darkShadow),
      "List.selectionBackground", new ColorUIResource(Color.black),
      "List.selectionForeground", new ColorUIResource(Color.white),
      "Menu.acceleratorFont", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Menu.acceleratorForeground", new ColorUIResource(darkShadow),
      "Menu.acceleratorSelectionForeground", new ColorUIResource(Color.white),
      "Menu.arrowIcon", BasicIconFactory.getMenuArrowIcon(),
      "Menu.background", new ColorUIResource(light),
      "Menu.border", new BasicBorders.MarginBorder(),
      "Menu.borderPainted", Boolean.FALSE,
      "Menu.checkIcon", BasicIconFactory.getMenuItemCheckIcon(),
      "Menu.consumesTabs", Boolean.TRUE,
      "Menu.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Menu.foreground", new ColorUIResource(darkShadow),
      "Menu.margin", new InsetsUIResource(2, 2, 2, 2),
      "Menu.selectedWindowInputMapBindings", new Object[] {
        "ESCAPE", "cancel",
        "DOWN",  "selectNext",
        "KP_DOWN", "selectNext",
        "UP",  "selectPrevious",
        "KP_UP", "selectPrevious",
        "LEFT",  "selectParent",
        "KP_LEFT", "selectParent",
        "RIGHT", "selectChild",
        "KP_RIGHT", "selectChild",
        "ENTER", "return",
        "SPACE", "return"
      },
      "Menu.selectionBackground", new ColorUIResource(Color.black),
      "Menu.selectionForeground", new ColorUIResource(Color.white),
      "MenuBar.background", new ColorUIResource(light),
      "MenuBar.border", new BasicBorders.MenuBarBorder(null, null),
      "MenuBar.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "MenuBar.foreground", new ColorUIResource(darkShadow),
      "MenuBar.highlight", new ColorUIResource(highLight),
      "MenuBar.shadow", new ColorUIResource(shadow),
      "MenuBar.windowBindings", new Object[] {
        "F10", "takeFocus"
      },
      "MenuItem.acceleratorDelimiter", "-",
      "MenuItem.acceleratorFont", new FontUIResource("Dialog", Font.PLAIN, 12),
      "MenuItem.acceleratorForeground", new ColorUIResource(darkShadow),
      "MenuItem.acceleratorSelectionForeground",
      new ColorUIResource(Color.white),
      "MenuItem.arrowIcon", BasicIconFactory.getMenuItemArrowIcon(),
      "MenuItem.background", new ColorUIResource(light),
      "MenuItem.border", new BasicBorders.MarginBorder(),
      "MenuItem.borderPainted", Boolean.FALSE,
      "MenuItem.checkIcon", BasicIconFactory.getMenuItemCheckIcon(),
      "MenuItem.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "MenuItem.foreground", new ColorUIResource(darkShadow),
      "MenuItem.margin", new InsetsUIResource(2, 2, 2, 2),
      "MenuItem.selectionBackground", new ColorUIResource(Color.black),
      "MenuItem.selectionForeground", new ColorUIResource(Color.white),
      "OptionPane.background", new ColorUIResource(light),
      "OptionPane.border",
      new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0),
      "OptionPane.buttonAreaBorder",
      new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0),
      "OptionPane.cancelButtonText", "Cancel",
      // XXX Don't use gif
      "OptionPane.errorIcon",
      new IconUIResource(new ImageIcon("icons/Error.gif")),
      "OptionPane.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "OptionPane.foreground", new ColorUIResource(darkShadow),
      // XXX Don't use gif
      "OptionPane.informationIcon",
      new IconUIResource(new ImageIcon("icons/Inform.gif")),
      "OptionPane.messageAreaBorder",
      new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0),
      "OptionPane.messageForeground", new ColorUIResource(darkShadow),
      "OptionPane.minimumSize", new DimensionUIResource(262, 90),
      "OptionPane.noButtonText", "No",
      "OptionPane.okButtonText", "OK",
      // XXX Don't use gif
      "OptionPane.questionIcon",
      new IconUIResource(new ImageIcon("icons/Question.gif")),
      // XXX Don't use gif
      "OptionPane.warningIcon",
      new IconUIResource(new ImageIcon("icons/Warn.gif")),
      "OptionPane.windowBindings", new Object[] {
        "ESCAPE",  "close"
      },
      "OptionPane.yesButtonText", "Yes",
      "Panel.background", new ColorUIResource(light),
      "Panel.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Panel.foreground", new ColorUIResource(Color.black),
      "PasswordField.background", new ColorUIResource(light),
      "PasswordField.border", new BasicBorders.FieldBorder(null, null,
                                                           null, null),
      "PasswordField.caretBlinkRate", new Integer(500),
      "PasswordField.caretForeground", new ColorUIResource(Color.black),
      "PasswordField.font", new FontUIResource("MonoSpaced", Font.PLAIN, 12),
      "PasswordField.foreground", new ColorUIResource(Color.black),
      "PasswordField.inactiveBackground", new ColorUIResource(light),
      "PasswordField.inactiveForeground", new ColorUIResource(Color.gray),
      "PasswordField.keyBindings", new JTextComponent.KeyBinding[] {
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
                                                             0),
                                      "notify-field-accept")},
      "PasswordField.margin", new InsetsUIResource(0, 0, 0, 0),
      "PasswordField.selectionBackground", new ColorUIResource(Color.black),
      "PasswordField.selectionForeground", new ColorUIResource(Color.white),
      "PopupMenu.background", new ColorUIResource(light),
      "PopupMenu.border", new BorderUIResource.BevelBorderUIResource(0),
      "PopupMenu.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "PopupMenu.foreground", new ColorUIResource(darkShadow),
      "ProgressBar.background", new ColorUIResource(light),
      "ProgressBar.border", new BorderUIResource.LineBorderUIResource(Color.darkGray),
      "ProgressBar.cellLength", new Integer(1),
      "ProgressBar.cellSpacing", new Integer(0),
      "ProgressBar.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "ProgressBar.foreground", new ColorUIResource(Color.black),
      "ProgressBar.selectionBackground", new ColorUIResource(Color.black),
      "ProgressBar.selectionForeground", new ColorUIResource(light),
      "ProgressBar.repaintInterval", new Integer(250),
      "ProgressBar.cycleTime", new Integer(6000),
      "RadioButton.background", new ColorUIResource(light),
      "RadioButton.border", new BorderUIResource.CompoundBorderUIResource(null,
                                                                          null),
      "RadioButton.darkShadow", new ColorUIResource(shadow),
      "RadioButton.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "SPACE",  "pressed",
        "released SPACE", "released"
      }),
      "RadioButton.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "RadioButton.foreground", new ColorUIResource(darkShadow),
      "RadioButton.highlight", new ColorUIResource(highLight),
      "RadioButton.icon", BasicIconFactory.getRadioButtonIcon(),
      "RadioButton.light", new ColorUIResource(highLight),
      "RadioButton.margin", new InsetsUIResource(2, 2, 2, 2),
      "RadioButton.shadow", new ColorUIResource(shadow),
      "RadioButton.textIconGap", new Integer(4),
      "RadioButton.textShiftOffset", new Integer(0),
      "RadioButtonMenuItem.acceleratorFont",
      new FontUIResource("Dialog", Font.PLAIN, 12),
      "RadioButtonMenuItem.acceleratorForeground",
      new ColorUIResource(darkShadow),
      "RadioButtonMenuItem.acceleratorSelectionForeground",
      new ColorUIResource(Color.white),
      "RadioButtonMenuItem.arrowIcon", BasicIconFactory.getMenuItemArrowIcon(),
      "RadioButtonMenuItem.background", new ColorUIResource(light),
      "RadioButtonMenuItem.border", new BasicBorders.MarginBorder(),
      "RadioButtonMenuItem.borderPainted", Boolean.FALSE,
      "RadioButtonMenuItem.checkIcon", BasicIconFactory.getRadioButtonMenuItemIcon(),
      "RadioButtonMenuItem.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "RadioButtonMenuItem.foreground", new ColorUIResource(darkShadow),
      "RadioButtonMenuItem.margin", new InsetsUIResource(2, 2, 2, 2),
      "RadioButtonMenuItem.selectionBackground",
      new ColorUIResource(Color.black),
      "RadioButtonMenuItem.selectionForeground",
      new ColorUIResource(Color.white),
      "RootPane.defaultButtonWindowKeyBindings", new Object[] {
        "ENTER",  "press",
        "released ENTER", "release",
        "ctrl ENTER",  "press",
        "ctrl released ENTER", "release"
      },
      "ScrollBar.background", new ColorUIResource(224, 224, 224),
      "ScrollBar.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "PAGE_UP", "negativeBlockIncrement",
        "PAGE_DOWN", "positiveBlockIncrement",
        "END",  "maxScroll",
        "HOME",  "minScroll",
        "LEFT",  "positiveUnitIncrement",
        "KP_UP", "negativeUnitIncrement",
        "KP_DOWN", "positiveUnitIncrement",
        "UP",  "negativeUnitIncrement",
        "RIGHT", "negativeUnitIncrement",
        "KP_LEFT", "positiveUnitIncrement",
        "DOWN",  "positiveUnitIncrement",
        "KP_RIGHT", "negativeUnitIncrement"
      }),
      "ScrollBar.foreground", new ColorUIResource(light),
      "ScrollBar.maximumThumbSize", new DimensionUIResource(4096, 4096),
      "ScrollBar.minimumThumbSize", new DimensionUIResource(8, 8),
      "ScrollBar.thumb", new ColorUIResource(light),
      "ScrollBar.thumbDarkShadow", new ColorUIResource(shadow),
      "ScrollBar.thumbHighlight", new ColorUIResource(highLight),
      "ScrollBar.thumbShadow", new ColorUIResource(shadow),
      "ScrollBar.track", new ColorUIResource(light),
      "ScrollBar.trackHighlight", new ColorUIResource(shadow),
      "ScrollPane.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "PAGE_UP", "scrollUp",
        "KP_LEFT", "unitScrollLeft",
        "ctrl PAGE_DOWN","scrollRight",
        "PAGE_DOWN", "scrollDown",
        "KP_RIGHT", "unitScrollRight",
        "LEFT",  "unitScrollLeft",
        "ctrl END", "scrollEnd",
        "UP",  "unitScrollUp",
        "RIGHT", "unitScrollRight",
        "DOWN",  "unitScrollDown",
        "ctrl HOME", "scrollHome",
        "ctrl PAGE_UP", "scrollLeft",
        "KP_UP", "unitScrollUp",
        "KP_DOWN", "unitScrollDown"
      }),
      "ScrollPane.background", new ColorUIResource(light),
      "ScrollPane.border", new BorderUIResource.EtchedBorderUIResource(),
      "ScrollPane.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "ScrollPane.foreground", new ColorUIResource(darkShadow),
      "Separator.background", new ColorUIResource(highLight),
      "Separator.foreground", new ColorUIResource(shadow),
      "Separator.highlight", new ColorUIResource(highLight),
      "Separator.shadow", new ColorUIResource(shadow),
      "Slider.background", new ColorUIResource(light),
      "Slider.focus", new ColorUIResource(shadow),
      "Slider.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "PAGE_UP", "positiveBlockIncrement",
        "PAGE_DOWN", "negativeBlockIncrement",
        "END",  "maxScroll",
        "HOME",  "minScroll",
        "LEFT",  "negativeUnitIncrement",
        "KP_UP", "positiveUnitIncrement",
        "KP_DOWN", "negativeUnitIncrement",
        "UP",  "positiveUnitIncrement",
        "RIGHT", "positiveUnitIncrement",
        "KP_LEFT", "negativeUnitIncrement",
        "DOWN",  "negativeUnitIncrement",
        "KP_RIGHT", "positiveUnitIncrement"
      }),
      "Slider.focusInsets", new InsetsUIResource(2, 2, 2, 2),
      "Slider.foreground", new ColorUIResource(light),
      "Slider.highlight", new ColorUIResource(highLight),
      "Slider.shadow", new ColorUIResource(shadow),
      "Slider.thumbHeight", new Integer(20),
      "Slider.thumbWidth", new Integer(10),
      "Slider.tickHeight", new Integer(12),
      "Spinner.background", new ColorUIResource(light),
      "Spinner.foreground", new ColorUIResource(light),
      "SplitPane.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "F6",  "toggleFocus",
        "F8",  "startResize",
        "END",  "selectMax",
        "HOME",  "selectMin",
        "LEFT",  "negativeIncremnent",
        "KP_UP", "negativeIncrement",
        "KP_DOWN", "positiveIncrement",
        "UP",  "negativeIncrement",
        "RIGHT", "positiveIncrement",
        "KP_LEFT", "negativeIncrement",
        "DOWN",  "positiveIncrement",
        "KP_RIGHT", "positiveIncrement"
      }),
      "SplitPane.background", new ColorUIResource(light),
      "SplitPane.border", new BasicBorders.SplitPaneBorder(null, null),
      "SplitPane.darkShadow", new ColorUIResource(shadow),
      "SplitPane.dividerSize", new Integer(10),
      "SplitPane.highlight", new ColorUIResource(highLight),
      "SplitPane.shadow", new ColorUIResource(shadow),
      "TabbedPane.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ctrl PAGE_DOWN","navigatePageDown",
        "ctrl PAGE_UP", "navigatePageUp",
        "ctrl UP", "requestFocus",
        "ctrl KP_UP", "requestFocus"
      }),
      "TabbedPane.background", new ColorUIResource(light),
      "TabbedPane.contentBorderInsets", new InsetsUIResource(2, 2, 3, 3),
      "TabbedPane.darkShadow", new ColorUIResource(shadow),
      "TabbedPane.focus", new ColorUIResource(darkShadow),
      "TabbedPane.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "LEFT",  "navigateLeft",
        "KP_UP", "navigateUp",
        "ctrl DOWN", "requestFocusForVisibleComponent",
        "UP", "navigateUp",
        "KP_DOWN", "navigateDown",
        "RIGHT", "navigateRight",
        "KP_LEFT", "navigateLeft",
        "ctrl KP_DOWN", "requestFocusForVisibleComponent",
        "KP_RIGHT", "navigateRight",
        "DOWN",  "navigateDown"
      }),
      "TabbedPane.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "TabbedPane.foreground", new ColorUIResource(darkShadow),
      "TabbedPane.highlight", new ColorUIResource(highLight),
      "TabbedPane.light", new ColorUIResource(highLight),
      "TabbedPane.selectedTabPadInsets", new InsetsUIResource(2, 2, 2, 1),
      "TabbedPane.shadow", new ColorUIResource(shadow),
      "TabbedPane.tabbedPaneTabAreaInsets", new InsetsUIResource(3, 2, 1, 2),
      "TabbedPane.tabbedPaneTabInsets", new InsetsUIResource(1, 4, 1, 4),
      "TabbedPane.tabbedPaneContentBorderInsets", new InsetsUIResource(3, 2, 1, 2),
      "TabbedPane.tabbedPaneTabPadInsets", new InsetsUIResource(1, 1, 1, 1),
      "TabbedPane.tabRunOverlay", new Integer(2),
      "TabbedPane.textIconGap", new Integer(4),
      "Table.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "shift PAGE_DOWN","scrollDownExtendSelection",
        "PAGE_DOWN", "scrollDownChangeSelection",
        "END",  "selectLastColumn",
        "shift END", "selectLastColumnExtendSelection",
        "HOME",  "selectFirstColumn",
        "ctrl END", "selectLastRow",
        "ctrl shift END","selectLastRowExtendSelection",
        "LEFT",  "selectPreviousColumn",
        "shift HOME", "selectFirstColumnExtendSelection",
        "UP",  "selectPreviousRow",
        "RIGHT", "selectNextColumn",
        "ctrl HOME", "selectFirstRow",
        "shift LEFT", "selectPreviousColumnExtendSelection",
        "DOWN",  "selectNextRow",
        "ctrl shift HOME","selectFirstRowExtendSelection",
        "shift UP", "selectPreviousRowExtendSelection",
        "F2",  "startEditing",
        "shift RIGHT", "selectNextColumnExtendSelection",
        "TAB",  "selectNextColumnCell",
        "shift DOWN", "selectNextRowExtendSelection",
        "ENTER", "selectNextRowCell",
        "KP_UP", "selectPreviousRow",
        "KP_DOWN", "selectNextRow",
        "KP_LEFT", "selectPreviousColumn",
        "KP_RIGHT", "selectNextColumn",
        "shift TAB", "selectPreviousColumnCell",
        "ctrl A", "selectAll",
        "shift ENTER", "selectPreviousRowCell",
        "shift KP_DOWN", "selectNextRowExtendSelection",
        "shift KP_LEFT", "selectPreviousColumnExtendSelection",
        "ESCAPE",  "cancel",
        "ctrl shift PAGE_UP", "scrollRightExtendSelection",
        "shift KP_RIGHT", " selectNextColumnExtendSelection",
        "ctrl PAGE_UP",  "scrollLeftChangeSelection",
        "shift PAGE_UP", "scrollUpExtendSelection",
        "ctrl shift PAGE_DOWN", "scrollLeftExtendSelection",
        "ctrl PAGE_DOWN", "scrollRightChangeSelection",
        "PAGE_UP",   "scrollUpChangeSelection"
      }),
      "Table.background", new ColorUIResource(light),
      "Table.focusCellBackground", new ColorUIResource(light),
      "Table.focusCellForeground", new ColorUIResource(darkShadow),
      "Table.focusCellHighlightBorder",
      new BorderUIResource.LineBorderUIResource(Color.white),
      "Table.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Table.foreground", new ColorUIResource(darkShadow),
      "Table.gridColor", new ColorUIResource(Color.gray),
      "Table.scrollPaneBorder", new BorderUIResource.BevelBorderUIResource(0),
      "Table.selectionBackground", new ColorUIResource(Color.black),
      "Table.selectionForeground", new ColorUIResource(Color.white),
      "TableHeader.background", new ColorUIResource(light),
      "TableHeader.cellBorder", new BorderUIResource.BevelBorderUIResource(0),
      "TableHeader.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "TableHeader.foreground", new ColorUIResource(darkShadow),
      "TextArea.background", new ColorUIResource(light),
      "TextArea.border", new BasicBorders.MarginBorder(),
      "TextArea.caretBlinkRate", new Integer(500),
      "TextArea.caretForeground", new ColorUIResource(Color.black),
      "TextArea.font", new FontUIResource("MonoSpaced", Font.PLAIN, 12),
      "TextArea.foreground", new ColorUIResource(Color.black),
      "TextArea.inactiveForeground", new ColorUIResource(Color.gray),
      "TextArea.keyBindings", new JTextComponent.KeyBinding[] {
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_UP,
                                                             0), "caret-up"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN,
                                                             0), "caret-down"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP,
                                                             0), "page-up"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN,
                                                             0), "page-down"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
                                                             0), "insert-break"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
                                                             0), "insert-tab")
          },
      "TextArea.margin", new InsetsUIResource(0, 0, 0, 0),
      "TextArea.selectionBackground", new ColorUIResource(Color.black),
      "TextArea.selectionForeground", new ColorUIResource(Color.white),
      "TextField.background", new ColorUIResource(light),
      "TextField.border", new BasicBorders.FieldBorder(null, null, null, null),
      "TextField.caretBlinkRate", new Integer(500),
      "TextField.caretForeground", new ColorUIResource(Color.black),
      "TextField.darkShadow", new ColorUIResource(shadow),
      "TextField.font", new FontUIResource("SansSerif", Font.PLAIN, 12),
      "TextField.foreground", new ColorUIResource(Color.black),
      "TextField.highlight", new ColorUIResource(highLight),
      "TextField.inactiveBackground", new ColorUIResource(light),
      "TextField.inactiveForeground", new ColorUIResource(Color.gray),
      "TextField.light", new ColorUIResource(highLight),
      "TextField.highlight", new ColorUIResource(light),
      "TextField.keyBindings", new JTextComponent.KeyBinding[] {
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
                                                             0),
                                      "notify-field-accept"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT,
							     InputEvent.SHIFT_DOWN_MASK),
							     "selection-backward"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT,
							     InputEvent.SHIFT_DOWN_MASK),
							     "selection-forward"),
          },
      "TextField.margin", new InsetsUIResource(0, 0, 0, 0),
      "TextField.selectionBackground", new ColorUIResource(Color.black),
      "TextField.selectionForeground", new ColorUIResource(Color.white),
      "TextPane.background", new ColorUIResource(Color.white),
      "TextPane.border", new BasicBorders.MarginBorder(),
      "TextPane.caretBlinkRate", new Integer(500),
      "TextPane.caretForeground", new ColorUIResource(Color.black),
      "TextPane.font", new FontUIResource("Serif", Font.PLAIN, 12),
      "TextPane.foreground", new ColorUIResource(Color.black),
      "TextPane.inactiveForeground", new ColorUIResource(Color.gray),
      "TextPane.keyBindings", new JTextComponent.KeyBinding[] {
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_UP,
                                                             0), "caret-up"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN,
                                                             0), "caret-down"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP,
                                                             0), "page-up"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN,
                                                             0), "page-down"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
                                                             0), "insert-break"),
        new JTextComponent.KeyBinding(KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
                                                             0), "insert-tab")
          },
      "TextPane.margin", new InsetsUIResource(3, 3, 3, 3),
      "TextPane.selectionBackground", new ColorUIResource(Color.black),
      "TextPane.selectionForeground", new ColorUIResource(Color.white),
      "TitledBorder.border", new BorderUIResource.EtchedBorderUIResource(),
      "TitledBorder.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "TitledBorder.titleColor", new ColorUIResource(darkShadow),
      "ToggleButton.background", new ColorUIResource(light),
      "ToggleButton.border",
      new BorderUIResource.CompoundBorderUIResource(null, null),
      "ToggleButton.darkShadow", new ColorUIResource(shadow),
      "ToggleButton.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "SPACE",  "pressed",
        "released SPACE", "released"
      }),
      "ToggleButton.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "ToggleButton.foreground", new ColorUIResource(darkShadow),
      "ToggleButton.highlight", new ColorUIResource(highLight),
      "ToggleButton.light", new ColorUIResource(light),
      "ToggleButton.margin", new InsetsUIResource(2, 14, 2, 14),
      "ToggleButton.shadow", new ColorUIResource(shadow),
      "ToggleButton.textIconGap", new Integer(4),
      "ToggleButton.textShiftOffset", new Integer(0),
      "ToolBar.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "UP",  "navigateUp",
        "KP_UP", "navigateUp",
        "DOWN",  "navigateDown",
        "KP_DOWN", "navigateDown",
        "LEFT",  "navigateLeft",
        "KP_LEFT", "navigateLeft",
        "RIGHT", "navigateRight",
        "KP_RIGHT", "navigateRight"
      }),
      "ToolBar.background", new ColorUIResource(light),
      "ToolBar.border", new BorderUIResource.EtchedBorderUIResource(),
      "ToolBar.darkShadow", new ColorUIResource(shadow),
      "ToolBar.dockingBackground", new ColorUIResource(light),
      "ToolBar.dockingForeground", new ColorUIResource(Color.red),
      "ToolBar.floatingBackground", new ColorUIResource(light),
      "ToolBar.floatingForeground", new ColorUIResource(Color.darkGray),
      "ToolBar.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "ToolBar.foreground", new ColorUIResource(darkShadow),
      "ToolBar.highlight", new ColorUIResource(highLight),
      "ToolBar.light", new ColorUIResource(highLight),
      "ToolBar.separatorSize", new DimensionUIResource(20, 20),
      "ToolBar.shadow", new ColorUIResource(shadow),
      "ToolTip.background", new ColorUIResource(light),
      "ToolTip.border", new BorderUIResource.LineBorderUIResource(Color.lightGray),
      "ToolTip.font", new FontUIResource("SansSerif", Font.PLAIN, 12),
      "ToolTip.foreground", new ColorUIResource(darkShadow),
      "Tree.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ESCAPE", "cancel"
      }),
      "Tree.background", new ColorUIResource(light),
      "Tree.changeSelectionWithFocus", Boolean.TRUE,
      "Tree.closedIcon", new IconUIResource(new ImageIcon("icons/TreeClosed.png")),
      "Tree.collapsedIcon", new IconUIResource(new ImageIcon("icons/TreeCollapsed.png")),
      "Tree.drawsFocusBorderAroundIcon", Boolean.FALSE,
      "Tree.editorBorder", new BorderUIResource.LineBorderUIResource(Color.lightGray),
      "Tree.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "shift PAGE_DOWN", "scrollDownExtendSelection",
        "PAGE_DOWN", "scrollDownChangeSelection",
        "END",  "selectLast",
        "ctrl KP_UP", "selectPreviousChangeLead",
        "shift END", "selectLastExtendSelection",
        "HOME",  "selectFirst",
        "ctrl END", "selectLastChangeLead",
        "ctrl /", "selectAll",
        "LEFT",  "selectParent",
        "shift HOME", "selectFirstExtendSelection",
        "UP",  "selectPrevious",
        "ctrl KP_DOWN", "selectNextChangeLead",
        "RIGHT", "selectChild",
        "ctrl HOME", "selectFirstChangeLead",
        "DOWN",  "selectNext",
        "ctrl KP_LEFT", "scrollLeft",
        "shift UP", "selectPreviousExtendSelection",
        "F2",  "startEditing",
        "ctrl LEFT", "scrollLeft",
        "ctrl KP_RIGHT","scrollRight",
        "ctrl UP", "selectPreviousChangeLead",
        "shift DOWN", "selectNextExtendSelection",
        "ENTER", "toggle",
        "KP_UP", "selectPrevious",
        "KP_DOWN", "selectNext",
        "ctrl RIGHT", "scrollRight",
        "KP_LEFT", "selectParent",
        "KP_RIGHT", "selectChild",
        "ctrl DOWN", "selectNextChangeLead",
        "ctrl A", "selectAll",
        "shift KP_UP", "selectPreviousExtendSelection",
        "shift KP_DOWN","selectNextExtendSelection",
        "ctrl SPACE", "toggleSelectionPreserveAnchor",
        "ctrl shift PAGE_UP", "scrollUpExtendSelection",
        "ctrl \\", "clearSelection",
        "shift SPACE", "extendSelection",
        "ctrl PAGE_UP", "scrollUpChangeLead",
        "shift PAGE_UP","scrollUpExtendSelection",
        "SPACE", "toggleSelectionPreserveAnchor",
        "ctrl shift PAGE_DOWN", "scrollDownExtendSelection",
        "PAGE_UP",  "scrollUpChangeSelection",
        "ctrl PAGE_DOWN", "scrollDownChangeLead"
      }),
      "Tree.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Tree.expandedIcon", new IconUIResource(new ImageIcon("icons/TreeExpanded.png")),
      "Tree.foreground", new ColorUIResource(Color.black),
      "Tree.hash", new ColorUIResource(Color.gray),
      "Tree.leafIcon", new IconUIResource(new ImageIcon("icons/TreeLeaf.png")),
      "Tree.leftChildIndent", new Integer(7),
      "Tree.openIcon", new IconUIResource(new ImageIcon("icons/TreeOpen.png")),
      "Tree.rightChildIndent", new Integer(13),
      "Tree.rowHeight", new Integer(16),
      "Tree.scrollsOnExpand", Boolean.TRUE,
      "Tree.selectionBackground", new ColorUIResource(Color.black),
      "Tree.selectionBorderColor", new ColorUIResource(Color.black),
      "Tree.selectionForeground", new ColorUIResource(Color.white),
      "Tree.textBackground", new ColorUIResource(Color.white),
      "Tree.textForeground", new ColorUIResource(Color.black),
      "Viewport.background", new ColorUIResource(light),
      "Viewport.foreground", new ColorUIResource(Color.black),
      "Viewport.font", new FontUIResource("Dialog", Font.PLAIN, 12)
    };
    defaults.putDefaults(uiDefaults);
  }
} // class BasicLookAndFeel
