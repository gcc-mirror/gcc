/* BasicLookAndFeel.java --
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.SystemColor;
import java.awt.Toolkit;
import java.awt.event.AWTEventListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.ResourceBundle;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.MenuSelectionManager;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.InsetsUIResource;

/**
 * A basic implementation of Swing's Look and Feel framework. This can serve
 * as a base for custom look and feel implementations.
 *
 * @author Andrew Selkirk
 */
public abstract class BasicLookAndFeel extends LookAndFeel
  implements Serializable
{

  /**
   * Helps closing menu popups when the user clicks outside of any menu area.
   * This is implemented as an AWTEventListener that listens on the event
   * queue directly, grabs all mouse events from there and finds out of they
   * are targetted at a menu/submenu/menubar or not. If not,
   * the MenuSelectionManager is messaged to close the currently opened menus,
   * if any.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class PopupHelper implements AWTEventListener
  {

    /**
     * Receives an event from the event queue.
     *
     * @param event
     */
    public void eventDispatched(AWTEvent event)
    {
      if (event instanceof MouseEvent)
        {
          MouseEvent mouseEvent = (MouseEvent) event;
          if (mouseEvent.getID() == MouseEvent.MOUSE_PRESSED)
            mousePressed(mouseEvent);
        }
    }

    /**
     * Handles mouse pressed events from the event queue.
     *
     * @param ev the mouse pressed event
     */
    private void mousePressed(MouseEvent ev)
    {
      // Autoclose all menus managed by the MenuSelectionManager.
      MenuSelectionManager m = MenuSelectionManager.defaultManager();
      Component target = ev.getComponent();
      if (target instanceof Container)
        target = ((Container) target).findComponentAt(ev.getPoint());
      if (m.getSelectedPath().length > 0
          && ! m.isComponentPartOfCurrentMenu(target)
          && (((JComponent)target).getClientProperty(DONT_CANCEL_POPUP) == null
          || !((JComponent)target).getClientProperty(DONT_CANCEL_POPUP).equals(Boolean.TRUE)))
        {
          m.clearSelectedPath();
        }
    }

  }

  /**
   * An action that can play an audio file.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class AudioAction extends AbstractAction
  {
    /**
     * The UIDefaults key that specifies the sound.
     */
    Object key;

    /**
     * Creates a new AudioAction.
     *
     * @param key the key that describes the audio action, normally a filename
     *        of an audio file relative to the current package
     */
    AudioAction(Object key)
    {
      this.key = key;
    }

    /**
     * Plays the sound represented by this action.
     *
     * @param event the action event that triggers this audio action
     */
    public void actionPerformed(ActionEvent event)
    {
      // We only can handle strings for now.
      if (key instanceof String)
        {
          String name = UIManager.getString(key);
          InputStream stream = getClass().getResourceAsStream(name);
          try
            {
              Clip clip = AudioSystem.getClip();
              AudioInputStream audioStream =
                AudioSystem.getAudioInputStream(stream);
              clip.open(audioStream);
            }
          catch (LineUnavailableException ex)
            {
              // Nothing we can do about it.
            }
          catch (IOException ex)
            {
              // Nothing we can do about it.
            }
          catch (UnsupportedAudioFileException e)
            {
              // Nothing we can do about it.
            }
        }
    }
  }

  static final long serialVersionUID = -6096995660290287879L;

  /**
   * This is a key for a client property that tells the PopupHelper that
   * it shouldn't close popups when the mouse event target has this
   * property set. This is used when the component handles popup closing
   * itself.
   */
  static final String DONT_CANCEL_POPUP = "noCancelPopup";

  /**
   * Helps closing menu popups when user clicks outside of the menu area.
   */
  private transient PopupHelper popupHelper;

  /**
   * Maps the audio actions for this l&f.
   */
  private ActionMap audioActionMap;

  /**
   * Creates a new instance of the Basic look and feel.
   */
  public BasicLookAndFeel()
  {
    // Nothing to do here.
  }

  /**
   * Creates and returns a new instance of the default resources for this look
   * and feel.
   *
   * @return The UI defaults.
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
   * Populates the <code>defaults</code> table with mappings between class IDs
   * and fully qualified class names for the UI delegates.
   *
   * @param defaults  the defaults table (<code>null</code> not permitted).
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
      "FileChooserUI", "javax.swing.plaf.basic.BasicFileChooserUI",
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
   * Populates the <code>defaults</code> table with system color defaults.
   *
   * This sets up a couple of default values and passes them to
   * {@link #loadSystemColors(UIDefaults, String[], boolean)}. If the
   * look and feel is a native look and feel, these defaults may be overridden
   * by the corresponding SystemColor constants.
   *
   * @param defaults  the defaults table (<code>null</code> not permitted).
   */
  protected void initSystemColorDefaults(UIDefaults defaults)
  {
    String[] defaultColors = new String[] {
      "activeCaption", "#000080",
      "activeCaptionBorder", "#C0C0C0",
      "activeCaptionText", "#FFFFFF",
      "control", "#C0C0C0",
      "controlDkShadow", "#000000",
      "controlHighlight", "#C0C0C0",
      "controlLtHighlight", "#FFFFFF",
      "controlShadow", "#808080",
      "controlText", "#000000",
      "desktop", "#005C5C",
      "inactiveCaption", "#808080",
      "inactiveCaptionBorder", "#C0C0C0",
      "inactiveCaptionText", "#C0C0C0",
      "info", "#FFFFE1",
      "infoText", "#000000",
      "menu", "#C0C0C0",
      "menuText", "#000000",
      "scrollbar", "#E0E0E0",
      "text", "#C0C0C0",
      "textHighlight", "#000080",
      "textHighlightText", "#FFFFFF",
      "textInactiveText", "#808080",
      "textText", "#000000",
      "window", "#FFFFFF",
      "windowBorder", "#000000",
      "windowText", "#000000"
    };
    loadSystemColors(defaults, defaultColors, isNativeLookAndFeel());
  }

  /**
   * Populates the <code>defaults</code> table with the system colors. If
   * <code>useNative</code> is <code>true</code>, the table is populated
   * with the constants in {@link SystemColor}, otherwise the
   * <code>systemColors</code> parameter is decoded into the defaults table.
   * The system colors array is made up of pairs, where the first entry is the
   * name of the system color, and the second entry is a string denoting
   * an RGB color value like &quot;#C0C0C0&quot;, which is decoded using
   * {@link Color#decode(String)}.
   *
   * @param defaults  the defaults table (<code>null</code> not permitted).
   * @param systemColors defaults to use when <code>useNative</code> is
   *        <code>false</code>
   * @param useNative when <code>true</code>, installs the values of the
   *        SystemColor constants, when <code>false</code>, install the values
   *        from <code>systemColors</code>
   */
  protected void loadSystemColors(UIDefaults defaults, String[] systemColors,
                                  boolean useNative)
  {
    if (useNative)
      {
        defaults.put("activeCaption",
                     new ColorUIResource(SystemColor.ACTIVE_CAPTION));
        defaults.put("activeCaptionBorder",
                     new ColorUIResource(SystemColor.ACTIVE_CAPTION_BORDER));
        defaults.put("activeCaptionText",
                     new ColorUIResource(SystemColor.ACTIVE_CAPTION_TEXT));
        defaults.put("control",
                     new ColorUIResource(SystemColor.CONTROL));
        defaults.put("controlDkShadow",
                     new ColorUIResource(SystemColor.CONTROL_DK_SHADOW));
        defaults.put("controlHighlight",
                     new ColorUIResource(SystemColor.CONTROL_HIGHLIGHT));
        defaults.put("controlLtHighlight",
                     new ColorUIResource(SystemColor.CONTROL_LT_HIGHLIGHT));
        defaults.put("controlShadow",
                     new ColorUIResource(SystemColor.CONTROL_SHADOW));
        defaults.put("controlText",
                     new ColorUIResource(SystemColor.CONTROL_TEXT));
        defaults.put("desktop",
                     new ColorUIResource(SystemColor.DESKTOP));
        defaults.put("inactiveCaption",
                     new ColorUIResource(SystemColor.INACTIVE_CAPTION));
        defaults.put("inactiveCaptionBorder",
                     new ColorUIResource(SystemColor.INACTIVE_CAPTION_BORDER));
        defaults.put("inactiveCaptionText",
                     new ColorUIResource(SystemColor.INACTIVE_CAPTION_TEXT));
        defaults.put("info",
                     new ColorUIResource(SystemColor.INFO));
        defaults.put("infoText",
                     new ColorUIResource(SystemColor.INFO_TEXT));
        defaults.put("menu",
                     new ColorUIResource(SystemColor.MENU));
        defaults.put("menuText",
                     new ColorUIResource(SystemColor.MENU_TEXT));
        defaults.put("scrollbar",
                     new ColorUIResource(SystemColor.SCROLLBAR));
        defaults.put("text",
                     new ColorUIResource(SystemColor.TEXT));
        defaults.put("textHighlight",
                     new ColorUIResource(SystemColor.TEXT_HIGHLIGHT));
        defaults.put("textHighlightText",
                     new ColorUIResource(SystemColor.TEXT_HIGHLIGHT_TEXT));
        defaults.put("textInactiveText",
                     new ColorUIResource(SystemColor.TEXT_INACTIVE_TEXT));
        defaults.put("textText",
                     new ColorUIResource(SystemColor.TEXT_TEXT));
        defaults.put("window",
                     new ColorUIResource(SystemColor.WINDOW));
        defaults.put("windowBorder",
                     new ColorUIResource(SystemColor.WINDOW_BORDER));
        defaults.put("windowText",
                     new ColorUIResource(SystemColor.WINDOW_TEXT));
      }
    else
      {
        for (int i = 0; i < systemColors.length; i += 2)
          {
            Color color = Color.BLACK;
            try
              {
                color = Color.decode(systemColors[i + 1]);
              }
            catch (NumberFormatException e)
              {
                e.printStackTrace();
              }
            defaults.put(systemColors[i], new ColorUIResource(color));
          }
      }
  }

  /**
   * Loads the resource bundle in 'resources/basic' and adds the contained
   * key/value pairs to the <code>defaults</code> table.
   *
   * @param defaults the UI defaults to load the resources into
   */
  // FIXME: This method is not used atm and private and thus could be removed.
  // However, I consider this method useful for providing localized
  // descriptions and similar stuff and therefore think that we should use it
  // instead and provide the resource bundles.
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
   * Populates the <code>defaults</code> table with UI default values for
   * colors, fonts, keybindings and much more.
   *
   * @param defaults  the defaults table (<code>null</code> not permitted).
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
      "Button.background", new ColorUIResource(Color.LIGHT_GRAY),
      "Button.border",
      new UIDefaults.LazyValue()
      {
        public Object createValue(UIDefaults table)
        {
          return BasicBorders.getButtonBorder();
        }
      },
      "Button.darkShadow", new ColorUIResource(Color.BLACK),
      "Button.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Button.foreground", new ColorUIResource(Color.BLACK),
      "Button.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
          KeyStroke.getKeyStroke("SPACE"), "pressed",
          KeyStroke.getKeyStroke("released SPACE"), "released"
      }),
      "Button.highlight", new ColorUIResource(Color.WHITE),
      "Button.light", new ColorUIResource(Color.LIGHT_GRAY),
      "Button.margin", new InsetsUIResource(2, 14, 2, 14),
      "Button.shadow", new ColorUIResource(Color.GRAY),
      "Button.textIconGap", new Integer(4),
      "Button.textShiftOffset", new Integer(0),
      "CheckBox.background", new ColorUIResource(new Color(204, 204, 204)),
      "CheckBox.border", new BorderUIResource.CompoundBorderUIResource(null,
                                                                       null),
      "CheckBox.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
          KeyStroke.getKeyStroke("SPACE"), "pressed",
          KeyStroke.getKeyStroke("released SPACE"), "released"
      }),
      "CheckBox.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "CheckBox.foreground", new ColorUIResource(darkShadow),
      "CheckBox.icon",
      new UIDefaults.LazyValue()
      {
        public Object createValue(UIDefaults def)
        {
          return BasicIconFactory.getCheckBoxIcon();
        }
      },
      "CheckBox.checkIcon",
      new UIDefaults.LazyValue()
      {
        public Object createValue(UIDefaults def)
        {
          return BasicIconFactory.getMenuItemCheckIcon();
        }
      },
      "CheckBox.margin", new InsetsUIResource(2, 2, 2, 2),
      "CheckBox.textIconGap", new Integer(4),
      "CheckBox.textShiftOffset", new Integer(0),
      "CheckBoxMenuItem.acceleratorFont", new FontUIResource("Dialog",
                                                             Font.PLAIN, 12),
      "CheckBoxMenuItem.acceleratorForeground",
      new ColorUIResource(new Color(16, 16, 16)),
      "CheckBoxMenuItem.acceleratorSelectionForeground",
      new ColorUIResource(Color.white),
      "CheckBoxMenuItem.arrowIcon", BasicIconFactory.getMenuItemArrowIcon(),
      "CheckBoxMenuItem.background", new ColorUIResource(light),
      "CheckBoxMenuItem.border", new BasicBorders.MarginBorder(),
      "CheckBoxMenuItem.borderPainted", Boolean.FALSE,
      "CheckBoxMenuItem.checkIcon",
      new UIDefaults.LazyValue()
      {
        public Object createValue(UIDefaults def)
        {
          return BasicIconFactory.getCheckBoxMenuItemIcon();
        }
      },
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
      "ColorChooser.rgbBlueMnemonic", "66",
      "ColorChooser.rgbBlueText", "Blue",
      "ColorChooser.rgbGreenMnemonic", "78",
      "ColorChooser.rgbGreenText", "Green",
      "ColorChooser.rgbNameText", "RGB",
      "ColorChooser.rgbRedMnemonic", "68",
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
      "ComboBox.background", new ColorUIResource(Color.white),
      "ComboBox.buttonBackground", new ColorUIResource(light),
      "ComboBox.buttonDarkShadow", new ColorUIResource(darkShadow),
      "ComboBox.buttonHighlight", new ColorUIResource(highLight),
      "ComboBox.buttonShadow", new ColorUIResource(shadow),
      "ComboBox.disabledBackground", new ColorUIResource(light),
      "ComboBox.disabledForeground", new ColorUIResource(Color.gray),
      "ComboBox.font", new FontUIResource("SansSerif", Font.PLAIN, 12),
      "ComboBox.foreground", new ColorUIResource(Color.black),
      "ComboBox.selectionBackground", new ColorUIResource(0, 0, 128),
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
        "ctrl alt shift F6", "selectPreviousFrame"
      }),
      "DesktopIcon.border", new BorderUIResource.CompoundBorderUIResource(null,
                                                                          null),
      "EditorPane.background", new ColorUIResource(Color.white),
      "EditorPane.border", BasicBorders.getMarginBorder(),
      "EditorPane.caretBlinkRate", new Integer(500),
      "EditorPane.caretForeground", new ColorUIResource(Color.black),
      "EditorPane.font", new FontUIResource("Serif", Font.PLAIN, 12),
      "EditorPane.foreground", new ColorUIResource(Color.black),
      "EditorPane.inactiveForeground", new ColorUIResource(Color.gray),
      "EditorPane.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
                KeyStroke.getKeyStroke("shift UP"), "selection-up",
                KeyStroke.getKeyStroke("ctrl RIGHT"), "caret-next-word",
                KeyStroke.getKeyStroke("shift ctrl LEFT"), "selection-previous-word",
                KeyStroke.getKeyStroke("shift KP_UP"), "selection-up",
                KeyStroke.getKeyStroke("DOWN"), "caret-down",
                KeyStroke.getKeyStroke("shift ctrl T"), "previous-link-action",
                KeyStroke.getKeyStroke("ctrl LEFT"), "caret-previous-word",
                KeyStroke.getKeyStroke("CUT"), "cut-to-clipboard",
                KeyStroke.getKeyStroke("END"), "caret-end-line",
                KeyStroke.getKeyStroke("shift PAGE_UP"), "selection-page-up",
                KeyStroke.getKeyStroke("KP_UP"), "caret-up",
                KeyStroke.getKeyStroke("DELETE"), "delete-next",
                KeyStroke.getKeyStroke("ctrl HOME"), "caret-begin",
                KeyStroke.getKeyStroke("shift LEFT"), "selection-backward",
                KeyStroke.getKeyStroke("ctrl END"), "caret-end",
                KeyStroke.getKeyStroke("BACK_SPACE"), "delete-previous",
                KeyStroke.getKeyStroke("shift ctrl RIGHT"), "selection-next-word",
                KeyStroke.getKeyStroke("LEFT"), "caret-backward",
                KeyStroke.getKeyStroke("KP_LEFT"), "caret-backward",
                KeyStroke.getKeyStroke("shift KP_RIGHT"), "selection-forward",
                KeyStroke.getKeyStroke("ctrl SPACE"), "activate-link-action",
                KeyStroke.getKeyStroke("ctrl H"), "delete-previous",
                KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "unselect",
                KeyStroke.getKeyStroke("ENTER"), "insert-break",
                KeyStroke.getKeyStroke("shift HOME"), "selection-begin-line",
                KeyStroke.getKeyStroke("RIGHT"), "caret-forward",
                KeyStroke.getKeyStroke("shift ctrl PAGE_UP"), "selection-page-left",
                KeyStroke.getKeyStroke("shift DOWN"), "selection-down",
                KeyStroke.getKeyStroke("PAGE_DOWN"), "page-down",
                KeyStroke.getKeyStroke("shift KP_LEFT"), "selection-backward",
                KeyStroke.getKeyStroke("shift ctrl O"), "toggle-componentOrientation",
                KeyStroke.getKeyStroke("ctrl X"), "cut-to-clipboard",
                KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN"), "selection-page-right",
                KeyStroke.getKeyStroke("ctrl C"), "copy-to-clipboard",
                KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "caret-next-word",
                KeyStroke.getKeyStroke("shift END"), "selection-end-line",
                KeyStroke.getKeyStroke("ctrl KP_LEFT"), "caret-previous-word",
                KeyStroke.getKeyStroke("HOME"), "caret-begin-line",
                KeyStroke.getKeyStroke("ctrl V"), "paste-from-clipboard",
                KeyStroke.getKeyStroke("KP_DOWN"), "caret-down",
                KeyStroke.getKeyStroke("ctrl A"), "select-all",
                KeyStroke.getKeyStroke("shift RIGHT"), "selection-forward",
                KeyStroke.getKeyStroke("shift ctrl END"), "selection-end",
                KeyStroke.getKeyStroke("COPY"), "copy-to-clipboard",
                KeyStroke.getKeyStroke("shift ctrl KP_LEFT"), "selection-previous-word",
                KeyStroke.getKeyStroke("ctrl T"), "next-link-action",
                KeyStroke.getKeyStroke("shift KP_DOWN"), "selection-down",
                KeyStroke.getKeyStroke("TAB"), "insert-tab",
                KeyStroke.getKeyStroke("UP"), "caret-up",
                KeyStroke.getKeyStroke("shift ctrl HOME"), "selection-begin",
                KeyStroke.getKeyStroke("shift PAGE_DOWN"), "selection-page-down",
                KeyStroke.getKeyStroke("KP_RIGHT"), "caret-forward",
                KeyStroke.getKeyStroke("shift ctrl KP_RIGHT"), "selection-next-word",
                KeyStroke.getKeyStroke("PAGE_UP"), "page-up",
                KeyStroke.getKeyStroke("PASTE"), "paste-from-clipboard"
          }),
      "EditorPane.margin", new InsetsUIResource(3, 3, 3, 3),
      "EditorPane.selectionBackground", new ColorUIResource(Color.black),
      "EditorPane.selectionForeground", new ColorUIResource(Color.white),
      "FileChooser.acceptAllFileFilterText", "All Files (*.*)",
      "FileChooser.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ESCAPE", "cancelSelection"
      }),
      "FileChooser.cancelButtonMnemonic", "67",
      "FileChooser.cancelButtonText", "Cancel",
      "FileChooser.cancelButtonToolTipText", "Abort file chooser dialog",
      "FileChooser.directoryDescriptionText", "Directory",
      "FileChooser.fileDescriptionText", "Generic File",
      "FileChooser.directoryOpenButtonMnemonic", "79",
      "FileChooser.helpButtonMnemonic", "72",
      "FileChooser.helpButtonText", "Help",
      "FileChooser.helpButtonToolTipText", "FileChooser help",
      "FileChooser.newFolderErrorSeparator", ":",
      "FileChooser.newFolderErrorText", "Error creating new folder",
      "FileChooser.openButtonMnemonic", "79",
      "FileChooser.openButtonText", "Open",
      "FileChooser.openButtonToolTipText", "Open selected file",
      "FileChooser.saveButtonMnemonic", "83",
      "FileChooser.saveButtonText", "Save",
      "FileChooser.saveButtonToolTipText", "Save selected file",
      "FileChooser.updateButtonMnemonic", "85",
      "FileChooser.updateButtonText", "Update",
      "FileChooser.updateButtonToolTipText", "Update directory listing",
      "FocusManagerClassName", "TODO",
      "FormattedTextField.background", new ColorUIResource(light),
      "FormattedTextField.caretForeground", new ColorUIResource(Color.black),
      "FormattedTextField.margin", new InsetsUIResource(0, 0, 0, 0),
      "FormattedTextField.caretBlinkRate", new Integer(500),
      "FormattedTextField.font",
      new FontUIResource("SansSerif", Font.PLAIN, 12),
      "FormattedTextField.foreground", new ColorUIResource(Color.black),
      "FormattedTextField.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        KeyStroke.getKeyStroke("KP_UP"), "increment",
        KeyStroke.getKeyStroke("END"), "caret-end-line",
        KeyStroke.getKeyStroke("shift ctrl  O"), "toggle-componentOrientation",
        KeyStroke.getKeyStroke("shift KP_LEFT"), "selection-backward",
        KeyStroke.getKeyStroke("shift RIGHT"), "selection-forward",
        KeyStroke.getKeyStroke("KP_DOWN"), "decrement",
        KeyStroke.getKeyStroke("HOME"), "caret-begin-line",
        KeyStroke.getKeyStroke("ctrl V"), "paste-from-clipboard",
        KeyStroke.getKeyStroke("ctrl H"), "delete-previous",
        KeyStroke.getKeyStroke("KP_LEFT"), "caret-backward",
        KeyStroke.getKeyStroke("LEFT"), "caret-backward",
        KeyStroke.getKeyStroke("ctrl X"), "cut-to-clipboard",
        KeyStroke.getKeyStroke("KP_RIGHT"), "caret-forward",
        KeyStroke.getKeyStroke("UP"), "increment",
        KeyStroke.getKeyStroke("shift ctrl KP_RIGHT"), "selection-next-word",
        KeyStroke.getKeyStroke("COPY"), "copy-to-clipboard",
        KeyStroke.getKeyStroke("shift HOME"), "selection-begin-line",
        KeyStroke.getKeyStroke("ESCAPE"), "reset-field-edit",
        KeyStroke.getKeyStroke("RIGHT"), "caret-forward",
        KeyStroke.getKeyStroke("shift ctrl LEFT"), "selection-previous-word",
        KeyStroke.getKeyStroke("ctrl KP_LEFT"), "caret-previous-word",
        KeyStroke.getKeyStroke("DOWN"), "decrement",
        KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "caret-next-word",
        KeyStroke.getKeyStroke("PASTE"), "paste-from-clipboard",
        KeyStroke.getKeyStroke("shift ctrl RIGHT"), "selection-next-word",
        KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "unselect",
        KeyStroke.getKeyStroke("ctrl A"), "select-all",
        KeyStroke.getKeyStroke("shift KP_RIGHT"), "selection-forward",
        KeyStroke.getKeyStroke("CUT"), "cut-to-clipboard",
        KeyStroke.getKeyStroke("ctrl LEFT"), "caret-previous-word",
        KeyStroke.getKeyStroke("BACK_SPACE"), "delete-previous",
        KeyStroke.getKeyStroke("shift ctrl KP_LEFT"), "selection-previous-word",
        KeyStroke.getKeyStroke("ctrl C"), "copy-to-clipboard",
        KeyStroke.getKeyStroke("shift END"), "selection-end-line",
        KeyStroke.getKeyStroke("ctrl RIGHT"), "caret-next-word",
        KeyStroke.getKeyStroke("DELETE"), "delete-next",
        KeyStroke.getKeyStroke("ENTER"), "notify-field-accept",
        KeyStroke.getKeyStroke("shift LEFT"), "selection-backward"
      }),
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
      new UIDefaults.LazyValue()
      {
        public Object createValue(UIDefaults table)
        {
          Color lineColor = new Color(238, 238, 238);
          Border inner = BorderFactory.createLineBorder(lineColor, 1);
          Color shadowInner = new Color(184, 207, 229);
          Color shadowOuter = new Color(122, 138, 153);
          Border outer = BorderFactory.createBevelBorder(BevelBorder.RAISED,
                                                         Color.WHITE,
                                                         Color.WHITE,
                                                         shadowOuter,
                                                         shadowInner);
          Border border = new BorderUIResource.CompoundBorderUIResource(outer,
                                                                        inner);
          return border;
        }
      },
      "InternalFrame.borderColor", new ColorUIResource(light),
      "InternalFrame.borderDarkShadow", new ColorUIResource(Color.BLACK),
      "InternalFrame.borderHighlight", new ColorUIResource(Color.WHITE),
      "InternalFrame.borderLight", new ColorUIResource(Color.LIGHT_GRAY),
      "InternalFrame.borderShadow", new ColorUIResource(Color.GRAY),
      "InternalFrame.closeIcon", BasicIconFactory.createEmptyFrameIcon(),
      "InternalFrame.icon",
      new UIDefaults.LazyValue()
      {
        public Object createValue(UIDefaults def)
        {
          return new IconUIResource(BasicIconFactory.createEmptyFrameIcon());
        }
      },
      "InternalFrame.iconifyIcon", BasicIconFactory.createEmptyFrameIcon(),
      "InternalFrame.inactiveTitleBackground", new ColorUIResource(Color.gray),
      "InternalFrame.inactiveTitleForeground",
      new ColorUIResource(Color.lightGray),
      "InternalFrame.maximizeIcon", BasicIconFactory.createEmptyFrameIcon(),
      "InternalFrame.minimizeIcon", BasicIconFactory.createEmptyFrameIcon(),
      "InternalFrame.titleFont", new FontUIResource("Dialog", Font.BOLD, 12),
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
      "List.background", new ColorUIResource(Color.white),
      "List.border", new BasicBorders.MarginBorder(),
      "List.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
            KeyStroke.getKeyStroke("ctrl DOWN"), "selectNextRowChangeLead",
            KeyStroke.getKeyStroke("shift UP"), "selectPreviousRowExtendSelection",
            KeyStroke.getKeyStroke("ctrl RIGHT"), "selectNextColumnChangeLead",
            KeyStroke.getKeyStroke("shift ctrl LEFT"), "selectPreviousColumnExtendSelection",
            KeyStroke.getKeyStroke("shift KP_UP"), "selectPreviousRowExtendSelection",
            KeyStroke.getKeyStroke("DOWN"), "selectNextRow",
            KeyStroke.getKeyStroke("ctrl UP"), "selectPreviousRowChangeLead",
            KeyStroke.getKeyStroke("ctrl LEFT"), "selectPreviousColumnChangeLead",
            KeyStroke.getKeyStroke("CUT"), "cut",
            KeyStroke.getKeyStroke("END"), "selectLastRow",
            KeyStroke.getKeyStroke("shift PAGE_UP"), "scrollUpExtendSelection",
            KeyStroke.getKeyStroke("KP_UP"), "selectPreviousRow",
            KeyStroke.getKeyStroke("shift ctrl UP"), "selectPreviousRowExtendSelection",
            KeyStroke.getKeyStroke("ctrl HOME"), "selectFirstRowChangeLead",
            KeyStroke.getKeyStroke("shift LEFT"), "selectPreviousColumnExtendSelection",
            KeyStroke.getKeyStroke("ctrl END"), "selectLastRowChangeLead",
            KeyStroke.getKeyStroke("ctrl PAGE_DOWN"), "scrollDownChangeLead",
            KeyStroke.getKeyStroke("shift ctrl RIGHT"), "selectNextColumnExtendSelection",
            KeyStroke.getKeyStroke("LEFT"), "selectPreviousColumn",
            KeyStroke.getKeyStroke("ctrl PAGE_UP"), "scrollUpChangeLead",
            KeyStroke.getKeyStroke("KP_LEFT"), "selectPreviousColumn",
            KeyStroke.getKeyStroke("shift KP_RIGHT"), "selectNextColumnExtendSelection",
            KeyStroke.getKeyStroke("SPACE"), "addToSelection",
            KeyStroke.getKeyStroke("ctrl SPACE"), "toggleAndAnchor",
            KeyStroke.getKeyStroke("shift SPACE"), "extendTo",
            KeyStroke.getKeyStroke("shift ctrl SPACE"), "moveSelectionTo",
            KeyStroke.getKeyStroke("shift ctrl DOWN"), "selectNextRowExtendSelection",
            KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "clearSelection",
            KeyStroke.getKeyStroke("shift HOME"), "selectFirstRowExtendSelection",
            KeyStroke.getKeyStroke("RIGHT"), "selectNextColumn",
            KeyStroke.getKeyStroke("shift ctrl PAGE_UP"), "scrollUpExtendSelection",
            KeyStroke.getKeyStroke("shift DOWN"), "selectNextRowExtendSelection",
            KeyStroke.getKeyStroke("PAGE_DOWN"), "scrollDown",
            KeyStroke.getKeyStroke("shift ctrl KP_UP"), "selectPreviousRowExtendSelection",
            KeyStroke.getKeyStroke("shift KP_LEFT"), "selectPreviousColumnExtendSelection",
            KeyStroke.getKeyStroke("ctrl X"), "cut",
            KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN"), "scrollDownExtendSelection",
            KeyStroke.getKeyStroke("ctrl SLASH"), "selectAll",
            KeyStroke.getKeyStroke("ctrl C"), "copy",
            KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "selectNextColumnChangeLead",
            KeyStroke.getKeyStroke("shift END"), "selectLastRowExtendSelection",
            KeyStroke.getKeyStroke("shift ctrl KP_DOWN"), "selectNextRowExtendSelection",
            KeyStroke.getKeyStroke("ctrl KP_LEFT"), "selectPreviousColumnChangeLead",
            KeyStroke.getKeyStroke("HOME"), "selectFirstRow",
            KeyStroke.getKeyStroke("ctrl V"), "paste",
            KeyStroke.getKeyStroke("KP_DOWN"), "selectNextRow",
            KeyStroke.getKeyStroke("ctrl KP_DOWN"), "selectNextRowChangeLead",
            KeyStroke.getKeyStroke("shift RIGHT"), "selectNextColumnExtendSelection",
            KeyStroke.getKeyStroke("ctrl A"), "selectAll",
            KeyStroke.getKeyStroke("shift ctrl END"), "selectLastRowExtendSelection",
            KeyStroke.getKeyStroke("COPY"), "copy",
            KeyStroke.getKeyStroke("ctrl KP_UP"), "selectPreviousRowChangeLead",
            KeyStroke.getKeyStroke("shift ctrl KP_LEFT"), "selectPreviousColumnExtendSelection",
            KeyStroke.getKeyStroke("shift KP_DOWN"), "selectNextRowExtendSelection",
            KeyStroke.getKeyStroke("UP"), "selectPreviousRow",
            KeyStroke.getKeyStroke("shift ctrl HOME"), "selectFirstRowExtendSelection",
            KeyStroke.getKeyStroke("shift PAGE_DOWN"), "scrollDownExtendSelection",
            KeyStroke.getKeyStroke("KP_RIGHT"), "selectNextColumn",
            KeyStroke.getKeyStroke("shift ctrl KP_RIGHT"), "selectNextColumnExtendSelection",
            KeyStroke.getKeyStroke("PAGE_UP"), "scrollUp",
            KeyStroke.getKeyStroke("PASTE"), "paste"
      }),
      "List.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "List.foreground", new ColorUIResource(Color.black),
      "List.selectionBackground", new ColorUIResource(0, 0, 128),
      "List.selectionForeground", new ColorUIResource(Color.white),
      "List.focusCellHighlightBorder",
      new BorderUIResource.
      LineBorderUIResource(new ColorUIResource(Color.yellow)),
      "Menu.acceleratorFont", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Menu.crossMenuMnemonic", Boolean.TRUE,
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
      "Menu.menuPopupOffsetX", new Integer(0),
      "Menu.menuPopupOffsetY", new Integer(0),
      "Menu.submenuPopupOffsetX", new Integer(0),
      "Menu.submenuPopupOffsetY", new Integer(0),
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
      "MenuItem.acceleratorDelimiter", "+",
      "MenuItem.acceleratorFont", new FontUIResource("Dialog", Font.PLAIN, 12),
      "MenuItem.acceleratorForeground", new ColorUIResource(darkShadow),
      "MenuItem.acceleratorSelectionForeground",
      new ColorUIResource(Color.white),
      "MenuItem.arrowIcon", BasicIconFactory.getMenuItemArrowIcon(),
      "MenuItem.background", new ColorUIResource(light),
      "MenuItem.border", new BasicBorders.MarginBorder(),
      "MenuItem.borderPainted", Boolean.FALSE,
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
      "OptionPane.buttonClickThreshhold", new Integer(500),
      "OptionPane.cancelButtonText", "Cancel",
      "OptionPane.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "OptionPane.foreground", new ColorUIResource(darkShadow),
      "OptionPane.messageAreaBorder",
      new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0),
      "OptionPane.messageForeground", new ColorUIResource(darkShadow),
      "OptionPane.minimumSize",
      new DimensionUIResource(BasicOptionPaneUI.MinimumWidth,
                              BasicOptionPaneUI.MinimumHeight),
      "OptionPane.noButtonText", "No",
      "OptionPane.okButtonText", "OK",
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
      "PasswordField.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
                      KeyStroke.getKeyStroke("END"), "caret-end-line",
                      KeyStroke.getKeyStroke("shift ctrl O"), "toggle-componentOrientation",
                      KeyStroke.getKeyStroke("shift KP_LEFT"), "selection-backward",
                      KeyStroke.getKeyStroke("shift RIGHT"), "selection-forward",
                      KeyStroke.getKeyStroke("HOME"), "caret-begin-line",
                      KeyStroke.getKeyStroke("ctrl V"), "paste-from-clipboard",
                      KeyStroke.getKeyStroke("ctrl H"), "delete-previous",
                      KeyStroke.getKeyStroke("KP_LEFT"), "caret-backward",
                      KeyStroke.getKeyStroke("LEFT"), "caret-backward",
                      KeyStroke.getKeyStroke("ctrl X"), "cut-to-clipboard",
                      KeyStroke.getKeyStroke("KP_RIGHT"), "caret-forward",
                      KeyStroke.getKeyStroke("shift ctrl KP_RIGHT"), "selection-end-line",
                      KeyStroke.getKeyStroke("COPY"), "copy-to-clipboard",
                      KeyStroke.getKeyStroke("shift HOME"), "selection-begin-line",
                      KeyStroke.getKeyStroke("RIGHT"), "caret-forward",
                      KeyStroke.getKeyStroke("shift ctrl LEFT"), "selection-begin-line",
                      KeyStroke.getKeyStroke("ctrl KP_LEFT"), "caret-begin-line",
                      KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "caret-end-line",
                      KeyStroke.getKeyStroke("PASTE"), "paste-from-clipboard",
                      KeyStroke.getKeyStroke("shift ctrl RIGHT"), "selection-end-line",
                      KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "unselect",
                      KeyStroke.getKeyStroke("ctrl A"), "select-all",
                      KeyStroke.getKeyStroke("shift KP_RIGHT"), "selection-forward",
                      KeyStroke.getKeyStroke("CUT"), "cut-to-clipboard",
                      KeyStroke.getKeyStroke("ctrl LEFT"), "caret-begin-line",
                      KeyStroke.getKeyStroke("BACK_SPACE"), "delete-previous",
                      KeyStroke.getKeyStroke("shift ctrl KP_LEFT"), "selection-begin-line",
                      KeyStroke.getKeyStroke("ctrl C"), "copy-to-clipboard",
                      KeyStroke.getKeyStroke("shift END"), "selection-end-line",
                      KeyStroke.getKeyStroke("ctrl RIGHT"), "caret-end-line",
                      KeyStroke.getKeyStroke("DELETE"), "delete-next",
                      KeyStroke.getKeyStroke("ENTER"), "notify-field-accept",
                      KeyStroke.getKeyStroke("shift LEFT"), "selection-backward"
                            }),
      "PasswordField.margin", new InsetsUIResource(0, 0, 0, 0),
      "PasswordField.selectionBackground", new ColorUIResource(Color.black),
      "PasswordField.selectionForeground", new ColorUIResource(Color.white),
      "PopupMenu.background", new ColorUIResource(light),
      "PopupMenu.border", new BorderUIResource.BevelBorderUIResource(0),
      "PopupMenu.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "PopupMenu.foreground", new ColorUIResource(darkShadow),
      "PopupMenu.selectedWindowInputMapBindings",
      new Object[] {"ESCAPE", "cancel",
                    "DOWN", "selectNext",
                    "KP_DOWN", "selectNext",
                    "UP", "selectPrevious",
                    "KP_UP", "selectPrevious",
                    "LEFT", "selectParent",
                    "KP_LEFT", "selectParent",
                    "RIGHT", "selectChild",
                    "KP_RIGHT", "selectChild",
                    "ENTER", "return",
                    "SPACE", "return"
      },
      "PopupMenu.selectedWindowInputMapBindings.RightToLeft",
      new Object[] {"LEFT", "selectChild",
                    "KP_LEFT", "selectChild",
                    "RIGHT", "selectParent",
                    "KP_RIGHT", "selectParent",
      },
      "ProgressBar.background", new ColorUIResource(Color.LIGHT_GRAY),
      "ProgressBar.border",
      new BorderUIResource.LineBorderUIResource(Color.GREEN, 2),
      "ProgressBar.cellLength", new Integer(1),
      "ProgressBar.cellSpacing", new Integer(0),
      "ProgressBar.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "ProgressBar.foreground", new ColorUIResource(0, 0, 128),
      "ProgressBar.selectionBackground", new ColorUIResource(0, 0, 128),
      "ProgressBar.selectionForeground", new ColorUIResource(Color.LIGHT_GRAY),
      "ProgressBar.repaintInterval", new Integer(50),
      "ProgressBar.cycleTime", new Integer(3000),
      "RadioButton.background", new ColorUIResource(light),
      "RadioButton.border", BasicBorders.getRadioButtonBorder(),
      "RadioButton.darkShadow", new ColorUIResource(shadow),
      "RadioButton.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
        KeyStroke.getKeyStroke("SPACE"),  "pressed",
        KeyStroke.getKeyStroke("released SPACE"), "released"
      }),
      "RadioButton.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "RadioButton.foreground", new ColorUIResource(darkShadow),
      "RadioButton.highlight", new ColorUIResource(highLight),
      "RadioButton.icon",
      new UIDefaults.LazyValue()
      {
        public Object createValue(UIDefaults def)
        {
          return BasicIconFactory.getRadioButtonIcon();
        }
      },
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
        "LEFT",  "negativeUnitIncrement",
        "KP_UP", "negativeUnitIncrement",
        "KP_DOWN", "positiveUnitIncrement",
        "UP",  "negativeUnitIncrement",
        "RIGHT", "positiveUnitIncrement",
        "KP_LEFT", "negativeUnitIncrement",
        "DOWN",  "positiveUnitIncrement",
        "KP_RIGHT", "positiveUnitIncrement"
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
      "ScrollBar.width", new Integer(16),
      "ScrollPane.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "PAGE_UP", "scrollUp",
        "KP_LEFT", "unitScrollLeft",
        "ctrl PAGE_DOWN", "scrollRight",
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
            "ctrl PAGE_DOWN", "negativeBlockIncrement",
            "PAGE_DOWN", "negativeBlockIncrement",
            "PAGE_UP", "positiveBlockIncrement",
            "ctrl PAGE_UP", "positiveBlockIncrement",
            "KP_RIGHT", "positiveUnitIncrement",
            "DOWN", "negativeUnitIncrement",
            "KP_LEFT", "negativeUnitIncrement",
            "RIGHT", "positiveUnitIncrement",
            "KP_DOWN", "negativeUnitIncrement",
            "UP", "positiveUnitIncrement",
            "KP_UP", "positiveUnitIncrement",
            "LEFT", "negativeUnitIncrement",
            "HOME", "minScroll",
            "END", "maxScroll"
      }),
      "Slider.focusInsets", new InsetsUIResource(2, 2, 2, 2),
      "Slider.foreground", new ColorUIResource(light),
      "Slider.highlight", new ColorUIResource(highLight),
      "Slider.shadow", new ColorUIResource(shadow),
      "Slider.thumbHeight", new Integer(20),
      "Slider.thumbWidth", new Integer(11),
      "Slider.tickHeight", new Integer(12),
      "Slider.horizontalSize", new Dimension(200, 21),
      "Slider.verticalSize", new Dimension(21, 200),
      "Slider.minimumHorizontalSize", new Dimension(36, 21),
      "Slider.minimumVerticalSize", new Dimension(21, 36),
      "Spinner.background", new ColorUIResource(light),
      "Spinner.foreground", new ColorUIResource(light),
      "Spinner.arrowButtonSize", new DimensionUIResource(16, 5),
      "Spinner.editorBorderPainted", Boolean.FALSE,
      "Spinner.font", new FontUIResource("MonoSpaced", Font.PLAIN, 12),
      "SplitPane.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "F6",  "toggleFocus",
        "F8",  "startResize",
        "END",  "selectMax",
        "HOME",  "selectMin",
        "LEFT",  "negativeIncrement",
        "KP_UP", "negativeIncrement",
        "KP_DOWN", "positiveIncrement",
        "UP",  "negativeIncrement",
        "RIGHT", "positiveIncrement",
        "KP_LEFT", "negativeIncrement",
        "DOWN",  "positiveIncrement",
        "KP_RIGHT", "positiveIncrement",
        "shift ctrl pressed TAB", "focusOutBackward",
        "ctrl pressed TAB", "focusOutForward"
      }),
      "SplitPane.background", new ColorUIResource(light),
      "SplitPane.border", new BasicBorders.SplitPaneBorder(null, null),
      "SplitPane.darkShadow", new ColorUIResource(shadow),
      "SplitPane.dividerSize", new Integer(7),
      "SplitPane.highlight", new ColorUIResource(highLight),
      "SplitPane.shadow", new ColorUIResource(shadow),
      "SplitPaneDivider.border", BasicBorders.getSplitPaneDividerBorder(),
      "SplitPaneDivider.draggingColor", new ColorUIResource(Color.DARK_GRAY),
      "TabbedPane.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ctrl PAGE_DOWN", "navigatePageDown",
        "ctrl PAGE_UP", "navigatePageUp",
        "ctrl UP", "requestFocus",
        "ctrl KP_UP", "requestFocus"
      }),
      "TabbedPane.background", new ColorUIResource(192, 192, 192),
      "TabbedPane.contentBorderInsets", new InsetsUIResource(2, 2, 3, 3),
      "TabbedPane.darkShadow", new ColorUIResource(Color.black),
      "TabbedPane.focus", new ColorUIResource(Color.black),
      "TabbedPane.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
            KeyStroke.getKeyStroke("ctrl DOWN"), "requestFocusForVisibleComponent",
            KeyStroke.getKeyStroke("KP_UP"), "navigateUp",
            KeyStroke.getKeyStroke("LEFT"), "navigateLeft",
            KeyStroke.getKeyStroke("ctrl KP_DOWN"), "requestFocusForVisibleComponent",
            KeyStroke.getKeyStroke("UP"), "navigateUp",
            KeyStroke.getKeyStroke("KP_DOWN"), "navigateDown",
            KeyStroke.getKeyStroke("KP_LEFT"), "navigateLeft",
            KeyStroke.getKeyStroke("RIGHT"), "navigateRight",
            KeyStroke.getKeyStroke("KP_RIGHT"), "navigateRight",
            KeyStroke.getKeyStroke("DOWN"), "navigateDown"
      }),
      "TabbedPane.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "TabbedPane.foreground", new ColorUIResource(Color.black),
      "TabbedPane.highlight", new ColorUIResource(Color.white),
      "TabbedPane.light", new ColorUIResource(192, 192, 192),
      "TabbedPane.selectedTabPadInsets", new InsetsUIResource(2, 2, 2, 1),
      "TabbedPane.shadow", new ColorUIResource(128, 128, 128),
      "TabbedPane.tabsOpaque", Boolean.TRUE,
      "TabbedPane.tabAreaInsets", new InsetsUIResource(3, 2, 0, 2),
      "TabbedPane.tabInsets", new InsetsUIResource(0, 4, 1, 4),
      "TabbedPane.tabRunOverlay", new Integer(2),
      "TabbedPane.tabsOverlapBorder", Boolean.FALSE,
      "TabbedPane.textIconGap", new Integer(4),
      "Table.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ctrl DOWN", "selectNextRowChangeLead",
        "ctrl RIGHT", "selectNextColumnChangeLead",
        "ctrl UP", "selectPreviousRowChangeLead",
        "ctrl LEFT", "selectPreviousColumnChangeLead",
        "CUT", "cut",
        "SPACE", "addToSelection",
        "ctrl SPACE", "toggleAndAnchor",
        "shift SPACE", "extendTo",
        "shift ctrl SPACE", "moveSelectionTo",
        "ctrl X", "cut",
        "ctrl C", "copy",
        "ctrl KP_RIGHT", "selectNextColumnChangeLead",
        "ctrl KP_LEFT", "selectPreviousColumnChangeLead",
        "ctrl V", "paste",
        "ctrl KP_DOWN", "selectNextRowChangeLead",
        "COPY", "copy",
        "ctrl KP_UP", "selectPreviousRowChangeLead",
        "PASTE", "paste",
        "shift PAGE_DOWN", "scrollDownExtendSelection",
        "PAGE_DOWN", "scrollDownChangeSelection",
        "END",  "selectLastColumn",
        "shift END", "selectLastColumnExtendSelection",
        "HOME",  "selectFirstColumn",
        "ctrl END", "selectLastRow",
        "ctrl shift END", "selectLastRowExtendSelection",
        "LEFT",  "selectPreviousColumn",
        "shift HOME", "selectFirstColumnExtendSelection",
        "UP",  "selectPreviousRow",
        "RIGHT", "selectNextColumn",
        "ctrl HOME", "selectFirstRow",
        "shift LEFT", "selectPreviousColumnExtendSelection",
        "DOWN",  "selectNextRow",
        "ctrl shift HOME", "selectFirstRowExtendSelection",
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
        "ctrl shift PAGE_UP", "scrollLeftExtendSelection",
        "shift KP_RIGHT", "selectNextColumnExtendSelection",
        "ctrl PAGE_UP",  "scrollLeftChangeSelection",
        "shift PAGE_UP", "scrollUpExtendSelection",
        "ctrl shift PAGE_DOWN", "scrollRightExtendSelection",
        "ctrl PAGE_DOWN", "scrollRightChangeSelection",
        "PAGE_UP",   "scrollUpChangeSelection",
        "ctrl shift LEFT", "selectPreviousColumnExtendSelection",
        "shift KP_UP", "selectPreviousRowExtendSelection",
        "ctrl shift UP", "selectPreviousRowExtendSelection",
        "ctrl shift RIGHT", "selectNextColumnExtendSelection",
        "ctrl shift KP_RIGHT", "selectNextColumnExtendSelection",
        "ctrl shift DOWN", "selectNextRowExtendSelection",
        "ctrl BACK_SLASH", "clearSelection",
        "ctrl shift KP_UP", "selectPreviousRowExtendSelection",
        "ctrl shift KP_LEFT", "selectPreviousColumnExtendSelection",
        "ctrl SLASH", "selectAll",
        "ctrl shift KP_DOWN", "selectNextRowExtendSelection",
      }),
      "Table.background", new ColorUIResource(new ColorUIResource(255, 255, 255)),
      "Table.focusCellBackground", new ColorUIResource(new ColorUIResource(255, 255, 255)),
      "Table.focusCellForeground", new ColorUIResource(new ColorUIResource(0, 0, 0)),
      "Table.focusCellHighlightBorder",
      new BorderUIResource.LineBorderUIResource(
                                             new ColorUIResource(255, 255, 0)),
      "Table.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Table.foreground", new ColorUIResource(new ColorUIResource(0, 0, 0)),
      "Table.gridColor", new ColorUIResource(new ColorUIResource(128, 128, 128)),
      "Table.scrollPaneBorder", new BorderUIResource.BevelBorderUIResource(0),
      "Table.selectionBackground", new ColorUIResource(new ColorUIResource(0, 0, 128)),
      "Table.selectionForeground", new ColorUIResource(new ColorUIResource(255, 255, 255)),
      "TableHeader.background", new ColorUIResource(new ColorUIResource(192, 192, 192)),
      "TableHeader.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "TableHeader.foreground", new ColorUIResource(new ColorUIResource(0, 0, 0)),

      "TextArea.background", new ColorUIResource(light),
      "TextArea.border", new BorderUIResource(BasicBorders.getMarginBorder()),
      "TextArea.caretBlinkRate", new Integer(500),
      "TextArea.caretForeground", new ColorUIResource(Color.black),
      "TextArea.font", new FontUIResource("MonoSpaced", Font.PLAIN, 12),
      "TextArea.foreground", new ColorUIResource(Color.black),
      "TextArea.inactiveForeground", new ColorUIResource(Color.gray),
      "TextArea.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
         KeyStroke.getKeyStroke("shift UP"), "selection-up",
         KeyStroke.getKeyStroke("ctrl RIGHT"), "caret-next-word",
         KeyStroke.getKeyStroke("shift ctrl LEFT"), "selection-previous-word",
         KeyStroke.getKeyStroke("shift KP_UP"), "selection-up",
         KeyStroke.getKeyStroke("DOWN"), "caret-down",
         KeyStroke.getKeyStroke("shift ctrl T"), "previous-link-action",
         KeyStroke.getKeyStroke("ctrl LEFT"), "caret-previous-word",
         KeyStroke.getKeyStroke("CUT"), "cut-to-clipboard",
         KeyStroke.getKeyStroke("END"), "caret-end-line",
         KeyStroke.getKeyStroke("shift PAGE_UP"), "selection-page-up",
         KeyStroke.getKeyStroke("KP_UP"), "caret-up",
         KeyStroke.getKeyStroke("DELETE"), "delete-next",
         KeyStroke.getKeyStroke("ctrl HOME"), "caret-begin",
         KeyStroke.getKeyStroke("shift LEFT"), "selection-backward",
         KeyStroke.getKeyStroke("ctrl END"), "caret-end",
         KeyStroke.getKeyStroke("BACK_SPACE"), "delete-previous",
         KeyStroke.getKeyStroke("shift ctrl RIGHT"), "selection-next-word",
         KeyStroke.getKeyStroke("LEFT"), "caret-backward",
         KeyStroke.getKeyStroke("KP_LEFT"), "caret-backward",
         KeyStroke.getKeyStroke("shift KP_RIGHT"), "selection-forward",
         KeyStroke.getKeyStroke("ctrl SPACE"), "activate-link-action",
         KeyStroke.getKeyStroke("ctrl H"), "delete-previous",
         KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "unselect",
         KeyStroke.getKeyStroke("ENTER"), "insert-break",
         KeyStroke.getKeyStroke("shift HOME"), "selection-begin-line",
         KeyStroke.getKeyStroke("RIGHT"), "caret-forward",
         KeyStroke.getKeyStroke("shift ctrl PAGE_UP"), "selection-page-left",
         KeyStroke.getKeyStroke("shift DOWN"), "selection-down",
         KeyStroke.getKeyStroke("PAGE_DOWN"), "page-down",
         KeyStroke.getKeyStroke("shift KP_LEFT"), "selection-backward",
         KeyStroke.getKeyStroke("shift ctrl O"), "toggle-componentOrientation",
         KeyStroke.getKeyStroke("ctrl X"), "cut-to-clipboard",
         KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN"), "selection-page-right",
         KeyStroke.getKeyStroke("ctrl C"), "copy-to-clipboard",
         KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "caret-next-word",
         KeyStroke.getKeyStroke("shift END"), "selection-end-line",
         KeyStroke.getKeyStroke("ctrl KP_LEFT"), "caret-previous-word",
         KeyStroke.getKeyStroke("HOME"), "caret-begin-line",
         KeyStroke.getKeyStroke("ctrl V"), "paste-from-clipboard",
         KeyStroke.getKeyStroke("KP_DOWN"), "caret-down",
         KeyStroke.getKeyStroke("ctrl A"), "select-all",
         KeyStroke.getKeyStroke("shift RIGHT"), "selection-forward",
         KeyStroke.getKeyStroke("shift ctrl END"), "selection-end",
         KeyStroke.getKeyStroke("COPY"), "copy-to-clipboard",
         KeyStroke.getKeyStroke("shift ctrl KP_LEFT"), "selection-previous-word",
         KeyStroke.getKeyStroke("ctrl T"), "next-link-action",
         KeyStroke.getKeyStroke("shift KP_DOWN"), "selection-down",
         KeyStroke.getKeyStroke("TAB"), "insert-tab",
         KeyStroke.getKeyStroke("UP"), "caret-up",
         KeyStroke.getKeyStroke("shift ctrl HOME"), "selection-begin",
         KeyStroke.getKeyStroke("shift PAGE_DOWN"), "selection-page-down",
         KeyStroke.getKeyStroke("KP_RIGHT"), "caret-forward",
         KeyStroke.getKeyStroke("shift ctrl KP_RIGHT"), "selection-next-word",
         KeyStroke.getKeyStroke("PAGE_UP"), "page-up",
         KeyStroke.getKeyStroke("PASTE"), "paste-from-clipboard"
      }),
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
      "TextField.inactiveBackground", new ColorUIResource(Color.LIGHT_GRAY),
      "TextField.inactiveForeground", new ColorUIResource(Color.GRAY),
      "TextField.light", new ColorUIResource(highLight),
      "TextField.highlight", new ColorUIResource(light),
      "TextField.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
         KeyStroke.getKeyStroke("ENTER"), "notify-field-accept",
         KeyStroke.getKeyStroke("LEFT"), "caret-backward",
         KeyStroke.getKeyStroke("RIGHT"), "caret-forward",
         KeyStroke.getKeyStroke("BACK_SPACE"), "delete-previous",
         KeyStroke.getKeyStroke("ctrl X"), "cut-to-clipboard",
         KeyStroke.getKeyStroke("ctrl C"), "copy-to-clipboard",
         KeyStroke.getKeyStroke("ctrl V"), "paste-from-clipboard",
         KeyStroke.getKeyStroke("shift LEFT"), "selection-backward",
         KeyStroke.getKeyStroke("shift RIGHT"), "selection-forward",
         KeyStroke.getKeyStroke("HOME"), "caret-begin-line",
         KeyStroke.getKeyStroke("END"), "caret-end-line",
         KeyStroke.getKeyStroke("DELETE"), "delete-next",
         KeyStroke.getKeyStroke("shift ctrl O"), "toggle-componentOrientation",
         KeyStroke.getKeyStroke("shift KP_LEFT"), "selection-backward",
         KeyStroke.getKeyStroke("ctrl H"), "delete-previous",
         KeyStroke.getKeyStroke("KP_LEFT"), "caret-backward",
         KeyStroke.getKeyStroke("KP_RIGHT"), "caret-forward",
         KeyStroke.getKeyStroke("shift ctrl KP_RIGHT"), "selection-next-word",
         KeyStroke.getKeyStroke("COPY"), "copy-to-clipboard",
         KeyStroke.getKeyStroke("shift HOME"), "selection-begin-line",
         KeyStroke.getKeyStroke("shift ctrl LEFT"), "selection-previous-word",
         KeyStroke.getKeyStroke("ctrl KP_LEFT"), "caret-previous-word",
         KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "caret-next-word",
         KeyStroke.getKeyStroke("PASTE"), "paste-from-clipboard",
         KeyStroke.getKeyStroke("shift ctrl RIGHT"), "selection-next-word",
         KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "unselect",
         KeyStroke.getKeyStroke("ctrl A"), "select-all",
         KeyStroke.getKeyStroke("shift KP_RIGHT"), "selection-forward",
         KeyStroke.getKeyStroke("CUT"), "cut-to-clipboard",
         KeyStroke.getKeyStroke("ctrl LEFT"), "caret-previous-word",
         KeyStroke.getKeyStroke("shift ctrl KP_LEFT"), "selection-previous-word",
         KeyStroke.getKeyStroke("shift END"), "selection-end-line",
         KeyStroke.getKeyStroke("ctrl RIGHT"), "caret-next-word"
      }),
      "TextField.margin", new InsetsUIResource(0, 0, 0, 0),
      "TextField.selectionBackground", new ColorUIResource(Color.black),
      "TextField.selectionForeground", new ColorUIResource(Color.white),
      "TextPane.background", new ColorUIResource(Color.white),
      "TextPane.border", BasicBorders.getMarginBorder(),
      "TextPane.caretBlinkRate", new Integer(500),
      "TextPane.caretForeground", new ColorUIResource(Color.black),
      "TextPane.font", new FontUIResource("Serif", Font.PLAIN, 12),
      "TextPane.foreground", new ColorUIResource(Color.black),
      "TextPane.inactiveForeground", new ColorUIResource(Color.gray),
      "TextPane.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
          KeyStroke.getKeyStroke("shift UP"), "selection-up",
          KeyStroke.getKeyStroke("ctrl RIGHT"), "caret-next-word",
          KeyStroke.getKeyStroke("shift ctrl LEFT"), "selection-previous-word",
          KeyStroke.getKeyStroke("shift KP_UP"), "selection-up",
          KeyStroke.getKeyStroke("DOWN"), "caret-down",
          KeyStroke.getKeyStroke("shift ctrl T"), "previous-link-action",
          KeyStroke.getKeyStroke("ctrl LEFT"), "caret-previous-word",
          KeyStroke.getKeyStroke("CUT"), "cut-to-clipboard",
          KeyStroke.getKeyStroke("END"), "caret-end-line",
          KeyStroke.getKeyStroke("shift PAGE_UP"), "selection-page-up",
          KeyStroke.getKeyStroke("KP_UP"), "caret-up",
          KeyStroke.getKeyStroke("DELETE"), "delete-next",
          KeyStroke.getKeyStroke("ctrl HOME"), "caret-begin",
          KeyStroke.getKeyStroke("shift LEFT"), "selection-backward",
          KeyStroke.getKeyStroke("ctrl END"), "caret-end",
          KeyStroke.getKeyStroke("BACK_SPACE"), "delete-previous",
          KeyStroke.getKeyStroke("shift ctrl RIGHT"), "selection-next-word",
          KeyStroke.getKeyStroke("LEFT"), "caret-backward",
          KeyStroke.getKeyStroke("KP_LEFT"), "caret-backward",
          KeyStroke.getKeyStroke("shift KP_RIGHT"), "selection-forward",
          KeyStroke.getKeyStroke("ctrl SPACE"), "activate-link-action",
          KeyStroke.getKeyStroke("ctrl H"), "delete-previous",
          KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "unselect",
          KeyStroke.getKeyStroke("ENTER"), "insert-break",
          KeyStroke.getKeyStroke("shift HOME"), "selection-begin-line",
          KeyStroke.getKeyStroke("RIGHT"), "caret-forward",
          KeyStroke.getKeyStroke("shift ctrl PAGE_UP"), "selection-page-left",
          KeyStroke.getKeyStroke("shift DOWN"), "selection-down",
          KeyStroke.getKeyStroke("PAGE_DOWN"), "page-down",
          KeyStroke.getKeyStroke("shift KP_LEFT"), "selection-backward",
          KeyStroke.getKeyStroke("shift ctrl O"), "toggle-componentOrientation",
          KeyStroke.getKeyStroke("ctrl X"), "cut-to-clipboard",
          KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN"), "selection-page-right",
          KeyStroke.getKeyStroke("ctrl C"), "copy-to-clipboard",
          KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "caret-next-word",
          KeyStroke.getKeyStroke("shift END"), "selection-end-line",
          KeyStroke.getKeyStroke("ctrl KP_LEFT"), "caret-previous-word",
          KeyStroke.getKeyStroke("HOME"), "caret-begin-line",
          KeyStroke.getKeyStroke("ctrl V"), "paste-from-clipboard",
          KeyStroke.getKeyStroke("KP_DOWN"), "caret-down",
          KeyStroke.getKeyStroke("ctrl A"), "select-all",
          KeyStroke.getKeyStroke("shift RIGHT"), "selection-forward",
          KeyStroke.getKeyStroke("shift ctrl END"), "selection-end",
          KeyStroke.getKeyStroke("COPY"), "copy-to-clipboard",
          KeyStroke.getKeyStroke("shift ctrl KP_LEFT"), "selection-previous-word",
          KeyStroke.getKeyStroke("ctrl T"), "next-link-action",
          KeyStroke.getKeyStroke("shift KP_DOWN"), "selection-down",
          KeyStroke.getKeyStroke("TAB"), "insert-tab",
          KeyStroke.getKeyStroke("UP"), "caret-up",
          KeyStroke.getKeyStroke("shift ctrl HOME"), "selection-begin",
          KeyStroke.getKeyStroke("shift PAGE_DOWN"), "selection-page-down",
          KeyStroke.getKeyStroke("KP_RIGHT"), "caret-forward",
          KeyStroke.getKeyStroke("shift ctrl KP_RIGHT"), "selection-next-word",
          KeyStroke.getKeyStroke("PAGE_UP"), "page-up",
          KeyStroke.getKeyStroke("PASTE"), "paste-from-clipboard"
      }),
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
          KeyStroke.getKeyStroke("SPACE"),  "pressed",
          KeyStroke.getKeyStroke("released SPACE"), "released"
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
      "ToolBar.separatorSize", new DimensionUIResource(10, 10),
      "ToolBar.shadow", new ColorUIResource(shadow),
      "ToolTip.background", new ColorUIResource(light),
      "ToolTip.border", new BorderUIResource.LineBorderUIResource(Color.lightGray),
      "ToolTip.font", new FontUIResource("SansSerif", Font.PLAIN, 12),
      "ToolTip.foreground", new ColorUIResource(darkShadow),
      "Tree.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[] {
        "ESCAPE", "cancel"
      }),
      "Tree.background", new ColorUIResource(new Color(255, 255, 255)),
      "Tree.changeSelectionWithFocus", Boolean.TRUE,
      "Tree.drawsFocusBorderAroundIcon", Boolean.FALSE,
      "Tree.editorBorder", new BorderUIResource.LineBorderUIResource(Color.lightGray),
      "Tree.focusInputMap", new UIDefaults.LazyInputMap(new Object[] {
              KeyStroke.getKeyStroke("ctrl DOWN"), "selectNextChangeLead",
              KeyStroke.getKeyStroke("shift UP"), "selectPreviousExtendSelection",
              KeyStroke.getKeyStroke("ctrl RIGHT"), "scrollRight",
              KeyStroke.getKeyStroke("shift KP_UP"), "selectPreviousExtendSelection",
              KeyStroke.getKeyStroke("DOWN"), "selectNext",
              KeyStroke.getKeyStroke("ctrl UP"), "selectPreviousChangeLead",
              KeyStroke.getKeyStroke("ctrl LEFT"), "scrollLeft",
              KeyStroke.getKeyStroke("CUT"), "cut",
              KeyStroke.getKeyStroke("END"), "selectLast",
              KeyStroke.getKeyStroke("shift PAGE_UP"), "scrollUpExtendSelection",
              KeyStroke.getKeyStroke("KP_UP"), "selectPrevious",
              KeyStroke.getKeyStroke("shift ctrl UP"), "selectPreviousExtendSelection",
              KeyStroke.getKeyStroke("ctrl HOME"), "selectFirstChangeLead",
              KeyStroke.getKeyStroke("ctrl END"), "selectLastChangeLead",
              KeyStroke.getKeyStroke("ctrl PAGE_DOWN"), "scrollDownChangeLead",
              KeyStroke.getKeyStroke("LEFT"), "selectParent",
              KeyStroke.getKeyStroke("ctrl PAGE_UP"), "scrollUpChangeLead",
              KeyStroke.getKeyStroke("KP_LEFT"), "selectParent",
              KeyStroke.getKeyStroke("SPACE"), "addToSelection",
              KeyStroke.getKeyStroke("ctrl SPACE"), "toggleAndAnchor",
              KeyStroke.getKeyStroke("shift SPACE"), "extendTo",
              KeyStroke.getKeyStroke("shift ctrl SPACE"), "moveSelectionTo",
              KeyStroke.getKeyStroke("ADD"), "expand",
              KeyStroke.getKeyStroke("ctrl BACK_SLASH"), "clearSelection",
              KeyStroke.getKeyStroke("shift ctrl DOWN"), "selectNextExtendSelection",
              KeyStroke.getKeyStroke("shift HOME"), "selectFirstExtendSelection",
              KeyStroke.getKeyStroke("RIGHT"), "selectChild",
              KeyStroke.getKeyStroke("shift ctrl PAGE_UP"), "scrollUpExtendSelection",
              KeyStroke.getKeyStroke("shift DOWN"), "selectNextExtendSelection",
              KeyStroke.getKeyStroke("PAGE_DOWN"), "scrollDownChangeSelection",
              KeyStroke.getKeyStroke("shift ctrl KP_UP"), "selectPreviousExtendSelection",
              KeyStroke.getKeyStroke("SUBTRACT"), "collapse",
              KeyStroke.getKeyStroke("ctrl X"), "cut",
              KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN"), "scrollDownExtendSelection",
              KeyStroke.getKeyStroke("ctrl SLASH"), "selectAll",
              KeyStroke.getKeyStroke("ctrl C"), "copy",
              KeyStroke.getKeyStroke("ctrl KP_RIGHT"), "scrollRight",
              KeyStroke.getKeyStroke("shift END"), "selectLastExtendSelection",
              KeyStroke.getKeyStroke("shift ctrl KP_DOWN"), "selectNextExtendSelection",
              KeyStroke.getKeyStroke("ctrl KP_LEFT"), "scrollLeft",
              KeyStroke.getKeyStroke("HOME"), "selectFirst",
              KeyStroke.getKeyStroke("ctrl V"), "paste",
              KeyStroke.getKeyStroke("KP_DOWN"), "selectNext",
              KeyStroke.getKeyStroke("ctrl A"), "selectAll",
              KeyStroke.getKeyStroke("ctrl KP_DOWN"), "selectNextChangeLead",
              KeyStroke.getKeyStroke("shift ctrl END"), "selectLastExtendSelection",
              KeyStroke.getKeyStroke("COPY"), "copy",
              KeyStroke.getKeyStroke("ctrl KP_UP"), "selectPreviousChangeLead",
              KeyStroke.getKeyStroke("shift KP_DOWN"), "selectNextExtendSelection",
              KeyStroke.getKeyStroke("UP"), "selectPrevious",
              KeyStroke.getKeyStroke("shift ctrl HOME"), "selectFirstExtendSelection",
              KeyStroke.getKeyStroke("shift PAGE_DOWN"), "scrollDownExtendSelection",
              KeyStroke.getKeyStroke("KP_RIGHT"), "selectChild",
              KeyStroke.getKeyStroke("F2"), "startEditing",
              KeyStroke.getKeyStroke("PAGE_UP"), "scrollUpChangeSelection",
              KeyStroke.getKeyStroke("PASTE"), "paste"
      }),
      "Tree.font", new FontUIResource("Dialog", Font.PLAIN, 12),
      "Tree.foreground", new ColorUIResource(Color.black),
      "Tree.hash", new ColorUIResource(new Color(184, 207, 228)),
      "Tree.leftChildIndent", new Integer(7),
      "Tree.rightChildIndent", new Integer(13),
      "Tree.rowHeight", new Integer(16),
      "Tree.scrollsOnExpand", Boolean.TRUE,
      "Tree.selectionBackground", new ColorUIResource(Color.black),
      "Tree.nonSelectionBackground", new ColorUIResource(new Color(255, 255, 255)),
      "Tree.selectionBorderColor", new ColorUIResource(Color.black),
      "Tree.selectionBorder", new BorderUIResource.LineBorderUIResource(Color.black),
      "Tree.selectionForeground", new ColorUIResource(new Color(255, 255, 255)),
      "Viewport.background", new ColorUIResource(light),
      "Viewport.foreground", new ColorUIResource(Color.black),
      "Viewport.font", new FontUIResource("Dialog", Font.PLAIN, 12)
    };
    defaults.putDefaults(uiDefaults);
  }

  /**
   * Returns the <code>ActionMap</code> that stores all the actions that are
   * responsibly for rendering auditory cues.
   *
   * @return the action map that stores all the actions that are
   *         responsibly for rendering auditory cues
   *
   * @see #createAudioAction
   * @see #playSound
   *
   * @since 1.4
   */
  protected ActionMap getAudioActionMap()
  {
    if (audioActionMap != null)
      audioActionMap = new ActionMap();
    return audioActionMap;
  }

  /**
   * Creates an <code>Action</code> that can play an auditory cue specified by
   * the key. The UIDefaults value for the key is normally a String that points
   * to an audio file relative to the current package.
   *
   * @param key a UIDefaults key that specifies the sound
   *
   * @return an action that can play the sound
   *
   * @see #playSound
   *
   * @since 1.4
   */
  protected Action createAudioAction(Object key)
  {
    return new AudioAction(key);
  }

  /**
   * Plays the sound of the action if it is listed in
   * <code>AuditoryCues.playList</code>.
   *
   * @param audioAction the audio action to play
   *
   * @since 1.4
   */
  protected void playSound(Action audioAction)
  {
    if (audioAction instanceof AudioAction)
      {
        Object[] playList = (Object[]) UIManager.get("AuditoryCues.playList");
        for (int i = 0; i < playList.length; ++i)
          {
            if (playList[i].equals(((AudioAction) audioAction).key))
              {
                ActionEvent ev = new ActionEvent(this,
                                                 ActionEvent.ACTION_PERFORMED,
                                                 (String) playList[i]);
                audioAction.actionPerformed(ev);
                break;
              }
          }
      }
  }

  /**
   * Initializes the Look and Feel.
   */
  public void initialize()
  {
    Toolkit toolkit = Toolkit.getDefaultToolkit();
    popupHelper = new PopupHelper();
    toolkit.addAWTEventListener(popupHelper, AWTEvent.MOUSE_EVENT_MASK);
  }

  /**
   * Uninitializes the Look and Feel.
   */
  public void uninitialize()
  {
    Toolkit toolkit = Toolkit.getDefaultToolkit();
    toolkit.removeAWTEventListener(popupHelper);
    popupHelper = null;
  }
}
