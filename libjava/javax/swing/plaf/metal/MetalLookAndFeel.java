/* MetalLookAndFeel.java
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf.metal;

import java.awt.Color;
import java.awt.Insets;

import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.basic.BasicLookAndFeel;

public class MetalLookAndFeel extends BasicLookAndFeel
{	   
  private static final long serialVersionUID = 6680646159193457980L;
  private static MetalTheme theme;
  private UIDefaults LAF_defaults;

  public MetalLookAndFeel()
  {
    createDefaultTheme();
  }

  protected void createDefaultTheme()
  {
    setCurrentTheme(new DefaultMetalTheme());
  }

  public boolean isNativeLookAndFeel()
  {
    return true;
  }

  public boolean isSupportedLookAndFeel()
  {
    return true;
  }

  public String getDescription()
  {
    return "Metal look and feel";
  }

  public String getID()
  {
    return "MetalLookAndFeel";
  }

  public String getName()
  {
    return "MetalLookAndFeel";
  }

  public UIDefaults getDefaults()
  {
    if (LAF_defaults == null)
      LAF_defaults = super.getDefaults();

    // add custom theme entries to the table
    theme.addCustomEntriesToTable(LAF_defaults);
    
    // Returns the default values for this look and feel. 
    return LAF_defaults;
  }

  public static ColorUIResource getAcceleratorForeground()
  {
    return theme.getAcceleratorForeground();
  }

  public static ColorUIResource getAcceleratorSelectedForeground()
  {
    return theme.getAcceleratorSelectedForeground();
  }

  public static ColorUIResource getBlack()
  {
    return theme.getBlack();
  }

  public static ColorUIResource getControl()
  {
    return theme.getControl();
  }

  public static ColorUIResource getControlDarkShadow()
  {
    return theme.getControlDarkShadow();
  }

  public static ColorUIResource getControlDisabled()
  {
    return theme.getControlDisabled();
  }

  public static ColorUIResource getControlHighlight()
  {
    return theme.getControlHighlight();
  }

  public static ColorUIResource getControlInfo()
  {
    return theme.getControlInfo();
  }

  public static ColorUIResource getControlShadow()
  {
    return theme.getControlShadow();
  }

  public static ColorUIResource getControlTextColor()
  {
    return theme.getControlTextColor();
  }

  public static FontUIResource getControlTextFont()
  {
    return theme.getControlTextFont();
  }

  public static ColorUIResource getDesktopColor()
  {
    return theme.getDesktopColor();
  }

  public static ColorUIResource getFocusColor()
  {
    return theme.getFocusColor();
  }

  public static ColorUIResource getHighlightedTextColor()
  {
    return theme.getHighlightedTextColor();
  }

  public static ColorUIResource getInactiveControlTextColor()
  {
    return theme.getInactiveControlTextColor();
  }

  public static ColorUIResource getInactiveSystemTextColor()
  {
    return theme.getInactiveSystemTextColor();
  }

  public static ColorUIResource getMenuBackground()
  {
    return theme.getMenuBackground();
  }

  public static ColorUIResource getMenuDisabledForeground()
  {
    return theme.getMenuDisabledForeground();
  }

  public static ColorUIResource getMenuForeground()
  {
    return theme.getMenuForeground();
  }

  public static ColorUIResource getMenuSelectedBackground()
  {
    return theme.getMenuSelectedBackground();
  }

  public static ColorUIResource getMenuSelectedForeground()
  {
    return theme.getMenuSelectedForeground();
  }

  public static FontUIResource getMenuTextFont()
  {
    return theme.getMenuTextFont();
  }

  public static ColorUIResource getPrimaryControl()
  {
    return theme.getPrimaryControl();
  }

  public static ColorUIResource getPrimaryControlDarkShadow()
  {
    return theme.getPrimaryControlDarkShadow();
  }

  public static ColorUIResource getPrimaryControlHighlight()
  {
    return theme.getPrimaryControlHighlight();
  }

  public static ColorUIResource getPrimaryControlInfo()
  {
    return theme.getPrimaryControlInfo();
  }

  public static ColorUIResource getPrimaryControlShadow()
  {
    return theme.getPrimaryControlShadow();
  }

  public static ColorUIResource getSeparatorBackground()
  {
    return theme.getSeparatorBackground();
  }

  public static ColorUIResource getSeparatorForeground()
  {
    return theme.getSeparatorForeground();
  }

  public static FontUIResource getSubTextFont()
  {
    return theme.getSubTextFont();
  }

  public static ColorUIResource getSystemTextColor()
  {
    return theme.getSystemTextColor();
  }

  public static FontUIResource getSystemTextFont()
  {
    return theme.getSystemTextFont();
  }

  public static ColorUIResource getTextHighlightColor()
  {
    return theme.getTextHighlightColor();
  }

  public static ColorUIResource getUserTextColor()
  {
    return theme.getUserTextColor();
  }

  public static FontUIResource getUserTextFont()
  {
    return theme.getUserTextFont();
  }

  public static ColorUIResource getWhite()
  {
    return theme.getWhite();
  }

  public static ColorUIResource getWindowBackground()
  {
    return theme.getWindowBackground();
  }

  public static ColorUIResource getWindowTitleBackground()
  {
    return theme.getWindowTitleBackground();
  }

  public static FontUIResource getWindowTitleFont()
  {
    return theme.getWindowTitleFont();
  }

  public static ColorUIResource getWindowTitleForeground()
  {
    return theme.getWindowTitleForeground();
  }

  public static ColorUIResource getWindowTitleInactiveBackground()
  {
    return theme.getWindowTitleInactiveBackground();
  }

  public static ColorUIResource getWindowTitleInactiveForeground()
  {
    return theme.getWindowTitleInactiveForeground();
  }

  public static void setCurrentTheme(MetalTheme theme)
  {
    MetalLookAndFeel.theme = theme;
  }

  /**
   * Sets the ComponentUI classes for all Swing components to the Metal
   * implementations.
   *
   * In particular this sets the following keys:
   *
   * <table>
   * <tr>
   * <th>Key</th><th>Value</th>
   * </tr><tr>
   * <td>ButtonUI</td><td>{@link MetalButtonUI}</td>
   * </tr><tr>
   * <td>CheckBoxUI</td><td>{@link MetalCheckBoxUI}</td>
   * </tr><tr>
   * <td>ComboBoxUI</td><td>{@link MetalComboBoxUI}</td>
   * </tr><tr>
   * <td>DesktopIconUI</td><td>{@link MetalDesktopIconUI}</td>
   * </tr><tr>
   * <td>InternalFrameUI</td><td>{@link MetalInternalFrameUI}</td>
   * </tr><tr>
   * <td>LabelUI</td><td>{@link MetalLabelUI}</td>
   * </tr><tr>
   * <td>PopupMenuSeparatorUI</td><td>{@link MetalPopupMenuSeparatorUI}</td>
   * </tr><tr>
   * <td>ProgressBarUI</td><td>{@link MetalProgressBarUI}</td>
   * </tr><tr>
   * <td>RadioButtonUI</td><td>{@link MetalRadioButtonUI}</td>
   * </tr><tr>
   * <td>RootPaneUI</td><td>{@link MetalRootPaneUI}</td>
   * </tr><tr>
   * <td>ScrollBarUI</td><td>{@link MetalScrollBarUI}</td>
   * </tr><tr>
   * <td>ScrollPaneUI</td><td>{@link MetalScrollPaneUI}</td>
   * </tr><tr>
   * <td>SeparatorUI</td><td>{@link MetalSeparatorUI}</td>
   * </tr><tr>
   * <td>SliderUI</td><td>{@link MetalSliderUI}</td>
   * </tr><tr>
   * <td>SplitPaneUI</td><td>{@link MetalSplitPaneUI}</td>
   * </tr><tr>
   * <td>TabbedPaneUI</td><td>{@link MetalTabbedPaneUI}</td>
   * </tr><tr>
   * <td>TextFieldUI</td><td>{@link MetalTextFieldUI}</td>
   * </tr><tr>
   * <td>ToggleButtonUI</td><td>{@link MetalToggleButtonUI}</td>
   * </tr><tr>
   * <td>ToolBarUI</td><td>{@link MetalToolBarUI}</td>
   * </tr><tr>
   * <td>ToolTipUI</td><td>{@link MetalToolTipUI}</td>
   * </tr><tr>
   * <td>TreeUI</td><td>{@link MetalTreeUI}</td>
   * </tr><tr>
   * </table>
   *
   * @param defaults the UIDefaults where the class defaults are added
   */
  protected void initClassDefaults(UIDefaults defaults)
  {
    super.initClassDefaults(defaults);

    // Variables
    Object[] uiDefaults;
    // Initialize Class Defaults
    uiDefaults = new Object[] {
      "ButtonUI", "javax.swing.plaf.metal.MetalButtonUI",
      "CheckBoxUI", "javax.swing.plaf.metal.MetalCheckBoxUI",
      "ComboBoxUI", "javax.swing.plaf.metal.MetalComboBoxUI",
      "DesktopIconUI", "javax.swing.plaf.metal.MetalDesktopIconUI",
      "InternalFrameUI", "javax.swing.plaf.metal.MetalInternalFrameUI",
      "LabelUI", "javax.swing.plaf.metal.MetalLabelUI",
      "PopupMenuSeparatorUI",
      "javax.swing.plaf.metal.MetalPopupMenuSeparatorUI",
      "ProgressBarUI", "javax.swing.plaf.metal.MetalProgressBarUI",
      "RadioButtonUI", "javax.swing.plaf.metal.MetalRadioButtonUI",
      "RootPaneUI", "javax.swing.plaf.metal.MetalRootPaneUI",
      "ScrollBarUI", "javax.swing.plaf.metal.MetalScrollBarUI",
      "ScrollPaneUI", "javax.swing.plaf.metal.MetalScrollPaneUI",
      "SeparatorUI", "javax.swing.plaf.metal.MetalSeparatorUI",
      "SliderUI", "javax.swing.plaf.metal.MetalSliderUI",
      "SplitPaneUI", "javax.swing.plaf.metal.MetalSplitPaneUI",
      "TabbedPaneUI", "javax.swing.plaf.metal.MetalTabbedPaneUI",
      "TextFieldUI", "javax.swing.plaf.metal.MetalTextFieldUI",
      "ToggleButtonUI", "javax.swing.plaf.metal.MetalToggleButtonUI",
      "ToolBarUI", "javax.swing.plaf.metal.MetalToolBarUI",
      "ToolTipUI", "javax.swing.plaf.metal.MetalToolTipUI",
      "TreeUI", "javax.swing.plaf.metal.MetalTreeUI",
    };
    // Add Class Defaults to UI Defaults table
    defaults.putDefaults(uiDefaults);
  }

  /**
   * Initializes the component defaults for the Metal Look &amp; Feel.
   *
   * In particular this sets the following keys (the colors are given
   * as RGB hex values):
   *
   * <table>
   * <tr>
   * <th>Key</th><th>Value</th>
   * </tr><tr>
   * <td>Button.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>Button.border</td><td>{@link MetalBorders.ButtonBorder}</td>
   * </tr><tr>
   * <td>Button.font</td><td>{@link #getControlTextFont}</td>
   * </tr><tr>
   * <td>Button.margin</td><td><code>new java.awt.Insets(2, 14, 2, 14)</code>
   * </td>
   * </tr><tr>
   * <td>CheckBox.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>CheckBoxMenuItem.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>ToolBar.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>Panel.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>Slider.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>OptionPane.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>ProgressBar.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>TabbedPane.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>Label.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>Label.font</td><td>{@link #getControlTextFont}</td>
   * </tr><tr>
   * <td>Menu.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>MenuBar.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>MenuItem.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>ScrollBar.background</td><td>0xcccccc</td>
   * </tr><tr>
   * <td>PopupMenu.border</td>
   * <td><code>new javax.swing.plaf.metal.MetalBorders.PopupMenuBorder()</td>
   * </tr><tr>
   * </table>
   *
   * @param defaults the UIDefaults instance to which the values are added
   */
  protected void initComponentDefaults(UIDefaults defaults)
  {
    super.initComponentDefaults(defaults);
    Object[] myDefaults = new Object[] {
      "Button.background", new ColorUIResource(getControl()),
      "Button.border", MetalBorders.getButtonBorder(),
      "Button.darkShadow", new ColorUIResource(getControlDarkShadow()),
      "Button.disabledText", new ColorUIResource(getControlDisabled()),
      "Button.focus", new ColorUIResource(getFocusColor()),
      "Button.font", getControlTextFont(),
      "Button.foreground", new ColorUIResource(getSystemTextColor()),
      "Button.highlight", new ColorUIResource(getControlHighlight()),
      "Button.light", new ColorUIResource(getControlHighlight()),
      "Button.margin", new Insets(2, 14, 2, 14),
      "Button.select", new ColorUIResource(getPrimaryControlShadow()),
      "Button.shadow", new ColorUIResource(getPrimaryControlShadow()),
      "CheckBox.background", new ColorUIResource(getControl()),
      "CheckBoxMenuItem.background", new ColorUIResource(getControl()),
      "ToolBar.background", new ColorUIResource(getControl()),
      "Panel.background", new ColorUIResource(getControl()),
      "Slider.background", new ColorUIResource(getControl()),
      "OptionPane.background", new ColorUIResource(getControl()),
      "ProgressBar.background", new ColorUIResource(getControl()),
      "ScrollPane.border", new MetalBorders.ScrollPaneBorder(),
      "TabbedPane.background", new ColorUIResource(getControl()),
      "Label.background", new ColorUIResource(getControl()),
      "Label.font", getControlTextFont(),
      "Label.disabledForeground", new ColorUIResource(getControlDisabled()),
      "Label.foreground", new ColorUIResource(getSystemTextColor()),
      "Menu.background", new ColorUIResource(getControl()),
      "Menu.font", getControlTextFont(),
      "MenuBar.background", new ColorUIResource(getControl()),
      "MenuBar.font", getControlTextFont(),
      "MenuItem.background", new ColorUIResource(getControl()),
      "MenuItem.font", getControlTextFont(),
      "ScrollBar.background", new ColorUIResource(getControl()),
      "ScrollBar.shadow", new ColorUIResource(getControlShadow()),
      "ScrollBar.thumb", new ColorUIResource(getPrimaryControlShadow()),
      "ScrollBar.thumbDarkShadow",
      new ColorUIResource(getPrimaryControlDarkShadow()),
      "ScrollBar.thumbHighlight",
      new ColorUIResource(getPrimaryControl()),
      "PopupMenu.border", new MetalBorders.PopupMenuBorder()
    };
    defaults.putDefaults(myDefaults);
  }

  /**
   * Initializes the system color defaults.
   *
   * In particular this sets the following keys:
   *
   * <table>
   * <tr>
   * <th>Key</th><th>Value</th><th>Description</th>
   * </tr><tr>
   * <td>control</td><td>0xcccccc</td><td>The default color for components</td>
   * </tr>
   * </table>
   */
  protected void initSystemColorDefaults(UIDefaults defaults)
  {
    super.initSystemColorDefaults(defaults);
    Object[] uiDefaults;
    uiDefaults = new Object[] {
      "control", new ColorUIResource(getControl())
    };
    defaults.putDefaults(uiDefaults);
  }

}
