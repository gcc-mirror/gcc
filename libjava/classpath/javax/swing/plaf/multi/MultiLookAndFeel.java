/* MultiLookAndFeel.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package javax.swing.plaf.multi;

import java.util.Vector;

import javax.swing.JComponent;
import javax.swing.LookAndFeel;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;

/**
 * A look and feel that provides the ability to use auxiliary look and feels
 * in addition to the primary look and feel.
 */
public class MultiLookAndFeel extends LookAndFeel 
{

  /**
   * Creates a new instance of the look and feel.
   */
  public MultiLookAndFeel()
  {
    // Nothing to do here.
  }
  
  /**
   * Returns the name for the look and feel.
   * 
   * @return "Multiplexing Look and Feel".
   */
  public String getName()
  {
    return "Multiplexing Look and Feel";
  }
  
  /**
   * Returns an identifier for the look and feel.
   * 
   * @return "Multiplex".
   */
  public String getID()
  {
    return "Multiplex"; 
  }
  
  /**
   * Returns a description of the look and feel.
   * 
   * @return A description of the look and feel.
   */
  public String getDescription()
  {
    return "Allows multiple UI instances per component instance";    
  }
  
  /**
   * Returns <code>false</code> to indicate that this look and feel is not 
   * native to any platform.
   * 
   * @return <code>false</code>.
   */
  public boolean isNativeLookAndFeel()
  {
    return false;    
  }

  /**
   * Returns <code>true</code> always, since this look and feel is supported on
   * all platforms.
   * 
   * @return <code>true</code>.
   */
  public boolean isSupportedLookAndFeel()
  {
    return true;
  }
  
  /**
   * Creates and returns the UI defaults for this look and feel.
   * 
   * @return The UI defaults.
   */
  public UIDefaults getDefaults()
  {
    UIDefaults defaults = new UIDefaults();
    defaults.put("ButtonUI", "javax.swing.plaf.multi.MultiButtonUI");
    defaults.put("CheckBoxUI", "javax.swing.plaf.multi.MultiButtonUI");
    defaults.put("CheckBoxMenuItemUI", "javax.swing.plaf.multi.MultiMenuItemUI");
    defaults.put("ColorChooserUI", 
        "javax.swing.plaf.multi.MultiColorChooserUI");
    defaults.put("ComboBoxUI", "javax.swing.plaf.multi.MultiComboBoxUI");
    defaults.put("DesktopPaneUI", "javax.swing.plaf.multi.MultiDesktopPaneUI");
    defaults.put("DesktopIconUI", "javax.swing.plaf.multi.MultiDesktopIconUI");
    defaults.put("EditorPaneUI", "javax.swing.plaf.multi.MultiTextUI");
    defaults.put("FileChooserUI", "javax.swing.plaf.multi.MultiFileChooserUI");
    defaults.put("FormattedTextFieldUI", "javax.swing.plaf.multi.MultiTextUI");
    defaults.put("InternalFrameUI", 
        "javax.swing.plaf.multi.MultiInternalFrameUI");
    defaults.put("LabelUI", "javax.swing.plaf.multi.MultiLabelUI");
    defaults.put("ListUI", "javax.swing.plaf.multi.MultiListUI");
    defaults.put("MenuItemUI", "javax.swing.plaf.multi.MultiMenuItemUI");
    defaults.put("MenuUI", "javax.swing.plaf.multi.MultiMenuItemUI");
    defaults.put("MenuBarUI", "javax.swing.plaf.multi.MultiMenuBarUI");
    defaults.put("OptionPaneUI", "javax.swing.plaf.multi.MultiOptionPaneUI");
    defaults.put("PanelUI", "javax.swing.plaf.multi.MultiPanelUI");
    defaults.put("PasswordFieldUI", "javax.swing.plaf.multi.MultiTextUI");
    defaults.put("PopupMenuUI", "javax.swing.plaf.multi.MultiPopupMenuUI");
    defaults.put("PopupMenuSeparatorUI", 
        "javax.swing.plaf.multi.MultiSeparatorUI");
    defaults.put("ProgressBarUI", "javax.swing.plaf.multi.MultiProgressBarUI");
    defaults.put("RadioButtonUI", "javax.swing.plaf.multi.MultiButtonUI");
    defaults.put("RadioButtonMenuItemUI", 
        "javax.swing.plaf.multi.MultiMenuItemUI");
    defaults.put("RootPaneUI", "javax.swing.plaf.multi.MultiRootPaneUI");
    defaults.put("ScrollBarUI", "javax.swing.plaf.multi.MultiScrollBarUI");
    defaults.put("ScrollPaneUI", "javax.swing.plaf.multi.MultiScrollPaneUI");
    defaults.put("SeparatorUI", "javax.swing.plaf.multi.MultiSeparatorUI");
    defaults.put("SliderUI", "javax.swing.plaf.multi.MultiSliderUI");
    defaults.put("SpinnerUI", "javax.swing.plaf.multi.MultiSpinnerUI");
    defaults.put("SplitPaneUI", "javax.swing.plaf.multi.MultiSplitPaneUI");
    defaults.put("TabbedPaneUI", "javax.swing.plaf.multi.MultiTabbedPaneUI");
    defaults.put("TableHeaderUI", "javax.swing.plaf.multi.MultiTableHeaderUI");
    defaults.put("TableUI", "javax.swing.plaf.multi.MultiTableUI");
    defaults.put("TextAreaUI", "javax.swing.plaf.multi.MultiTextUI");
    defaults.put("TextFieldUI", "javax.swing.plaf.multi.MultiTextUI");
    defaults.put("TextPaneUI", "javax.swing.plaf.multi.MultiTextUI");
    defaults.put("ToggleButtonUI", "javax.swing.plaf.multi.MultiButtonUI");
    defaults.put("ToolBarSeparatorUI", 
        "javax.swing.plaf.multi.MultiSeparatorUI");
    defaults.put("ToolBarUI", "javax.swing.plaf.multi.MultiToolBarUI");
    defaults.put("ToolTipUI", "javax.swing.plaf.multi.MultiToolTipUI");
    defaults.put("ViewportUI", "javax.swing.plaf.multi.MultiViewportUI");
    return defaults;
  }
  
  /**
   * Creates the UI delegates for the <code>target</code> component and
   * returns a multiplexing UI delegate (<code>mui</code>) if there are
   * multiple delegates.
   * 
   * @param mui  a multiplexing UI delegate appropriate for the component.
   * @param uis  a vector into which the UI delegates will be added.
   * @param target  the target component.
   * 
   * @return A UI delegate.
   */
  public static ComponentUI createUIs(ComponentUI mui, Vector uis, 
                                      JComponent target)
  {
    // get primary UI delegate for 'target', and add it to uis
    ComponentUI ui = null;
    LookAndFeel primary = UIManager.getLookAndFeel();
    if (primary != null) 
    {
      ui = UIManager.getUI(target);
      uis.add(ui);
    }
    // for any auxiliary look and feels in use, get the UI delegate and add 
    // it to uis
    LookAndFeel[] auxlafs = UIManager.getAuxiliaryLookAndFeels();
    for (int i = 0; i < auxlafs.length; i++)
    {
      LookAndFeel auxlaf = auxlafs[i];
      // FIXME: here I call getDefaults() to get the UI delegate from the 
      // auxiliary look and feel.  But getDefaults() creates a new set of
      // defaults every time it is called, which is wasteful.  Unfortunately
      // I cannot find another way to get the UI delegate, so I'm doing it
      // anyway...
      UIDefaults defaults = auxlaf.getDefaults();
      ui = defaults.getUI(target);
      if (ui != null)
        uis.add(ui);
    }
    // if uis contains more than 1 delegate, return mui, otherwise return 
    // the primary delegate
    if (uis.size() > 1)
      return mui;
    else
      return ui;    
  }
  
  /**
   * Returns an array containing the same {@link ComponentUI} instances as
   * <code>uis</code>.  If <code>uis</code> is <code>null</code>, a zero-length
   * array is returned.
   * 
   * @param uis  a list of {@link ComponentUI} references (<code>null</code> 
   *             permitted).
   * 
   * @return An array containing the same {@link ComponentUI} instances as
   *         <code>uis</code>, or <code>null</code> if <code>uis</code> is
   *         empty.  
   */
  protected static ComponentUI[] uisToArray(Vector uis)
  {
    if (uis == null) 
      return new ComponentUI[0];
    int size = uis.size();
    if (size == 0) 
      return null;
    ComponentUI[] result = new ComponentUI[size];
    uis.copyInto(result);
    return result;    
  }

}
