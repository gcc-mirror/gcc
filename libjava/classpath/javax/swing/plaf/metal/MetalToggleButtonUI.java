/* MetalToggleButtonUI.java
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


package javax.swing.plaf.metal;

import java.awt.Color;

import javax.swing.JComponent;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicToggleButtonUI;

public class MetalToggleButtonUI
  extends BasicToggleButtonUI
{

  /** The color for the focus border. */
  protected Color focusColor;

  /** The color that indicates a selected button. */
  protected Color selectColor;

  /** The color for disabled button labels. */
  protected Color disabledTextColor;

  /** The shared UI instance for MetalToggleButtonUIs */
  private static MetalToggleButtonUI instance = null;

  /**
   * Constructs a new instance of MetalToggleButtonUI.
   */
  public MetalToggleButtonUI()
  {
    super();
    focusColor = getFocusColor();
    selectColor = getSelectColor();
    disabledTextColor = getDisabledTextColor();
  }


  /**
   * Returns the color for the focus border.
   *
   * @return the color for the focus border
   */
  protected Color getFocusColor()
  {
    UIDefaults def = UIManager.getLookAndFeelDefaults();
    return def.getColor(getPropertyPrefix() + ".focus");
  }

  /**
   * Returns the color that indicates a selected button.
   *
   * @return the color that indicates a selected button
   */
  protected Color getSelectColor()
  {
    UIDefaults def = UIManager.getLookAndFeelDefaults();
    return def.getColor(getPropertyPrefix() + ".select");
  }

  /**
   * Returns the color for the text label of disabled buttons.
   *
   * @return the color for the text label of disabled buttons
   */
  protected Color getDisabledTextColor()
  {
    UIDefaults def = UIManager.getLookAndFeelDefaults();
    return def.getColor(getPropertyPrefix() + ".disabledText");
  }

  /**
   * Returns an instance of MetalToggleButtonUI.
   *
   * @param component the component for which we return an UI instance
   *
   * @return an instance of MetalToggleButtonUI
   */
  public static ComponentUI createUI(JComponent component)
  {
    if (instance == null)
      instance = new MetalToggleButtonUI();
    return instance;
  }
}
