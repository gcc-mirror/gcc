/* MetalCheckBoxIcon.java -- An icon for JCheckBoxes in the Metal L&F
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Graphics;
import java.io.Serializable;

import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.UIResource;

/**
 * An {@link Icon} used by the {@link MetalCheckBoxUI} class.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class MetalCheckBoxIcon
  implements Icon, UIResource, Serializable
{

  /** Used to paint the border of the icon. */
  MetalBorders.ButtonBorder border;

  /**
   * Creates a new MetalCheckBoxIcon instance.
   */
  public MetalCheckBoxIcon()
  {
    border = new MetalBorders.ButtonBorder();
  }

  /**
   * Draws the check in the CheckBox.
   *
   * @param c the component to draw on
   * @param g the Graphics context to draw with
   * @param x the X position
   * @param y the Y position
   */
  protected void drawCheck(Component c, Graphics g, int x, int y)
  {
    if (c.isEnabled())
      g.setColor(MetalLookAndFeel.getBlack());
    else
      g.setColor(MetalLookAndFeel.getControlDisabled());
    g.drawLine(3 + x, 5 + y, 3 + x, 9 + y);
    g.drawLine(4 + x, 5 + y, 4 + x, 9 + y);
    g.drawLine(5 + x, 7 + y, 9 + x, 3 + y);
    g.drawLine(5 + x, 8 + y, 9 + x, 4 + y);
  }

  /**
   * Returns the size (both X and Y) of the checkbox icon.
   *
   * @return the size of the checkbox icon
   */
  protected int getControlSize()
  {
    return 13;
  }

  /**
   * Returns the width of the icon in pixels.
   *
   * @return the width of the icon in pixels
   */
  public int getIconWidth()
  {
    return getControlSize();
  }

  /**
   * Returns the height of the icon in pixels.
   *
   * @return the height of the icon in pixels
   */
  public int getIconHeight()
  {
    return getControlSize();
  }

  /**
   * Paints the icon. This first paints the border of the CheckBox and
   * if the CheckBox is selected it calls {@link #drawCheck} to draw
   * the check.
   *
   * @param c the Component to draw on (gets casted to JCheckBox)
   * @param g the Graphics context to draw with
   * @param x the X position
   * @param y the Y position
   */
  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    if (UIManager.get("CheckBox.gradient") != null)
      MetalUtils.paintGradient(g, x, y, getIconWidth(), getIconHeight(),
                               SwingConstants.VERTICAL, "CheckBox.gradient");
    border.paintBorder(c, g, x, y, getIconWidth(), getIconHeight());
    
    AbstractButton b = (AbstractButton) c;
    if (b.isSelected())
      drawCheck(b, g, x, y);
  }
}
