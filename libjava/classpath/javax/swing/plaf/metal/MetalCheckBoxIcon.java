/* MetalCheckBoxIcon.java -- An icon for JCheckBoxes in the Metal L&F
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
import java.awt.Component;
import java.awt.Graphics;

import java.io.Serializable;

import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.plaf.UIResource;

/**
 * An {@link Icon} implementation for {@link JCheckBox}es in the
 * Metal Look &amp; Feel.
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
    g.setColor(Color.BLACK);
    g.drawLine(3, 5, 3, 9);
    g.drawLine(4, 5, 4, 9);
    g.drawLine(5, 7, 9, 3);
    g.drawLine(5, 8, 9, 4);
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
   * @param x the Y position
   */
  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    border.paintBorder(c, g, x, y, getIconWidth(), getIconHeight());
    JCheckBox cb = (JCheckBox) c;
    if (cb.isSelected())
      drawCheck(c, g, x, y);
  }
}
