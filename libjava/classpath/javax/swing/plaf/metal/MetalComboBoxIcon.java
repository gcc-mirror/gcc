/* MetalComboBoxIcon.java
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

/**
 * An icon used by the {@link MetalComboBoxUI} class.
 */
public class MetalComboBoxIcon implements Icon, Serializable 
{
  
  /**
   * Creates a new icon.
   */
  public MetalComboBoxIcon() 
  {
    // nothing required.
  }
  
  /**
   * Returns the icon width, which for this icon is 10 pixels.
   * 
   * @return <code>10</code>.
   */
  public int getIconWidth()
  {
    return 10;
  }

  /**
   * Returns the icon height, which for this icon is 5 pixels.
   * 
   * @return <code>5</code>.
   */
  public int getIconHeight()
  {
    return 5;
  }

  /**
   * Paints the icon at the location (x, y).
   * 
   * @param c  the combo box (ignored here).
   * @param g  the graphics device.
   * @param x  the x coordinate.
   * @param y  the y coordinate.
   */
  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    Color savedColor = g.getColor();
    if (c.isEnabled())
      g.setColor(MetalLookAndFeel.getBlack());
    else
      g.setColor(MetalLookAndFeel.getControlDisabled());
    for (int i = 0; i < 5; i++)
      g.drawLine(x + i, y + i, x + 9 - i, y + i);
    g.setColor(savedColor);
  }

}
