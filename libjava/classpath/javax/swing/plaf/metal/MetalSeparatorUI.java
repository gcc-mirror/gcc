/* MetalSeparatorUI.java
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
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.JComponent;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSeparatorUI;

/**
 * A UI delegate for the {@link JSeparator} component.
 */
public class MetalSeparatorUI
  extends BasicSeparatorUI
{

  // FIXME: maybe replace by a Map of instances when this becomes stateful
  /** The shared UI instance for MetalSeparatorUIs */
  private static MetalSeparatorUI instance = null;

  /**
   * Constructs a new instance of <code>MetalSeparatorUI</code>.
   */
  public MetalSeparatorUI()
  {
    super();
  }

  /**
   * Returns a shared instance of <code>MetalSeparatorUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A shared instance of <code>MetalSeparatorUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    if (instance == null)
      instance = new MetalSeparatorUI();
    return instance;
  }

  /**
   * The separator is made of two lines. The top line will be 
   * the Metal theme color separatorForeground (or left line if it's vertical).
   * The bottom or right line will be the Metal theme color
   * separatorBackground.
   * The two lines will 
   * be centered inside the bounds box. If the separator is horizontal, 
   * then it will be vertically centered, or if it's vertical, it will 
   * be horizontally centered.
   *
   * @param g The Graphics object to paint with
   * @param c The JComponent to paint.
   */
  public void paint(Graphics g, JComponent c)
  {
    Rectangle r = new Rectangle();
    SwingUtilities.calculateInnerArea(c, r);
    Color saved = g.getColor();
    Color c1 = UIManager.getColor("Separator.foreground");
    Color c2 = UIManager.getColor("Separator.background");
    JSeparator s;
    if (c instanceof JSeparator)
      s = (JSeparator) c;
    else
      return;
      
    if (s.getOrientation() == JSeparator.HORIZONTAL)
      {    
        int midAB = r.height / 2;
        g.setColor(c1);
        g.drawLine(r.x, r.y + midAB - 1, r.x + r.width, r.y + midAB - 1);

        g.setColor(c2);
        g.fillRect(r.x, r.y + midAB, r.x + r.width, r.y + midAB);
      }
      else
      {
        int midAD = r.height / 2 + r.y;
        g.setColor(c1);
        g.drawLine(r.x, r.y, r.x, r.y + r.height);

        g.setColor(c2);
        g.fillRect(r.x + midAD, r.y + r.height, r.x + midAD, r.y + r.height);
      }
    g.setColor(saved);
  }
}
