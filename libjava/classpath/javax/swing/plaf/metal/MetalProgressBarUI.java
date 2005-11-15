/* MetalProgressBarUI.java
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
import java.awt.Insets;

import javax.swing.JComponent;
import javax.swing.JProgressBar;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicProgressBarUI;

/**
 * A UI delegate for the {@link JProgressBar} component.
 */
public class MetalProgressBarUI extends BasicProgressBarUI
{  
  /**
   * Constructs a new instance of <code>MetalProgressBarUI</code>.
   */
  public MetalProgressBarUI()
  {
    super();
  }

  /**
   * Returns a new instance of <code>MetalProgressBarUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A new instance of <code>MetalProgressBarUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalProgressBarUI();
  }

  /**
   * Performs the painting for determinate progress bars. This calls the
   * superclass behaviour and then adds some highlighting to the upper and left
   * edge of the progress bar.
   *
   * @param g the graphics context
   * @param c not used here
   */
  public void paintDeterminate(Graphics g, JComponent c)
  {
    super.paintDeterminate(g, c);
    Color saved = g.getColor();
    Insets i = progressBar.getInsets();
    int w = progressBar.getWidth();
    int h = progressBar.getHeight();
    int orientation = progressBar.getOrientation();
    
    Color shadow = MetalLookAndFeel.getControlShadow();
    g.setColor(shadow);

    g.drawLine(i.left, i.top, w - i.right, i.top);
    g.drawLine(i.left, i.top, i.left, h - i.bottom);
    int full = getAmountFull(i, w, h);
    if (full > 0)
      {
        Color darkShadow = MetalLookAndFeel.getPrimaryControlDarkShadow();
        g.setColor(darkShadow);
        if (orientation == JProgressBar.HORIZONTAL)
          {
            g.drawLine(i.left, i.top, i.left, h - i.bottom);
            g.drawLine(i.left, i.top, i.left + full - 1, i.top);
          }
        else
          {
            if (full >= (h - i.top - i.bottom))
              g.drawLine(i.left, i.top, w - i.right, i.top);
            g.drawLine(i.left, h - i.bottom, i.left, h - i.bottom - full);
          }
      }
    g.setColor(saved);
  }

  /**
   * Performs the painting for indeterminate progress bars. This calls the
   * superclass behaviour and then adds some highlighting to the upper and left
   * edge of the progress bar.
   *
   * @param g the graphics context
   * @param c not used here
   */
  public void paintIndeterminate(Graphics g, JComponent c)
  {
    super.paintIndeterminate(g, c);
    Color saved = g.getColor();
    Insets i = progressBar.getInsets();
    int w = progressBar.getWidth();
    int h = progressBar.getHeight();
    Color shadow = MetalLookAndFeel.getControlShadow();
    g.setColor(shadow);
    g.drawLine(i.left, i.top, w - i.right, i.top);
    g.drawLine(i.left, i.top, i.left, h - i.bottom);

    boxRect = getBox(boxRect);
    Color darkShadow = MetalLookAndFeel.getPrimaryControlDarkShadow();
    g.setColor(darkShadow);
    int orientation = progressBar.getOrientation();
    if (orientation == JProgressBar.HORIZONTAL)
      g.drawLine(boxRect.x, i.top, boxRect.x + boxRect.width - 1, i.top);
    else
      g.drawLine(i.left, boxRect.y, i.left, boxRect.y + boxRect.height - 1);
    g.setColor(saved);
  }
}
