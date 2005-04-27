/* MetalScrollBarUI.java
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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.HashMap;

import javax.swing.JComponent;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollBarUI;

public class MetalScrollBarUI
  extends BasicScrollBarUI
{

  /** The minimum thumb size */
  private static final Dimension MIN_THUMB_SIZE = new Dimension(18, 18);

  // FIXME: maybe replace by a Map of instances when this becomes stateful
  /** The shared UI instance for JScrollBars. */
  private static HashMap instances = null;

  /**
   * Constructs a new instance of MetalScrollBarUI.
   */
  public MetalScrollBarUI()
  {
    super();
  }

  /**
   * Returns an instance of MetalScrollBarUI.
   *
   * @param component the component for which we return an UI instance
   *
   * @return an instance of MetalScrollBarUI
   */
  public static ComponentUI createUI(JComponent component)
  {
    if (instances == null)
      instances = new HashMap();

    Object o = instances.get(component);
    MetalScrollBarUI instance;
    if (o == null)
      {
	instance = new MetalScrollBarUI();
	instances.put(component, instance);
      }
    else
      instance = (MetalScrollBarUI) o;

    return instance;
  }

  /**
   * Paints the slider button of the ScrollBar.
   *
   * @param g the Graphics context to use
   * @param c the JComponent on which we paint
   * @param thumbBounds the rectangle that is the slider button
   */
  protected void paintThumb(Graphics g, JComponent c, Rectangle thumbBounds)
  {
    // first we fill the background
    g.setColor(thumbColor);
    g.fillRect(thumbBounds.x, thumbBounds.y, thumbBounds.width,
               thumbBounds.height);

    // draw the outer dark line
    g.setColor(thumbDarkShadowColor);
    g.drawRect(thumbBounds.x, thumbBounds.y, thumbBounds.width - 1,
               thumbBounds.height - 1);

    // draw the inner light line
    g.setColor(thumbHighlightColor);
    g.drawLine(thumbBounds.x + 1, thumbBounds.y + 1,
               thumbBounds.x + thumbBounds.width - 2,
               thumbBounds.y + 1);
    g.drawLine(thumbBounds.x + 1, thumbBounds.y + 1,
               thumbBounds.x + 1,
               thumbBounds.y + thumbBounds.height - 2);

    // draw the shadow line
    UIDefaults def = UIManager.getLookAndFeelDefaults();
    g.setColor(def.getColor("ScrollBar.shadow"));
    g.drawLine(thumbBounds.x + 1, thumbBounds.y + thumbBounds.height,
               thumbBounds.x + thumbBounds.width,
               thumbBounds.y + thumbBounds.height);

    // draw the pattern
    int xOff = 0;
    for (int y = thumbBounds.y + 4;
         y < (thumbBounds.y + thumbBounds.height - 4); y++)
      {
        // set color alternating with every line
        if ((y % 2) == 0)
          g.setColor(thumbHighlightColor);
        else
          g.setColor(thumbDarkShadowColor);

        for (int x = thumbBounds.x + 3 + (xOff);
             x < (thumbBounds.x + thumbBounds.width - 3); x = x + 4)
          {
            g.drawLine(x, y, x, y);
          }

        // increase x offset
        xOff++;
        if (xOff > 3)
          xOff = 0;

      }
  }

  /**
   * This method returns the minimum thumb size.
   *
   * @return The minimum thumb size.
   */
  protected Dimension getMinimumThumbSize()
  {
    return MIN_THUMB_SIZE;
  }
}
