/* MetalUtils.java
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
import java.awt.Graphics2D;
import java.awt.TexturePaint;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

/**
 * Some utility and helper methods for the Metal Look &amp; Feel.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
class MetalUtils
{

  /**
   * The typical metal pattern for use with Graphics2D.
   */
  static BufferedImage pattern2D;

  /**
   * The light color to draw the pattern.
   */
  static Color lightColor;

  /**
   * The dark color to draw to draw the pattern.
   */
  static Color darkColor;

  /**
   * Fills a rectangle with the typical Metal pattern.
   *
   * @param g the <code>Graphics</code> context to use
   * @param x the X coordinate of the upper left corner of the rectangle to
   *     fill
   * @param y the Y coordinate of the upper left corner of the rectangle to
   *     fill
   * @param w the width of the rectangle to fill
   * @param h the height of the rectangle to fill
   * @param light the light color to use
   * @param dark the dark color to use
   */
  static void fillMetalPattern(Component c, Graphics g, int x, int y, int w, int h,
                                Color light, Color dark)
  {
    if (g instanceof Graphics2D)
      fillMetalPattern2D((Graphics2D) g, x, y, w, h, light, dark);
    else
      {
        int xOff = 0;
        for (int mY = y; mY < (y + h); mY++)
          {
            // set color alternating with every line
            if (((mY - y) % 2) == 0)
              g.setColor(light);
            else
              g.setColor(dark);

            for (int mX = x + (xOff); mX < (x + w); mX += 4)
              {
                g.drawLine(mX, mY, mX, mY);
              }

            // increase x offset
            xOff++;
            if (xOff > 3)
              xOff = 0;
          }
        }
  }

  /**
   * Fills a rectangle with the typical Metal pattern using Java2D.
   *
   * @param g2d the <code>Graphics2D</code> context to use
   * @param x the X coordinate of the upper left corner of the rectangle to
   *     fill
   * @param y the Y coordinate of the upper left corner of the rectangle to
   *     fill
   * @param w the width of the rectangle to fill
   * @param h the height of the rectangle to fill
   */
  static void fillMetalPattern2D(Graphics2D g2d,  int x, int y, int w, int h,
                                 Color light, Color dark)
  {
    if (pattern2D == null || !darkColor.equals(dark) || !lightColor.equals(light))
      initializePattern(light, dark);

    // Prepare the texture.
    TexturePaint texture =
      new TexturePaint(pattern2D, new Rectangle2D.Double(0., 0., 4., 4.));
    g2d.setPaint(texture);
    g2d.fillRect(x, y, w, h);
  }

  /**
   * Initializes the pattern image.
   */
  static void initializePattern(Color light, Color dark)
  {
    pattern2D = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB);
    lightColor = light;
    darkColor = dark;
    Graphics g = pattern2D.getGraphics();
    g.setColor(light);
    g.fillRect(0, 0, 1, 1);
    g.fillRect(2, 2, 1, 1);
    g.setColor(dark);
    g.fillRect(1, 1, 1, 1);
    g.fillRect(3, 3, 1, 1);
    g.dispose();
  }
}
