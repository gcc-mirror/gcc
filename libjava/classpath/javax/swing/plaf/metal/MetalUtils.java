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

import gnu.classpath.SystemProperties;

import java.awt.Color;
import java.awt.Component;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.TexturePaint;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.List;

import javax.swing.SwingConstants;
import javax.swing.UIManager;

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
    if (g instanceof Graphics2D
      && SystemProperties.getProperty("gnu.javax.swing.noGraphics2D") == null)
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

            for (int mX = x + xOff; mX < (x + w); mX += 4)
              {
                g.fillRect(mX, mY, 1, 1);
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

  /**
   * Paints the typical Metal gradient. See {@link #paintGradient(Graphics,
   * int, int, int, int, float, float, Color, Color, Color, int, int[][])}
   * for more details.
   *
   * This variant paints a gradient without a mask.
   *
   * @param g the graphics context to use
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param dir the direction of the gradient, either
   * @param uiProp the key of the UIManager property that has the parameters
   */
  static void paintGradient(Graphics g, int x, int y, int w, int h,
                            int dir, String uiProp)
  {
    paintGradient(g, x, y, w, h, dir, uiProp, null);
  }
  
  /**
   * Paints the typical Metal gradient. See {@link #paintGradient(Graphics,
   * int, int, int, int, float, float, Color, Color, Color, int, int[][])}
   * for more details.
   *
   * The parameters are fetched from the UIManager using the key
   * <code>uiProp</code>. The value is expected to be a {@link List} that
   * contains 4 values: two {@link Double}s and 3 {@link Color} object that
   * together make up the parameters passed to the painting method.
   *
   * @param g the graphics context to use
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param dir the direction of the gradient, either
   * @param uiProp the key of the UIManager property that has the parameters
   * @param mask the mask that should be used when painting the gradient as
   *        described above
   */
  static void paintGradient(Graphics g, int x, int y, int w, int h,
                            int dir, String uiProp, int[][] mask)
  {
    List params = (List) UIManager.get(uiProp);
    float g1 = ((Float) params.get(0)).floatValue();
    float g2 = ((Float) params.get(1)).floatValue();
    Color c1 = (Color) params.get(2);
    Color c2 = (Color) params.get(3);
    Color c3 = (Color) params.get(4);
    paintGradient(g, x, y, w, h, g1, g2, c1, c2, c3, dir, mask);
  }

  /**
   * Paints the typical Metal gradient. The gradient is painted as follows:
   * <pre>
   * 
   * +-------+--------+--------+-----------------------------+
   * |       |        |        |                             |
   * +-------+--------+--------+-----------------------------+
   * c1  ->  c2  --   c2  ->   c1         -------->          c3
   * < -g1- > < -g2- > < -g1- >
   * </pre>
   * 
   * There are 4 distinct areas in this gradient:
   * <ol>
   * <li>A gradient from color 1 to color 2 with the relative width specified
   *   by <code>g1</code></li>
   * <li>A solid area with the color 2 and the relative width specified by
   *  <code>g2</code></li>
   * <li>A gradient from color 2 to color 1 with the relative width specified
   *   by <code>g1</code></li>
   *
   * The <code>mask</code> parameter is an array if int arrays, where the first
   * index specifies the row (in the gradient direction), and the second index
   * is the starting and end offset of that line. This way you can specify a
   * mask that should be laid over the gradient for paintint non-rectangular
   * gradients. The following example should demonstrate this for painting
   * a circular shaped gradient (note that the first and last line should not
   * be drawn at all, they are only here to show the circular shape more
   * clearly). Everything <em>inside</code> the surrounded area is filled by
   * the gradient:
   *
   * <pre>
   *     012345678
   *         xxx
   * 0      x   x         { {4, 7},
   * 1     x     x          {3, 8},
   * 2     x     x          {3, 8},
   * 3     x     x          {3, 8},
   * 4      x   x           {4, 7} }
   *         xxx
   * </pre>
   *
   * The <code>mask</code> array is expected to have <code>w</code> or
   * <code>h</code> array elements, depending on the direction.
   *
   * If the <code>mask</code> parameter is null, then the gradient is painted
   * without a mask.
   *
   * @param g the graphics context to use
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param g1 the relative width of the c1->c2 gradients
   * @param g2 the relative width of the c2 solid area
   * @param c1 the color 1
   * @param c2 the color 2
   * @param c3 the color 3
   * @param dir the direction of the gradient, either
   *        {@link SwingConstants#HORIZONTAL} or {@link SwingConstants#VERTICAL}
   * @param mask the mask that should be used when painting the gradient as
   *        described above
   */
  static void paintGradient(Graphics g, int x, int y, int w, int h, float g1,
                            float g2, Color c1, Color c2, Color c3, int dir,
                            int[][] mask)
  {
    if (dir == SwingConstants.HORIZONTAL)
      paintHorizontalGradient(g, x, y, w, h, g1, g2, c1, c2, c3, mask);
    else
      paintVerticalGradient(g, x, y, w, h, g1, g2, c1, c2, c3, mask);
  }

  /**
   * Paints a horizontal gradient. See {@link #paintGradient(Graphics, int,
   * int, int, int, float, float, Color, Color, Color, int, int[][])}
   * for details.
   *
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param g1 the relative width of the c1->c2 gradients
   * @param g2 the relative width of the c2 solid area
   * @param c1 the color 1
   * @param c2 the color 2
   * @param c3 the color 3
   * @param mask the mask that should be used when painting the gradient as
   *        described above
   */
  static void paintHorizontalGradient(Graphics g, int x, int y, int w, int h,
                                      float g1, float g2, Color c1, Color c2,
                                      Color c3, int[][] mask)
  {
    
    if (g instanceof Graphics2D
        && SystemProperties.getProperty("gnu.javax.swing.noGraphics2D") == null)
      {
        paintHorizontalGradient2D((Graphics2D) g, x, y, w, h, g1, g2, c1, c2,
                                  c3, mask);
        return;
      }

    // Calculate the coordinates.
    int y0 = y;
    int y1 = y + h;
    // The size of the first gradient area (c1->2).
    int w1 = (int) (w * g1);
    // The size of the solid c2 area.
    int w2 = (int) (w * g2);
    int x0 = x;
    int x1 = x0 + w1;
    int x2 = x1 + w2;
    int x3 = x2 + w1;
    int x4 = x + w;

    // Paint first gradient area (c1->c2).
    int xc; // The current y coordinate.
    for (xc = x0; xc < x1; xc++)
      {
        if (xc > x + w)
          break;

        // Perform color interpolation;
        double factor = (xc - x0) / (double) w1;
        int rInt = (int) ((c2.getRed() - c1.getRed()) * factor + c1.getRed());
        int gInt = (int) ((c2.getGreen() - c1.getGreen()) * factor
            + c1.getGreen());
        int bInt = (int) ((c2.getBlue() - c1.getBlue()) * factor
            + c1.getBlue());
        Color interpolated = new Color(rInt, gInt, bInt);
        g.setColor(interpolated);
        if (mask != null)
          {
            y0 = mask[xc - x0][0] + y;
            y1 = mask[xc - x0][1] + y;
          }
        g.fillRect(xc, y0, 1, y1 - y0);
      }
    // Paint solid c2 area.
    g.setColor(c2);
    if (mask == null)
      {
        g.fillRect(x1, y, x2 - x1, h);
      }
    else
      {
        for (xc = x1; xc < x2; xc++)
          {
            y0 = mask[xc - x0][0] + y;
            y1 = mask[xc - x0][1] + y;
            g.fillRect(xc, y0, 1, y1 - y0);
          }
      }

    // Paint second gradient area (c2->c1).
    for (xc = x2; xc < x3; xc++)
      {
        if (xc > x + w)
          break;

        // Perform color interpolation;
        double factor = (xc - x2) / (double) w1;
        int rInt = (int) ((c1.getRed() - c2.getRed()) * factor + c2.getRed());
        int gInt = (int) ((c1.getGreen() - c2.getGreen()) * factor
            + c2.getGreen());
        int bInt = (int) ((c1.getBlue() - c2.getBlue()) * factor
            + c2.getBlue());
        Color interpolated = new Color(rInt, gInt, bInt);
        g.setColor(interpolated);
        if (mask != null)
          {
            y0 = mask[xc - x0][0] + y;
            y1 = mask[xc - x0][1] + y;
          }
        g.fillRect(xc, y0, 1, y1 - y0);
      }

    // Paint third gradient area (c1->c3).
    for (xc = x3; xc < x4; xc++)
      {
        if (xc > x + w)
          break;

        // Perform color interpolation;
        double factor = (xc - x3) / (double) (x4 - x3);
        int rInt = (int) ((c3.getRed() - c1.getRed()) * factor + c1.getRed());
        int gInt = (int) ((c3.getGreen() - c1.getGreen()) * factor
            + c1.getGreen());
        int bInt = (int) ((c3.getBlue() - c1.getBlue()) * factor
            + c1.getBlue());
        Color interpolated = new Color(rInt, gInt, bInt);
        g.setColor(interpolated);
        if (mask != null)
          {
            y0 = mask[xc - x0][0] + y;
            y1 = mask[xc - x0][1] + y;
          }
        g.drawLine(xc, y0, xc, y1);
      }
  }

  /**
   * Paints a vertical gradient. See {@link #paintGradient(Graphics, int, int,
   * int, int, float, float, Color, Color, Color, int, int[][])} for details.
   *
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param g1 the relative width of the c1->c2 gradients
   * @param g2 the relative width of the c2 solid area
   * @param c1 the color 1
   * @param c2 the color 2
   * @param c3 the color 3
   * @param mask the mask that should be used when painting the gradient as
   *        described above
   */
  static void paintVerticalGradient(Graphics g, int x, int y, int w, int h,
                                    float g1, float g2, Color c1, Color c2,
                                    Color c3, int[][] mask)
  {
    if (g instanceof Graphics2D
        && SystemProperties.getProperty("gnu.javax.swing.noGraphics2D") == null)
       {
         paintVerticalGradient2D((Graphics2D) g, x, y, w, h, g1, g2, c1, c2,
                                   c3, mask);
         return;
       }

    // Calculate the coordinates.
    int x0 = x;
    int x1 = x + w;
    // The size of the first gradient area (c1->2).
    int w1 = (int) (h * g1);
    // The size of the solid c2 area.
    int w2 = (int) (h * g2);
    int y0 = y;
    int y1 = y0 + w1;
    int y2 = y1 + w2;
    int y3 = y2 + w1;
    int y4 = y + h;

    // Paint first gradient area (c1->c2).
    int yc; // The current y coordinate.
    for (yc = y0; yc < y1; yc++)
      {
        if (yc > y + h)
          break;

        // Perform color interpolation;
        double factor = (yc - y0) / (double) w1;
        int rInt = (int) ((c2.getRed() - c1.getRed()) * factor + c1.getRed());
        int gInt = (int) ((c2.getGreen() - c1.getGreen()) * factor
            + c1.getGreen());
        int bInt = (int) ((c2.getBlue() - c1.getBlue()) * factor
            + c1.getBlue());
        Color interpolated = new Color(rInt, gInt, bInt);
        g.setColor(interpolated);
        if (mask != null)
          {
            x0 = mask[yc - y0][0] + x;
            x1 = mask[yc - y0][1] + x;
          }
        g.fillRect(x0, yc, x1 - x0, 1);
      }
    // Paint solid c2 area.
    g.setColor(c2);
    if (mask == null)
      {
        g.fillRect(x, y1, w, y2 - y1);
      }
    else
      {
        for (yc = y1; yc < y2; yc++)
          {
            x0 = mask[yc - y0][0] + x;
            x1 = mask[yc - y0][1] + x;
            g.fillRect(x0, yc, x1 - x0, 1);
          }
      }

    // Paint second gradient area (c2->c1).
    for (yc = y2; yc < y3; yc++)
      {
        if (yc > y + h)
          break;

        // Perform color interpolation;
        double factor = (yc - y2) / (double) w1;
        int rInt = (int) ((c1.getRed() - c2.getRed()) * factor + c2.getRed());
        int gInt = (int) ((c1.getGreen() - c2.getGreen()) * factor
            + c2.getGreen());
        int bInt = (int) ((c1.getBlue() - c2.getBlue()) * factor
            + c2.getBlue());
        Color interpolated = new Color(rInt, gInt, bInt);
        g.setColor(interpolated);
        if (mask != null)
          {
            x0 = mask[yc - y0][0] + x;
            x1 = mask[yc - y0][1] + x;
          }
        g.fillRect(x0, yc, x1 - x0, 1);
      }

    // Paint third gradient area (c1->c3).
    for (yc = y3; yc < y4; yc++)
      {
        if (yc > y + h)
          break;

        // Perform color interpolation;
        double factor = (yc - y3) / (double) (y4 - y3);
        int rInt = (int) ((c3.getRed() - c1.getRed()) * factor + c1.getRed());
        int gInt = (int) ((c3.getGreen() - c1.getGreen()) * factor
            + c1.getGreen());
        int bInt = (int) ((c3.getBlue() - c1.getBlue()) * factor
            + c1.getBlue());
        Color interpolated = new Color(rInt, gInt, bInt);
        g.setColor(interpolated);
        if (mask != null)
          {
            x0 = mask[yc - y0][0] + x;
            x1 = mask[yc - y0][1] + x;
          }
        g.fillRect(x0, yc, x1 - x0, 1);
      }
  }

  /**
   * Paints a horizontal gradient using Graphics2D functionality.
   *
   * @param g the Graphics2D instance
   * @param x the X coordinate of the upper left corner of the rectangle
   * @param y the Y coordinate of the upper left corner of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param g1 the relative width of the c1->c2 gradients
   * @param g2 the relative width of the c2 solid area
   * @param c1 the color 1
   * @param c2 the color 2
   * @param c3 the color 3
   * @param mask the mask that should be used when painting the gradient as
   *        described above
   */
  private static void paintHorizontalGradient2D(Graphics2D g, int x, int y,
                                                int w, int h, float g1,
                                                float g2, Color c1,
                                                Color c2, Color c3,
                                                int[][] mask)
  {
    // FIXME: Handle the mask somehow, or do Graphics2D clipping instead.
    GradientPaint p1 = new GradientPaint(x, y, c1, x + w * g1, y, c2);
    g.setPaint(p1);
    // This fills the first gradient and the solid area in one go.
    g.fillRect(x, y, (int) (w * (g1 + g2)), h);

    GradientPaint p2 = new GradientPaint(x + (w * (g1 + g2)), y, c2, x + w, y,
                                         c3);
    g.setPaint(p2);
    g.fillRect((int) (x + (w * (g1 + g2))), y,
               (int) (w * (1. - (g1 + g2))), h);
  }

  private static void paintVerticalGradient2D(Graphics2D g, int x, int y,
                                              int w, int h, float g1,
                                              float g2, Color c1,
                                              Color c2, Color c3,
                                              int[][] mask)
  {
    // FIXME: Handle the mask somehow, or do Graphics2D clipping instead.
    GradientPaint p1 = new GradientPaint(x, y, c1, x, y + h * g1, c2);
    g.setPaint(p1);
    // This fills the first gradient and the solid area in one go.
    g.fillRect(x, y, w, (int) (h * (g1 + g2)));

    GradientPaint p2 = new GradientPaint(x, y + (h * (g1 + g2)), c2, x, y + h,
                                         c3);
    g.setPaint(p2);
    g.fillRect(x, (int) (y + (h * (g1 + g2))), w,
               (int) (h * (1. - (g1 + g2))));
  }
}
