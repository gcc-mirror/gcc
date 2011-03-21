/* LineBorder.java --
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package javax.swing.border;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;


/**
 * A border that consists of a line whose thickness and color can be
 * specified. There also is a variant with rounded corners.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class LineBorder extends AbstractBorder
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = -787563427772288970L;


  /**
   * A shared instance of a black, one pixel thick, plain LineBorder.
   * The singleton object is lazily created by {@link
   * #createBlackLineBorder()} upon its first invocation.
   */
  private static LineBorder blackLineBorder;


  /**
   * A shared instance of a gray, one pixel thick, plain LineBorder.
   * The singleton object is lazily created by {@link
   * #createGrayLineBorder()} upon its first invocation.
   */
  private static LineBorder grayLineBorder;


  /**
   * The width of the line in pixels.
   */
  protected int thickness;


  /**
   * The color of the line.
   */
  protected Color lineColor;


  /**
   * Indicates whether the line is drawn with rounded corners
   * (<code>true</code>) or not ((<code>false</code>).
   */
  protected boolean roundedCorners;


  /**
   * Constructs a LineBorder given its color.  The border will be one
   * pixel thick and have plain corners.
   *
   * @param color the color for drawing the border.
   *
   * @see #LineBorder(java.awt.Color, int, boolean)
   */
  public LineBorder(Color color)
  {
    this(color, /* thickness */ 1, /* roundedCorners */ false);
  }


  /**
   * Constructs a LineBorder given its color and thickness.  The
   * border will have plain corners.
   *
   * @param color the color for drawing the border.
   * @param thickness the width of the line in pixels.
   *
   * @see #LineBorder(java.awt.Color, int, boolean)
   */
  public LineBorder(Color color, int thickness)
  {
    this (color, thickness, /* roundedCorners */ false);
  }


  /**
   * Constructs a LineBorder given its color, thickness, and whether
   * it has rounded corners.
   *
   * <p><img src="doc-files/LineBorder-1.png" width="500" height="200"
   * alt="[An illustration of two LineBorders]" />
   *
   * <p>Note that the enlarged view in the right-hand picture shows
   * that the implementation draws one more pixel than specified,
   * provided that <code>roundedCorders</code> is <code>true</code>
   * and anti-aliasing is turned on while painting. While this might
   * be considered a bug, the Sun reference implementation (at least
   * JDK 1.3.1 on Apple MacOS X 10.1.5) can be observed to fill
   * exactly the same pixels as shown above. The GNU Classpath
   * LineBorder replicates the observed behavior of the Sun
   * implementation.
   *
   * @param color the color for drawing the border.
   * @param thickness the width of the line in pixels.
   * @param roundedCorners <code>true</code> for rounded corners,
   *        <code>false</code> for plain corners.
   *
   * @since 1.3
   */
  // For the bug mentioned in the JavaDoc, please see also the comment
  // in the paintBorder method below.
  //
  public LineBorder(Color color, int thickness, boolean roundedCorners)
  {
    if ((color == null) || (thickness < 0))
      throw new IllegalArgumentException();

    this.lineColor = color;
    this.thickness = thickness;
    this.roundedCorners = roundedCorners;
  }


  /**
   * Returns a black, one pixel thick, plain {@link LineBorder}. The method
   * may always return the same (singleton) {@link LineBorder} instance.
   *
   * @return The border.
   */
  public static Border createBlackLineBorder()
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (blackLineBorder == null)
      blackLineBorder = new LineBorder(Color.black);

    return blackLineBorder;
  }


  /**
   * Returns a gray, one pixel thick, plain {@link LineBorder}. The method
   * may always return the same (singleton) {@link LineBorder} instance.
   *
   * @return The border.
   */
  public static Border createGrayLineBorder()
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (grayLineBorder == null)
      grayLineBorder = new LineBorder(Color.gray);

    return grayLineBorder;
  }


  /**
   * Paints the line border around a given Component.
   *
   * @param c the component whose border is to be painted.
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   */
  public void paintBorder(Component c, Graphics  g,
                          int x, int y, int width, int height)
  {
    Color oldColor = g.getColor();

    try
    {
      g.setColor(lineColor);

      // If width and height were not adjusted, the border would
      // appear one pixel too large in both directions.
      width -= 1;
      height -= 1;

      // Blurred, too large appearance
      // -----------------------------
      // While Java 2D has introduced line strokes of arbitrary width,
      // it seems desirable to keep this code independent of Java 2D.
      // Therefore, multiple nested rectangles (or rounded rectangles)
      // are drawn in order to simulate a line whose thickness is
      // greater than one pixel.
      //
      // This hack causes a blurred appearance when anti-aliasing is
      // on. Interestingly enough, though, the Sun JDK 1.3.1 (at least
      // on MacOS X 10.1.5) shows exactly the same appearance under
      // this condition. It thus seems likely that Sun does the same
      // hack for simulating thick lines.  For this reason, the
      // blurred appearance seems acceptable -- especially since GNU
      // Classpath tries to be compatible with the Sun reference
      // implementation.
      for (int i = 0; i < thickness; i++)
      {
        if (roundedCorners)
          g.drawRoundRect(x, y, width, height, thickness, thickness);
        else
          g.drawRect(x, y, width, height);

        x += 1;
        y += 1;
        width -= 2;
        height -= 2;
      }
    }
    finally
    {
      g.setColor(oldColor);
    }
  }


  /**
   * Measures the width of this border.
   *
   * @param c the component whose border is to be measured.
   *
   * @return an Insets object whose <code>left</code>, <code>right</code>,
   *         <code>top</code> and <code>bottom</code> fields indicate the
   *         width of the border at the respective edge, which is the
   *         thickness of the line.
   *
   * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
   */
  public Insets getBorderInsets(Component c)
  {
    return new Insets(thickness, thickness, thickness, thickness);
  }


  /**
   * Measures the width of this border, storing the results into a
   * pre-existing Insets object.
   *
   * @param insets an Insets object for holding the result values.
   *        After invoking this method, the <code>left</code>,
   *        <code>right</code>, <code>top</code> and
   *        <code>bottom</code> fields indicate the width of the
   *        border at the respective edge, which is the thickness
   *        of the line.
   *
   * @return the same object that was passed for <code>insets</code>.
   *
   * @see #getBorderInsets(Component)
   */
  public Insets getBorderInsets(Component c, Insets insets)
  {
    insets.left = insets.right = insets.top = insets.bottom = thickness;
    return insets;
  }


  /**
   * Returns the color of the line.
   *
   * @return The line color (never <code>null</code>).
   */
  public Color getLineColor()
  {
    return lineColor;
  }


  /**
   * Returns the thickness of the line in pixels.
   *
   * @return The line thickness (in pixels).
   */
  public int getThickness()
  {
    return thickness;
  }


  /**
   * Returns whether this LineBorder os drawm with rounded
   * or with plain corners.
   *
   * @return <code>true</code> if the corners are rounded,
   *         <code>false</code> if the corners are plain.
   */
  public boolean getRoundedCorners()
  {
    return roundedCorners;
  }


  /**
   * Determines whether this border fills every pixel in its area
   * when painting.
   *
   * @return <code>true</code> if the corners are plain and the line
   *         color is fully opaque; <code>false</code> if the corners
   *         are rounded or the line color is partially transparent.
   */
  public boolean isBorderOpaque()
  {
    return (!roundedCorners) && (lineColor.getAlpha() == 255);
  }
}
