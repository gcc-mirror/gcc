/* SoftBevelBorder.java -- 
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
 * A rectangular, three pixel thick border that looks like a BevelBorder
 * with slightly softened corners.
 *
 * <p>Like BevelBorder, SoftBevelBorder has a highlight and a shadow
 * color. In the raised variant, the highlight color is used for the
 * top and left edges, and the shadow color is used for the bottom and
 * right edge.  In the lowered variant, color usage is reversed.  For
 * an image, see the documentation of the individual constructors.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class SoftBevelBorder
  extends BevelBorder
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Sun JDK 1.4.1_01 on GNU/Linux 2.4.20. Interestingly,
   * the Apple/Sun JDK 1.3.1 on MacOS X 10.1.5 gives a different
   * value, namely -6658357140774549493L.
   */
  static final long serialVersionUID = 5248789787305979975L;


  /**
   * Constructs a SoftBevelBorder whose colors will be derived from the
   * background of the enclosed component. The background color is
   * retrieved each time the border is painted, so a SoftBevelBorder
   * constructed by this method will automatically reflect a change
   * to the component&#x2019;s background color.
   *
   * <p><img src="doc-files/SoftBevelBorder-1.png" width="500" height="200"
   * alt="[An illustration showing raised and lowered SoftBevelBorders]" />
   *
   * @param bevelType the desired appearance of the border. The value
   *        must be either {@link BevelBorder#RAISED}
   *        or {@link BevelBorder#LOWERED}.
   *
   * @throws IllegalArgumentException if <code>bevelType</code> has
   *         an unsupported value.
   */
  public SoftBevelBorder(int bevelType)
  {
    super(bevelType);
  }


  /**
   * Constructs a SoftBevelBorder given its appearance type and two
   * colors for its highlight and shadow.
   *
   * <p><img src="doc-files/SoftBevelBorder-2.png" width="500" height="150"
   * alt="[An illustration showing SoftBevelBorders that were
   * constructed with this method]" />
   *
   * @param bevelType the desired appearance of the border. The value
   *        must be either {@link BevelBorder#RAISED} or {@link
   *        BevelBorder#LOWERED}.
   *
   * @param highlight the color that will be used for the inner side
   *        of the highlighted edges (top and left if if
   *        <code>bevelType</code> is {@link BevelBorder#RAISED};
   *        bottom and right otherwise). The color for the outer side
   *        is a brightened version of this color.
   *
   * @param shadow the color that will be used for the outer side of
   *        the shadowed edges (bottom and right if
   *        <code>bevelType</code> is {@link BevelBorder#RAISED}; top
   *        and left otherwise). The color for the inner side is a
   *        brightened version of this color.
   *
   * @throws IllegalArgumentException if <code>bevelType</code> has an
   *        unsupported value.
   *
   * @throws NullPointerException if <code>highlight</code> or
   *         <code>shadow</code> is <code>null</code>.
   *
   * @see java.awt.Color#brighter()
   */
  public SoftBevelBorder(int bevelType, Color highlight, Color shadow)
  {
    this(bevelType,
         /* highlightOuter */ highlight.brighter(),
         /* highlightInner */ highlight,
         /* shadowOuter */    shadow,
         /* shadowInner */    shadow.brighter());
  }


  /**
   * Constructs a SoftBevelBorder given its appearance type and all
   * colors.
   *
   * <p><img src="doc-files/SoftBevelBorder-3.png" width="500" height="150"
   * alt="[An illustration showing SoftBevelBorders that were
   * constructed with this method]" />
   *
   * @param bevelType the desired appearance of the border. The value
   *        must be either {@link BevelBorder#RAISED} or {@link
   *        BevelBorder#LOWERED}.
   *
   * @param highlightOuter the color that will be used for the outer
   *        side of the highlighted edges (top and left if
   *        <code>bevelType</code> is {@link BevelBorder#RAISED};
   *        bottom and right otherwise).
   *
   * @param highlightInner the color that will be used for the inner
   *        side of the highlighted edges.
   *
   * @param shadowOuter the color that will be used for the outer side
   *        of the shadowed edges (bottom and right if
   *        <code>bevelType</code> is {@link BevelBorder#RAISED}; top
   *        and left otherwise).
   *
   * @param shadowInner the color that will be used for the inner
   *        side of the shadowed edges.
   *
   * @throws IllegalArgumentException if <code>bevelType</code> has
   *         an unsupported value.
   *
   * @throws NullPointerException if one of the passed colors
   *         is <code>null</code>.
   */
  public SoftBevelBorder(int bevelType,
                         Color highlightOuter, Color highlightInner,
                         Color shadowOuter, Color shadowInner)
  {
    super(bevelType,
          highlightOuter, highlightInner,
          shadowOuter, shadowInner);
  }


  /**
   * Paints the border for a given component.
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
    switch (bevelType)
    {
    case RAISED:
      paintSoftBevel(g, x, y, width, height,
                     getHighlightOuterColor(c), getHighlightInnerColor(c),
                     getShadowInnerColor(c), getShadowOuterColor(c));
      break;

    case LOWERED:
      paintSoftBevel(g, x, y, width, height,
                     getShadowOuterColor(c), getShadowInnerColor(c),
                     getHighlightInnerColor(c), getHighlightOuterColor(c));
      break;
    }
  }


  /**
   * Measures the width of this border.
   *
   * @param c the component whose border is to be measured.
   *
   * @return an Insets object whose <code>left</code>, <code>right</code>,
   *         <code>top</code> and <code>bottom</code> fields indicate the
   *         width of the border at the respective edge.
   *
   * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
   */
  public Insets getBorderInsets(Component c)
  {
    return new Insets(3, 3, 3, 3);
  }


  /**
   * Measures the width of this border, storing the results into a
   * pre-existing Insets object.
   *
   * @param insets an Insets object for holding the result values.
   *        After invoking this method, the <code>left</code>,
   *        <code>right</code>, <code>top</code> and
   *        <code>bottom</code> fields indicate the width of the
   *        border at the respective edge.
   *
   * @return the same object that was passed for <code>insets</code>.
   *
   * @see #getBorderInsets(Component)
   */
  public Insets getBorderInsets(Component c, Insets insets)
  {
    insets.left = insets.right = insets.top = insets.bottom = 3;
    return insets;
  }

  
  /**
   * Determines whether this border fills every pixel in its area
   * when painting.
   *
   * <p>The enlarged view (see documentation for constructors) shows
   * that a SoftBevelBorder does not paint all pixels. Therefore,
   * this method always returns <code>false</code>.
   *
   * @return <code>false</code>.
   */
  public boolean isBorderOpaque()
  {
    return false;
  }


  /**
   * Paints a soft bevel in four colors.
   * 
   * <pre>
   * @@@@@@@@@@@.
   * @@.........#    @ = color a
   * @..        #    . = color b
   * @.         #    X = color c
   * ..        X#    # = color d
   * . ##########</pre>
   *
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   * @param a the color for the outer side of the top and left edges.
   * @param b the color for the inner side of the top and left edges.
   * @param c the color for the inner side of the bottom and right edges.
   * @param d the color for the outer side of the bottom and right edges.
   */
  private static void paintSoftBevel(Graphics g,
                                     int x, int y, int width, int height,
                                     Color a, Color b, Color c, Color d)
  {
    Color oldColor;

    oldColor = g.getColor();
    g.translate(x, y);
    width = width - 1;
    height = height - 1;

    try
    {
      /* To understand this code, it might be helpful to look at the
       * images that are included with the JavaDoc, especially
       * SoftBevelBorder-3.png. They are located in the "doc-files"
       * subdirectory.
       */
      g.setColor(a);
      g.drawLine(0, 0, width - 1, 0);                   // a, horizontal
      g.drawLine(0, 1, 2, 1);                           // a, horizontal
      g.drawLine(0, 2, 0, height - 1);                  // a, vertical

      g.setColor(b);
      g.drawLine(width, 0, width, 0);                   // b, horizontal
      g.drawLine(2, 1, width - 1, 1);                   // b, horizontal
      g.drawLine(1, 2, 2, 2);                           // b, horizontal
      g.drawLine(1, 3, 1, height - 1);                  // b, vertical
      g.drawLine(0, height - 1, 0, height);             // b, vertical

      g.setColor(c);
      g.drawLine(width - 1, height - 1,                 // c, one pixel
                 width - 1, height - 1);

      g.setColor(d);
      g.drawLine(2, height, width, height);             // d, horizontal
      g.drawLine(width, 2, width, height - 1);          // d, vertical
    }
    finally
    {
      g.translate(-x, -y);
      g.setColor(oldColor);
    }
  }
}

