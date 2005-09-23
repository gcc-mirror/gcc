/* BevelBorder.java -- 
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
 * A rectangular, two pixel thick border that causes the enclosed area
 * to appear as if it was raising out of or lowered into the screen. Some
 * LookAndFeels use this kind of border for rectangular buttons.
 *
 * <p>A BevelBorder has a highlight and a shadow color. In the raised
 * variant, the highlight color is used for the top and left edges,
 * and the shadow color is used for the bottom and right edge. For an
 * image, see the documentation of the individual constructors.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class BevelBorder
  extends AbstractBorder
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = -1034942243356299676L;


  /**
   * Indicates that the BevelBorder looks like if the enclosed area was
   * raising out of the screen.
   */
  public static final int RAISED = 0;


  /**
   * Indicates that the BevelBorder looks like if the enclosed area was
   * pressed into the screen.
   */
  public static final int LOWERED = 1;
  

  /**
   * The type of this BevelBorder, which is either {@link #RAISED}
   * or {@link #LOWERED}.
   */  
  protected int bevelType;


  /**
   * The outer highlight color, or <code>null</code> to indicate that
   * the color shall be derived from the background of the component
   * whose border is being painted.
   */
  protected Color highlightOuter;


  /**
   * The inner highlight color, or <code>null</code> to indicate that
   * the color shall be derived from the background of the component
   * whose border is being painted.
   */
  protected Color highlightInner;


  /**
   * The outer shadow color, or <code>null</code> to indicate that the
   * color shall be derived from the background of the component whose
   * border is being painted.
   */
  protected Color shadowOuter;


  /**
   * The inner shadow color, or <code>null</code> to indicate that the
   * color shall be derived from the background of the component whose
   * border is being painted.
   */
  protected Color shadowInner;


  /**
   * Constructs a BevelBorder whose colors will be derived from the
   * background of the enclosed component. The background color is
   * retrieved each time the border is painted, so a BevelBorder
   * constructed by this method will automatically reflect a change
   * to the component&#x2019;s background color.
   *
   * <p><img src="doc-files/BevelBorder-1.png" width="500" height="150"
   * alt="[An illustration showing raised and lowered BevelBorders]" />
   *
   * @param bevelType the desired appearance of the border. The value
   *        must be either {@link #RAISED} or {@link #LOWERED}.
   *
   * @throws IllegalArgumentException if <code>bevelType</code> has
   *         an unsupported value.
   */
  public BevelBorder(int bevelType)
  {
    if ((bevelType != RAISED) && (bevelType != LOWERED))
      throw new IllegalArgumentException();

    this.bevelType = bevelType;
  }


  /**
   * Constructs a BevelBorder given its appearance type and two colors
   * for its highlight and shadow.
   *
   * <p><img src="doc-files/BevelBorder-2.png" width="500" height="150"
   * alt="[An illustration showing BevelBorders that were constructed
   * with this method]" />
   *
   * @param bevelType the desired appearance of the border. The value
   *        must be either {@link #RAISED} or {@link #LOWERED}.
   *
   * @param highlight the color that will be used for the inner
   *        side of the highlighted edges (top and left if
   *        if <code>bevelType</code> is {@link #RAISED}; bottom
   *        and right otherwise). The color for the outer side
   *        is a brightened version of this color.
   *
   * @param shadow the color that will be used for the outer
   *        side of the shadowed edges (bottom and right
   *        if <code>bevelType</code> is {@link #RAISED}; top
   *        and left otherwise). The color for the inner side
   *        is a brightened version of this color.
   *
   * @throws IllegalArgumentException if <code>bevelType</code> has
   *         an unsupported value.
   *
   * @throws NullPointerException if <code>highlight</code> or
   *         <code>shadow</code> is <code>null</code>.
   *
   * @see java.awt.Color#brighter()
   */
  public BevelBorder(int bevelType, Color highlight, Color shadow)
  {
    this(bevelType,
         /* highlightOuter */ highlight.brighter(),
         /* highlightInner */ highlight,
         /* shadowOuter */    shadow,
         /* shadowInner */    shadow.brighter());
  }


  /**
   * Constructs a BevelBorder given its appearance type and all
   * colors.
   *
   * <p><img src="doc-files/BevelBorder-3.png" width="500" height="150"
   * alt="[An illustration showing BevelBorders that were constructed
   * with this method]" />
   *
   * @param bevelType the desired appearance of the border. The value
   *        must be either {@link #RAISED} or {@link #LOWERED}.
   *
   * @param highlightOuter the color that will be used for the outer
   *        side of the highlighted edges (top and left if
   *        <code>bevelType</code> is {@link #RAISED}; bottom and
   *        right otherwise).
   *
   * @param highlightInner the color that will be used for the inner
   *        side of the highlighted edges.
   *
   * @param shadowOuter the color that will be used for the outer
   *        side of the shadowed edges (bottom and right
   *        if <code>bevelType</code> is {@link #RAISED}; top
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
  public BevelBorder(int bevelType,
                     Color highlightOuter, Color highlightInner,
                     Color shadowOuter, Color shadowInner)
  {
    this(bevelType); // checks the validity of bevelType

    if ((highlightOuter == null) || (highlightInner == null)
        || (shadowOuter == null) || (shadowInner == null))
      throw new NullPointerException();

    this.highlightOuter = highlightOuter;
    this.highlightInner = highlightInner;
    this.shadowOuter = shadowOuter;
    this.shadowInner = shadowInner;
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
      paintRaisedBevel(c, g, x, y, width, height);
      break;

    case LOWERED:
      paintLoweredBevel(c, g, x, y, width, height);
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
    return new Insets(2, 2, 2, 2);
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
    insets.left = insets.right = insets.top = insets.bottom = 2;
    return insets;
  }

  
  /**
   * Determines the color that will be used for the outer side of
   * highlighted edges when painting the border.  If a highlight color
   * has been specified upon constructing the border, that color is
   * returned. Otherwise, the inner highlight color is brightened.
   *
   * @param c the component enclosed by this border.
   *
   * @see #getHighlightInnerColor(java.awt.Component)
   * @see java.awt.Color#brighter()
   */
  public Color getHighlightOuterColor(Component c)
  {
    if (highlightOuter != null)
      return highlightOuter;
    else
      return getHighlightInnerColor(c).brighter();
  }


  /**
   * Determines the color that will be used for the inner side of
   * highlighted edges when painting the border. If a highlight color
   * has been specified upon constructing the border, that color is
   * returned. Otherwise, the background color of the enclosed
   * component is brightened.
   *
   * @param c the component enclosed by this border.
   *
   * @see java.awt.Component#getBackground()
   * @see java.awt.Color#brighter()
   */
  public Color getHighlightInnerColor(Component c)
  {
    if (highlightInner != null)
      return highlightInner;
    else
      return c.getBackground().brighter();
  }


  /**
   * Determines the color that will be used for the inner side of
   * shadowed edges when painting the border. If a shadow color has
   * been specified upon constructing the border, that color is
   * returned. Otherwise, the background color of the enclosed
   * component is darkened.
   *
   * @param c the component enclosed by this border.
   *
   * @see java.awt.Component#getBackground()
   * @see java.awt.Color#darker()
   */
  public Color getShadowInnerColor(Component c)
  {
    if (shadowInner != null)
      return shadowInner;
    else
      return c.getBackground().darker();
  }


  /**
   * Determines the color that will be used for the outer side of
   * shadowed edges when painting the border.  If a shadow color
   * has been specified upon constructing the border, that color is
   * returned. Otherwise, the inner shadow color is darkened.
   *
   * @param c the component enclosed by this border.
   *
   * @see #getShadowInnerColor(java.awt.Component)
   * @see java.awt.Color#darker()
   */
  public Color getShadowOuterColor(Component c)
  {
    if (shadowOuter != null)
      return shadowOuter;
    else
      return getShadowInnerColor(c).darker();
  }


  /**
   * Returns the color that will be used for the outer side of
   * highlighted edges when painting the border, or <code>null</code>
   * if that color will be derived from the background of the enclosed
   * Component.
   */
  public Color getHighlightOuterColor()
  {
    return highlightOuter;
  }


  /**
   * Returns the color that will be used for the inner side of
   * highlighted edges when painting the border, or <code>null</code>
   * if that color will be derived from the background of the enclosed
   * Component.
   */
  public Color getHighlightInnerColor()
  {
    return highlightInner;
  }


  /**
   * Returns the color that will be used for the inner side of
   * shadowed edges when painting the border, or <code>null</code> if
   * that color will be derived from the background of the enclosed
   * Component.
   */
  public Color getShadowInnerColor()
  {
    return shadowInner;
  }


  /**
   * Returns the color that will be used for the outer side of
   * shadowed edges when painting the border, or <code>null</code> if
   * that color will be derived from the background of the enclosed
   * Component.
   */
  public Color getShadowOuterColor()
  {
    return shadowOuter;
  }
  
  
  /**
   * Returns the appearance of this border, which is either {@link
   * #RAISED} or {@link #LOWERED}.
   */
  public int getBevelType()
  {
    return bevelType;
  }


  /**
   * Determines whether this border fills every pixel in its area
   * when painting.
   *
   * <p>If the border colors are derived from the background color of
   * the enclosed component, the result is <code>true</code> because
   * the derivation method always returns opaque colors. Otherwise,
   * the result depends on the opacity of the individual colors.
   *
   * @return <code>true</code> if the border is fully opaque, or
   *         <code>false</code> if some pixels of the background
   *         can shine through the border.
   */
  public boolean isBorderOpaque()
  {
    /* If the colors are to be drived from the enclosed Component's
     * background color, the border is guaranteed to be fully opaque
     * because Color.brighten() and Color.darken() always return an
     * opaque color.
     */
    return 
      ((highlightOuter == null) || (highlightOuter.getAlpha() == 255))
      && ((highlightInner == null) || (highlightInner.getAlpha() == 255))
      && ((shadowInner == null) || (shadowInner.getAlpha() == 255))
      && ((shadowOuter == null) ||(shadowOuter.getAlpha() == 255));
  }


  /**
   * Paints a raised bevel border around a component.
   *
   * @param c the component whose border is to be painted.
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   */
  protected void paintRaisedBevel(Component c, Graphics g,
                                  int x, int y, int width, int height)
  {
    paintBevel(g, x, y, width, height,
               getHighlightOuterColor(c), getHighlightInnerColor(c),
               getShadowInnerColor(c), getShadowOuterColor(c));
  }


  /**
   * Paints a lowered bevel border around a component.
   *
   * @param c the component whose border is to be painted.
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   */
  protected void paintLoweredBevel(Component c, Graphics g,
                                   int x, int y, int width, int height)
  {
    paintBevel(g, x, y, width, height,
               getShadowInnerColor(c), getShadowOuterColor(c),
               getHighlightInnerColor(c), getHighlightOuterColor(c));
  }


  /**
   * Paints a two-pixel bevel in four colors.
   * 
   * <pre>
   * @@@@@@@@@@@@
   * @..........#    @ = color a
   * @.        X#    . = color b
   * @.        X#    X = color c
   * @.XXXXXXXXX#    # = color d
   * ############</pre>
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
  private static void paintBevel(Graphics g,
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
       * images that are included with the JavaDoc. They are located
       * in the "doc-files" subdirectory.
       */
      g.setColor(a);
      g.drawLine(0, 0, width, 0);                       // a, horizontal
      g.drawLine(0, 1, 0, height);                      // a, vertical

      g.setColor(b);
      g.drawLine(1, 1, width - 1, 1);                   // b, horizontal
      g.drawLine(1, 2, 1, height - 1);                  // b, vertical

      g.setColor(c);
      g.drawLine(2, height - 1, width - 1, height - 1); // c, horizontal
      g.drawLine(width - 1, 2, width - 1, height - 2);  // c, vertical

      g.setColor(d);
      g.drawLine(1, height, width, height);             // d, horizontal
      g.drawLine(width, 1, width, height - 1);          // d, vertical
    }
    finally
    {
      g.translate(-x, -y);
      g.setColor(oldColor);
    }
  }
}

