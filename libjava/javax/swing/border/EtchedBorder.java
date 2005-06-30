/* EtchedBorder.java -- 
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
 * A border that looks like an engraving etched into the background
 * surface, or (in its raised variant) coming out of the surface
 * plane. Using different constructors, it is possible to either
 * explicitly specify the border colors, or to let the colors derive
 * from the background color of the enclosed Component.
 *
 * <p><img src="doc-files/EtchedBorder-1.png" width="500" height="200"
 * alt="[An illustration of the two EtchedBorder variants]" />
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class EtchedBorder
  extends AbstractBorder
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = 4001244046866360638L;
  
  
  /**
   * Indicates that the border appears as coming out of the
   * background.
   */
  public static final int RAISED = 0;


  /**
   * Indicates that the border appears as engraved into the
   * background.
   */
  public static final int LOWERED = 1;

  
  /**
   * The type of this EtchedBorder, which is either {@link #RAISED}
   * or {@link #LOWERED}.
   */
  protected int etchType;
  

  /**
   * The highlight color, or <code>null</code> to indicate that the
   * color shall be derived from the background of the enclosed
   * component.
   */
  protected Color highlight;


  /**
   * The shadow color, or <code>null</code> to indicate that the
   * color shall be derived from the background of the enclosed
   * component.
   */
  protected Color shadow;


  /**
   * Constructs a lowered EtchedBorder. The colors will be derived
   * from the background color of the enclosed Component when the
   * border gets painted.
   */
  public EtchedBorder()
  {
    this(LOWERED);
  }


  /**
   * Constructs an EtchedBorder with the specified appearance. The
   * colors will be derived from the background color of the enclosed
   * Component when the border gets painted.
   *
   * <p><img src="doc-files/EtchedBorder-1.png" width="500" height="200"
   * alt="[An illustration of the two EtchedBorder variants]" />
   *
   * @param etchType the desired appearance of the border. The value
   *        must be either {@link #RAISED} or {@link #LOWERED}.
   *
   * @throws IllegalArgumentException if <code>etchType</code> has
   *         an unsupported value.
   */
  public EtchedBorder(int etchType)
  {
    if ((etchType != RAISED) && (etchType != LOWERED))
      throw new IllegalArgumentException();

    this.etchType = etchType;

    /* The highlight and shadow fields already have a null value
     * when the constructor gets called, so there is no need to
     * assign a value here.
     */
  }
  
  
  /**
   * Constructs a lowered EtchedBorder, explicitly selecting the
   * colors that will be used for highlight and shadow.
   *
   * @param highlight the color that will be used for painting
   *        the highlight part of the border.
   *
   * @param shadow the color that will be used for painting
   *        the shadow part of the border.
   *
   * @see #EtchedBorder(int, Color, Color)
   */
  public EtchedBorder(Color highlight, Color shadow)
  {
    this(LOWERED, highlight, shadow);
  }
  
  
  /**
   * Constructs an EtchedBorder with the specified appearance,
   * explicitly selecting the colors that will be used for
   * highlight and shadow.
   *
   * <p><img src="doc-files/EtchedBorder-2.png" width="500" height="200"
   * alt="[An illustration that shows which pixels get painted
   * in what color]" />
   *
   * @param etchType the desired appearance of the border. The value
   *        must be either {@link #RAISED} or {@link #LOWERED}.
   *
   * @param highlight the color that will be used for painting
   *        the highlight part of the border.
   *
   * @param shadow the color that will be used for painting
   *        the shadow part of the border.
   *
   * @throws IllegalArgumentException if <code>etchType</code> has
   *         an unsupported value.
   */
  public EtchedBorder(int etchType, Color highlight, Color shadow)
  {
    this(etchType);  // Checks the validity of the value.
    this.highlight = highlight;
    this.shadow = shadow;
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
    switch (etchType)
    {
    case RAISED:
      paintEtchedBorder(g, x, y, width, height,
                        getHighlightColor(c), getShadowColor(c));
      break;

    case LOWERED:
      paintEtchedBorder(g, x, y, width, height,
                        getShadowColor(c), getHighlightColor(c));
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
   * @see #getBorderInsets()
   */
  public Insets getBorderInsets(Component c, Insets insets)
  {
    insets.left = insets.right = insets.top = insets.bottom = 2;
    return insets;
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
      ((highlight == null) || (highlight.getAlpha() == 255))
      && ((shadow == null) || (shadow.getAlpha() == 255));
  }

  
  /**
   * Returns the appearance of this EtchedBorder, which is either
   * {@link #RAISED} or {@link #LOWERED}.
   */
  public int getEtchType()
  {
    return etchType;
  }


  /**
   * Determines the color that will be used for highlighted parts when
   * painting the border around a given component. If a highlight
   * color has been specified upon constructing the border, that color
   * is returned. Otherwise, the background color of the enclosed
   * component is brightened.
   *
   * @param c the component enclosed by this border.
   *
   * @see java.awt.Component#getBackground()
   * @see java.awt.Color#brighter()
   */
  public Color getHighlightColor(Component c)
  {
    if (highlight != null)
      return highlight;
    else
      return c.getBackground().brighter();
  }
  
  
  /**
   * Returns the color that will be used for highlighted parts when
   * painting the border, or <code>null</code> if that color will be
   * derived from the background of the enclosed Component.
   */
  public Color getHighlightColor()
  {
    return highlight;
  }


  /**
   * Determines the color that will be used for shadowed parts when
   * painting the border around a given component. If a shadow color
   * has been specified upon constructing the border, that color is
   * returned. Otherwise, the background color of the enclosed
   * component is darkened.
   *
   * @param c the component enclosed by this border.
   *
   * @see java.awt.Component#getBackground()
   * @see java.awt.Color#darker()
   */
  public Color getShadowColor(Component c)
  {
    if (shadow != null)
      return shadow;
    else
      return c.getBackground().darker();
  }
  
  
  /**
   * Returns the color that will be used for shadowed parts when
   * painting the border, or <code>null</code> if that color will be
   * derived from the background of the enclosed Component.
   */
  public Color getShadowColor()
  {
    return shadow;
  }


  /**
   * Paints a two-pixel etching in two colors.
   *
   * <pre>
   * @@@@@@@@@@@.
   * @.........@.    @ = color a
   * @.        @.    . = color b
   * @.        @.
   * @@@@@@@@@@@.
   * ............</pre>
   *
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   * @param a one of the two colors.
   * @param b the second of the two colors.
   */
  private static void paintEtchedBorder(Graphics g,
                                        int x, int y, int width, int height,
                                        Color a, Color b)
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
       * in the "doc-files" subdirectory. EtchedBorder-2.png might
       * be especially informative.
       */
      g.setColor(a);
      g.drawRect(0, 0, width - 1, height - 1);

      g.setColor(b);
      g.drawLine(1, 1, width - 2, 1);            // top edge
      g.drawLine(1, 2, 1, height - 2);           // left edge
      g.drawLine(0, height, width, height);      // bottom edge
      g.drawLine(width, 0, width, height - 1);   // right edge
    }
    finally
    {
      g.translate(-x, -y);
      g.setColor(oldColor);
    }
  }
}

