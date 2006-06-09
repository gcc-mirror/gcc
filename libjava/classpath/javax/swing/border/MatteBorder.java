/* MatteBorder.java -- 
   Copyright (C) 2003, 2004, 2006  Free Software Foundation, Inc.

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

import javax.swing.Icon;

/**
 * A border that is filled with either a solid color or with repeated
 * icon tiles.
 *
 * <p><img src="doc-files/MatteBorder-1.png" width="500" height="150"
 * alt="[Two MatteBorders]" />
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class MatteBorder extends EmptyBorder
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = 4422248989617298224L;
  
  
  /**
   * The color that is used for filling the border, or
   * <code>null</code> if the border is filled with repetitions of a
   * tile icon.
   *
   * @see #tileIcon
   */
  protected Color color;
  
  
  /**
   * The icon is used for filling the border with a tile, or
   * <code>null</code> if the border is filled with a solid
   * color.
   *
   * @see #color
   */
  protected Icon tileIcon;
  
  
  /**
   * Constructs a MatteBorder given the width on each side
   * and a fill color.
   *
   * <p><img src="doc-files/MatteBorder-2.png" width="500" height="150"
   * alt="[A picture of a MatteBorder made by this constructor]" />
   *
   * @param top the width of the border at its top edge.
   * @param left the width of the border at its left edge.
   * @param bottom the width of the border at its bottom edge.
   * @param right the width of the border at its right edge.
   * @param matteColor the color for filling the border.
   */
  public MatteBorder(int top, int left, int bottom, int right,
                     Color matteColor)
  {
    super(top, left, bottom, right);

    if (matteColor == null)
      throw new IllegalArgumentException();

    this.color = matteColor;
  }


  /**
   * Constructs a MatteBorder given its insets and fill color.
   *
   * <p><img src="doc-files/MatteBorder-3.png" width="500" height="150"
   * alt="[A picture of a MatteBorder made by this constructor]" />
   *
   * @param borderInsets an Insets object whose <code>top</code>,
   *        <code>left</code>, <code>bottom</code> and <code>right</code>
   *        fields indicate the with of the border at the respective
   *        edge.
   *
   * @param matteColor the color for filling the border.
   */
  public MatteBorder(Insets borderInsets, Color matteColor)
  {
    this(borderInsets.top, borderInsets.left,
         borderInsets.bottom, borderInsets.right,
         matteColor);
  }


  /**
   * Constructs a MatteBorder given the width on each side
   * and an icon for tiling the border area.
   *
   * <p><img src="doc-files/MatteBorder-4.png" width="500" height="150"
   * alt="[A picture of a MatteBorder made by this constructor]" />
   *
   * @param top the width of the border at its top edge.
   * @param left the width of the border at its left edge.
   * @param bottom the width of the border at its bottom edge.
   * @param right the width of the border at its right edge.
   * @param tileIcon an icon for tiling the border area.
   */
  public MatteBorder(int top, int left, int bottom, int right,
                     Icon tileIcon)
  {
    super(top, left, bottom, right);

    this.tileIcon = tileIcon;
  }


  /**
   * Constructs a MatteBorder given its insets and an icon
   * for tiling the border area.
   *
   * <p><img src="doc-files/MatteBorder-5.png" width="500" height="150"
   * alt="[A picture of a MatteBorder made by this constructor]" />
   *
   * @param borderInsets an Insets object whose <code>top</code>,
   *        <code>left</code>, <code>bottom</code> and <code>right</code>
   *        fields indicate the with of the border at the respective
   *        edge.
   *
   * @param tileIcon an icon for tiling the border area.
   */
  public MatteBorder(Insets borderInsets, Icon tileIcon)
  {
    this(borderInsets.top, borderInsets.left,
         borderInsets.bottom, borderInsets.right,
         tileIcon);
  }
  
  
  /**
   * Constructs a MatteBorder given an icon for tiling the
   * border area. The icon width is used for the border insets
   * at the left and right edge, the icon height for the top and
   * bottom edge.
   *
   * <p><img src="doc-files/MatteBorder-6.png" width="379" height="150"
   * alt="[A picture of a MatteBorder made by this constructor]" />
   *
   * @param tileIcon an icon for tiling the border area.
   */
  public MatteBorder(Icon tileIcon)
  {
    this(-1, -1, -1, -1, tileIcon);
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
    Insets i = getBorderInsets();
    paintEdge(c, g, x, y, width, i.top, 0, 0);      // top edge
    paintEdge(c, g, x, y + height - i.bottom,       // bottom edge
              width, i.bottom,
              0, height - i.bottom);
    paintEdge(c, g, x, y + i.top,                   // left edge
              i.left, height - i.top,
              0, i.top);
    paintEdge(c, g, x + width - i.right, y + i.top, // right edge
              i.right, height - i.bottom,
              width - i.right, i.top);
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
    /* There is no obvious reason for overriding this method, but we
     * try to have exactly the same API as the Sun reference
     * implementation.
     */
    return this.getBorderInsets(c, null);
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
    if (insets == null)
      insets = new Insets(0, 0, 0, 0);

    if ((tileIcon != null)
        && (top < 0) && (left < 0)
        && (right < 0) && (bottom < 0))
    {
      insets.left = insets.right = tileIcon.getIconWidth();
      insets.top = insets.bottom = tileIcon.getIconHeight();
      return insets;
    }

    /* Copy top, left, bottom and right into the respective
     * field of insets.
     */
    return super.getBorderInsets(c, insets);
  }
  
  
  /**
   * Measures the width of this border.
   *
   * @return an Insets object whose <code>left</code>, <code>right</code>,
   *         <code>top</code> and <code>bottom</code> fields indicate the
   *         width of the border at the respective edge.
   *
   * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
   */
  public Insets getBorderInsets()
  {
    /* The inherited implementation of EmptyBorder.isBorderOpaque()
     * would do the same. It is not clear why this is overriden in the
     * Sun implementation, at least not from just reading the JavaDoc.
     */
    return this.getBorderInsets(null, null);
  }
  
  
  /**
   * Returns the color that is used for filling the border, or
   * <code>null</code> if the border is filled with repetitions of a
   * tile icon.
   * 
   * @return The color (possibly <code>null</code>).
   */
  public Color getMatteColor()
  {
    return color;
  }
  
  
  /**
   * Returns the icon is used for tiling the border, or
   * <code>null</code> if the border is filled with a color instead of
   * an icon.
   * 
   * @return The icon (possibly <code>null</code>).
   */
  public Icon getTileIcon()
  {
    return tileIcon;
  }
  
  
  /**
   * Determines whether this border fills every pixel in its area
   * when painting.
   *
   * @return <code>true</code> if the border is filled with an
   *         opaque color; <code>false</code> if it is filled with
   *         a semi-transparent color or with an icon.
   */
  public boolean isBorderOpaque()
  {
    return (color != null) && (color.getAlpha() == 255);
  }
  

  /**
   * Paints a rectangular area of the border. This private helper
   * method is called once for each of the border edges
   * by {@link #paintBorder}.
   *
   * @param c the component whose border is being painted.
   * @param g the graphics for painting.
   * @param x the horizontal position of the rectangular area.
   * @param y the vertical position of the rectangular area.
   * @param width the width of the rectangular area.
   * @param height the height of the rectangular area.
   * @param dx the x displacement for repeating the tile.
   * @param dy the y displacement for repeating the tile.
   */
  private void paintEdge(Component c, Graphics g,
                         int x, int y, int width, int height,
                         int dx, int dy)
  {
    Color oldColor;
    int iconWidth, iconHeight;
    Graphics clipped;

    if ((width <= 0) || (height <= 0))
      return;

    /* Paint a colored rectangle if desired. */
    if (color != null)
    {
      oldColor = g.getColor();
      try
      {
        g.setColor(color);
        g.fillRect(x, y, width, height);
      }
      finally
      {
        g.setColor(oldColor);
      }
      return;
    }
    
    // If this border has no icon end painting here.
    if (tileIcon == null)
      return;

    /* Determine the width and height of the icon. Some icons return
     * -1 if it is an image whose dimensions have not yet been
     * retrieved. There is not much we can do about this, but we
     * should at least avoid entering the paint loop below
     * with negative increments.
     */
    iconWidth = tileIcon.getIconWidth();
    iconHeight = tileIcon.getIconHeight();
    if ((iconWidth <= 0) || (iconHeight <= 0))
      return;

    dx = dx % iconWidth;
    dy = dy % iconHeight;

    clipped = g.create();
    try
    {
      clipped.setClip(x, y, width, height);
      for (int ty = y - dy; ty < y + height; ty += iconHeight)
        for (int tx = x - dx; tx < x + width; tx += iconWidth)
          tileIcon.paintIcon(c, clipped, tx, ty);
    }
    finally
    {
      clipped.dispose();
    }
  }
}
