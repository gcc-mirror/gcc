/* BasicArrowButton.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Polygon;
import java.awt.Rectangle;

import javax.swing.JButton;
import javax.swing.SwingConstants;
import javax.swing.border.Border;

/**
 * This class draws simple arrow buttons for the Basic Look and Feel.
 */
public class BasicArrowButton extends JButton implements SwingConstants
{
  /** The default size of the Arrow buttons. */
  private static int defaultSize = 10;

  /** The Polygon that points up. */
  private static Polygon upIcon = new Polygon(new int[] { 0, 5, 9 },
                                              new int[] { 7, 2, 7 }, 3);

  /** The Polygon that points down. */
  private static Polygon downIcon = new Polygon(new int[] { 1, 5, 9 },
                                                new int[] { 3, 7, 3 }, 3);

  /** The Polygon that points left. */
  private static Polygon leftIcon = new Polygon(new int[] { 7, 3, 7 },
                                                new int[] { 1, 5, 9 }, 3);

  /** The Polygon that points right. */
  private static Polygon rightIcon = new Polygon(new int[] { 3, 7, 3 },
                                                 new int[] { 1, 5, 9 }, 3);

  /** The direction to point in. */
  protected int direction;

  /**
   * The color the arrow is painted in if disabled and the bottom and right
   * edges of the button.
   */
  private transient Color shadow = Color.GRAY;

  /**
   * The color the arrow is painted in if enabled and the bottom and right
   * edges of the button.
   */
  private transient Color darkShadow = Color.DARK_GRAY;

  /** The top and left edges of the button. */
  private transient Color highlight = Color.WHITE;

  /** The border around the ArrowButton. */
  private transient Border buttonBorder = new Border()
    {
      public Insets getBorderInsets(Component c)
      {
	return new Insets(2, 2, 2, 2);
      }

      public boolean isBorderOpaque()
      {
	return true;
      }

      public void paintBorder(Component c, Graphics g, int x, int y, int w,
                              int h)
      {
	Color saved = g.getColor();
	g.setColor(highlight);

	g.drawLine(x + 1, y + 1, x + w - 1, y + 1);
	g.drawLine(x + 1, y + 1, x + 1, y + h - 1);

	g.setColor(shadow);

	g.drawLine(x + 1, y + h - 1, x + w - 1, y + h - 1);
	g.drawLine(x + w - 1, y + 1, x + w - 1, y + h - 1);

	g.setColor(darkShadow);

	g.drawLine(x, y + h, x + w, y + h);
	g.drawLine(x + w, y, x + w, y + h);

	g.setColor(saved);
      }
    };

  /**
   * Creates a new BasicArrowButton object.
   *
   * @param direction The direction the arrow points in.
   */
  public BasicArrowButton(int direction)
  {
    super();
    setBorder(buttonBorder);
    setDirection(direction);
  }

  /**
   * Creates a new BasicArrowButton object with the given colors and
   * direction.
   *
   * @param direction The direction to point in.
   * @param background The background color.
   * @param shadow The shadow color.
   * @param darkShadow The dark shadow color.
   * @param highlight The highlight color.
   */
  public BasicArrowButton(int direction, Color background, Color shadow,
                          Color darkShadow, Color highlight)
  {
    this(direction);
    setBackground(background);
    this.shadow = shadow;
    this.darkShadow = darkShadow;
    this.highlight = highlight;
  }

  /**
   * This method returns whether the focus can traverse to this component.
   *
   * @return Whether the focus can traverse to this component.
   */
  public boolean isFocusTraversable()
  {
    return false;
  }

  /**
   * This method returns the direction of the arrow.
   *
   * @return The direction of the arrow.
   */
  public int getDirection()
  {
    return direction;
  }

  /**
   * This method changes the direction of the arrow.
   *
   * @param dir The new direction of the arrow.
   */
  public void setDirection(int dir)
  {
    this.direction = dir;
  }

  /**
   * This method paints the arrow button. The painting is delegated to the
   * paintTriangle method.
   *
   * @param g The Graphics object to paint with.
   */
  public void paint(Graphics g)
  {
    super.paint(g);
    Insets insets = getInsets();
    Rectangle bounds = getBounds();
    int x = insets.left
            + (bounds.width - insets.left - insets.right - defaultSize) / 2;
    int y = insets.top
            + (bounds.height - insets.left - insets.right - defaultSize) / 2;
    paintTriangle(g, x, y, defaultSize, direction, isEnabled());
  }

  /**
   * This method returns the preferred size of the arrow button.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize()
  {
    Insets insets = getInsets();
    int w = defaultSize + insets.left + insets.right;
    int h = defaultSize + insets.top + insets.bottom;

    return new Dimension(w, h);
  }

  /**
   * This method returns the minimum size of the arrow button.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize()
  {
    return getPreferredSize();
  }

  /**
   * This method returns the maximum size of the arrow button.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize()
  {
    return getPreferredSize();
  }

  /**
   * The method paints a triangle with the given size and direction at the
   * given x and y coordinates.
   *
   * @param g The Graphics object to paint with.
   * @param x The x coordinate to paint at.
   * @param y The y coordinate to paint at.
   * @param size The size of the icon.
   * @param direction The direction of the icon.
   * @param isEnabled Whether it is enabled.
   */
  public void paintTriangle(Graphics g, int x, int y, int size, int direction,
                            boolean isEnabled)
  {
    Polygon arrow = null;
    switch (direction)
      {
      case NORTH:
	arrow = upIcon;
	break;
      case SOUTH:
	arrow = downIcon;
	break;
      case EAST:
      case RIGHT:
	arrow = rightIcon;
	break;
      case WEST:
      case LEFT:
	arrow = leftIcon;
	break;
      }

    int[] xPoints = arrow.xpoints;
    int[] yPoints = arrow.ypoints;
    int x1;
    int y1;
    int x2;
    int y2;
    x1 = y1 = x2 = y2 = 0;

    if (size != defaultSize)
      {
	float scale = size * 1f / defaultSize;
	for (int i = 0; i < 3; i++)
	  {
	    xPoints[i] *= scale;
	    yPoints[i] *= scale;
	  }
      }
    g.translate(x, y);

    switch (direction)
      {
      case NORTH:
	x1 = xPoints[0] + 2;
	y1 = yPoints[0];
	y2 = y1;
	x2 = xPoints[2] - 1;
	break;
      case SOUTH:
	x1 = xPoints[1];
	y1 = yPoints[1] + 1;
	x2 = xPoints[2] - 1;
	y2 = yPoints[2];
	break;
      case LEFT:
      case WEST:
	x1 = xPoints[0] + 1;
	y1 = yPoints[0] + 1;
	x2 = x1;
	y2 = yPoints[2] + 1;
	break;
      case RIGHT:
      case EAST:
	x1 = xPoints[2];
	y1 = yPoints[2] + 1;
	x2 = xPoints[1] - 1;
	y2 = yPoints[1] + 1;
	break;
      }
    Color saved = g.getColor();

    if (isEnabled)
      {
	g.setColor(Color.DARK_GRAY);

	if (arrow != null)
	  g.fillPolygon(xPoints, yPoints, 3);
      }
    else
      {
	g.setColor(Color.GRAY);
	g.fillPolygon(xPoints, yPoints, 3);
	g.setColor(Color.WHITE);
	g.drawLine(x1, y1, x2, y2);
      }
    g.setColor(saved);
    g.translate(-x, -y);
  }
}
