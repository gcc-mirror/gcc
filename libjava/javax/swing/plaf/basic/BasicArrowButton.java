/* BasicArrowButton.java
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
import java.awt.Insets;
import java.awt.Graphics;
import java.awt.Polygon;
import java.awt.Rectangle;
import javax.swing.border.Border;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.SwingConstants;


/**
 * This class draws simple arrow buttons for the Basic Look and Feel.
 */
public class BasicArrowButton extends JButton implements SwingConstants
{
  /**
   * A private helper class that draws icons.
   */
  private class arrowIcon implements Icon
  {
    /** The polygon that describes the icon. */
    private Polygon arrow;

    /** The size of the icon. */
    private int size = 10;

    /**
     * Creates a new arrowIcon object using the given arrow polygon.
     *
     * @param arrow The polygon that describes the arrow.
     */
    public arrowIcon(Polygon arrow)
    {
      this.arrow = arrow;
    }

    /**
     * Returns the height of the icon.
     *
     * @return The height of the icon.
     */
    public int getIconHeight()
    {
      return size;
    }

    /**
     * Returns the width of the icon.
     *
     * @return The width of the icon.
     */
    public int getIconWidth()
    {
      return size;
    }

    /**
     * Sets the size of the icon.
     *
     * @param size The size of the icon.
     */
    public void setSize(int size)
    {
      this.size = size;
    }

    /**
     * Sets the arrow polygon.
     *
     * @param arrow The arrow polygon.
     */
    public void setArrow(Polygon arrow)
    {
      this.arrow = arrow;
    }

    /**
     * Paints the icon.
     *
     * @param c The Component to paint for.
     * @param g The Graphics object to draw with.
     * @param x The X coordinate to draw at.
     * @param y The Y coordinate to draw at.
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      Color arrowColor;
      if (c.isEnabled())
	arrowColor = darkShadow;
      else
	arrowColor = shadow;

      paintIconImpl(g, x, y, arrowColor);
    }

    /**
     * This method does the actual painting work.
     *
     * @param g The Graphics object to paint with.
     * @param x The x coordinate to paint at.
     * @param y The y coordinate to paint at.
     * @param arrowColor The color to paint the arrow with.
     */
    public void paintIconImpl(Graphics g, int x, int y, Color arrowColor)
    {
      g.translate(x, y);

      Color saved = g.getColor();

      g.setColor(arrowColor);

      g.fillPolygon(arrow);

      g.setColor(saved);
      g.translate(-x, -y);
    }
  }

  /** The direction to point in. */
  protected int direction;

  /** The color the arrow is painted in if disabled and the bottom and
   * right edges of the button. */
  private transient Color shadow = Color.gray;

  /** The color the arrow is painted in if enabled and the bottom and
   * right edges of the button. */
  private transient Color darkShadow = Color.BLACK;

  /** The top and left edges of the button. */
  private transient Color highlight = Color.BLACK;

  /** The border around the ArrowButton. */
  private transient Border tmpBorder = new Border()
  {
    public Insets getBorderInsets(Component c)
    {
      return new Insets(0, 0, 0, 0);
    }
    
    public boolean isBorderOpaque()
    {
      return false;
    }
    
    public void paintBorder(Component c, Graphics g, int x, int y, int w, int h)
    {
      Rectangle bounds = getBounds();

      Color saved = g.getColor();
      g.setColor(highlight);

      g.drawLine(bounds.x, bounds.y, bounds.x, bounds.y + bounds.height);
      g.drawLine(bounds.x, bounds.y, bounds.x + bounds.width, bounds.y);

      g.setColor(shadow);

      g.drawLine(bounds.x + 1, bounds.y + bounds.height - 1,
                 bounds.x + bounds.width - 1, bounds.y + bounds.height - 1);
      g.drawLine(bounds.x + bounds.width - 1, bounds.y + 1,
                 bounds.x + bounds.width - 1, bounds.y + bounds.height - 1);

      g.setColor(darkShadow);

      g.drawLine(bounds.x, bounds.y + bounds.height, bounds.x + bounds.width,
                 bounds.y + bounds.height);
      g.drawLine(bounds.x + bounds.width, bounds.y, bounds.x + bounds.width,
                 bounds.y + bounds.height);

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
    setDirection(direction);
    setBorder(tmpBorder);
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
    Polygon arrow = getArrow(dir, 10);
    if (getIcon() == null)
      setIcon(new arrowIcon(arrow));
    else
      ((arrowIcon) getIcon()).setArrow(arrow);
    this.direction = dir;
  }

  /**
   * This method paints the arrow button.
   *
   * @param g The Graphics object to paint with.
   */
  public void paint(Graphics g)
  {
    super.paint(g);
  }

  /**
   * This method returns the preferred size of the arrow button.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize()
  {
    return new Dimension(getIcon().getIconWidth(), getIcon().getIconHeight());
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
   * The method paints a triangle with the given size and direction at
   * the given x and y coordinates.
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
    Polygon arrow = getArrow(direction, size);
    arrowIcon arrowI = new arrowIcon(arrow);
    arrowI.setSize(size);

    Color arrowColor;
    if (isEnabled())
      arrowColor = darkShadow;
    else
      arrowColor = shadow;

    arrowI.paintIconImpl(g, x, y, arrowColor);
  }

  /**
   * This is a private helper that creates polygons for a given size 
   * and direction.
   *
   * @param direction The direction of the arrow.
   * @param size The size of the arrow.
   *
   * @return A new arrow polygon.
   */
  private Polygon getArrow(int direction, int size)
  {
    Polygon arrow;
    double dsize = (double) size;
		
		int one = (int) (dsize * 1 / 10);
    int two = (int) (dsize * 2 / 10);
		int five = (int) (dsize * 5 / 10);
    int eight = (int) (dsize * 8 / 10);
		
    switch (direction)
      {
      case NORTH:
	arrow = new Polygon(new int[] { eight, five, one },
	                    new int[] { eight, one, eight }, 3);
	break;
      case SOUTH:
	arrow = new Polygon(new int[] { eight, five, two },
	                    new int[] { two, eight, two }, 3);
	break;
      case EAST:
      case RIGHT:
	arrow = new Polygon(new int[] { two, eight, two },
	                    new int[] { two, five, eight }, 3);
	break;
      case WEST:
      case LEFT:
	arrow = new Polygon(new int[] { eight, two, eight },
	                    new int[] { two, five, eight }, 3);
	break;
      default:
	throw new IllegalArgumentException("Invalid direction given.");
      }
    return arrow;
  }
}
