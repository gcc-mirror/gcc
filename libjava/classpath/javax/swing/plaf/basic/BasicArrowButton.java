/* BasicArrowButton.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Polygon;
import java.awt.Rectangle;

import javax.swing.ButtonModel;
import javax.swing.JButton;
import javax.swing.SwingConstants;

/**
 * A button that displays an arrow (triangle) that points {@link #NORTH},
 * {@link #SOUTH}, {@link #EAST} or {@link #WEST}.  This button is used by
 * the {@link BasicComboBoxUI} class.
 * 
 * @see BasicComboBoxUI#createArrowButton
 */
public class BasicArrowButton extends JButton implements SwingConstants
{

  /** 
   * The direction that the arrow points. 
   * 
   * @see #getDirection()
   */
  protected int direction;

  /**
   * The color the arrow is painted in if disabled and the bottom and right
   * edges of the button.
   * This is package-private to avoid an accessor method.
   */
  transient Color shadow = Color.GRAY;

  /**
   * The color the arrow is painted in if enabled and the bottom and right
   * edges of the button.
   * This is package-private to avoid an accessor method.
   */
  transient Color darkShadow = new Color(102, 102, 102);

  /**
   * The top and left edges of the button.
   * This is package-private to avoid an accessor method.
   */
  transient Color highlight = Color.WHITE;

  /**
   * Creates a new <code>BasicArrowButton</code> object.
   *
   * @param direction The direction the arrow points in (one of: 
   * {@link #NORTH}, {@link #SOUTH}, {@link #EAST} and {@link #WEST}).
   */
  public BasicArrowButton(int direction)
  {
    super();
    setDirection(direction);
  }

  /**
   * Creates a new BasicArrowButton object with the given colors and
   * direction.
   *
   * @param direction The direction to point in (one of: 
   * {@link #NORTH}, {@link #SOUTH}, {@link #EAST} and {@link #WEST}).
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
   * Returns whether the focus can traverse to this component.  This method
   * always returns <code>false</code>.
   *
   * @return <code>false</code>.
   */
  public boolean isFocusTraversable()
  {
    return false;
  }

  /**
   * Returns the direction of the arrow (one of: {@link #NORTH}, 
   * {@link #SOUTH}, {@link #EAST} and {@link #WEST}).
   *
   * @return The direction of the arrow.
   */
  public int getDirection()
  {
    return direction;
  }

  /**
   * Sets the direction of the arrow.
   *
   * @param dir The new direction of the arrow (one of: {@link #NORTH}, 
   *            {@link #SOUTH}, {@link #EAST} and {@link #WEST}).
   */
  public void setDirection(int dir)
  {
    this.direction = dir;
  }

  /**
   * Paints the arrow button. The painting is delegated to the
   * paintTriangle method.
   *
   * @param g The Graphics object to paint with.
   */
  public void paint(Graphics g)
  {
    super.paint(g);
    Rectangle bounds = getBounds();
    int size = bounds.height / 4;
    int x = (bounds.width - size) / 2;
    int y = (bounds.height - size) / 2;
    ButtonModel m = getModel();
    if (m.isArmed())
      {
        x++;
        y++;
      }
    paintTriangle(g, x, y, size, direction, isEnabled());
  }

  /** The preferred size for the button. */
  private static final Dimension PREFERRED_SIZE = new Dimension(16, 16);

  /** The minimum size for the button. */
  private static final Dimension MINIMUM_SIZE = new Dimension(5, 5);

  /** The maximum size for the button. */
  private static final Dimension MAXIMUM_SIZE 
    = new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
  
  /**
   * Returns the preferred size of the arrow button.
   *
   * @return The preferred size (always 16 x 16).
   */
  public Dimension getPreferredSize()
  {
    return PREFERRED_SIZE;
  }

  /**
   * Returns the minimum size of the arrow button.
   *
   * @return The minimum size (always 5 x 5).
   */
  public Dimension getMinimumSize()
  {
    return MINIMUM_SIZE;
  }

  /**
   * Returns the maximum size of the arrow button.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize()
  {
    return MAXIMUM_SIZE;
  }

  /**
   * Paints a triangle with the given size, location and direction.  It is 
   * difficult to explain the rationale behind the positioning of the triangle
   * relative to the given (x, y) position - by trial and error we seem to 
   * match the behaviour of the reference implementation (which is missing a 
   * specification for this method).
   *
   * @param g  the graphics device.
   * @param x  the x-coordinate for the triangle's location.
   * @param y  the y-coordinate for the triangle's location.
   * @param size  the arrow size (depth).
   * @param direction  the direction of the arrow (one of: {@link #NORTH}, 
   *            {@link #SOUTH}, {@link #EAST} and {@link #WEST}).
   * @param isEnabled  if <code>true</code> the arrow is drawn in the enabled
   *                   state, otherwise it is drawn in the disabled state.
   */
  public void paintTriangle(Graphics g, int x, int y, int size, int direction,
                            boolean isEnabled)
  {
    Color savedColor = g.getColor();
    switch (direction)
      {
      case NORTH:
        paintTriangleNorth(g, x, y, size, isEnabled);
        break;
      case SOUTH:
        paintTriangleSouth(g, x, y, size, isEnabled);
        break;
      case LEFT:
      case WEST:
        paintTriangleWest(g, x, y, size, isEnabled);
        break;
      case RIGHT:
      case EAST:
        paintTriangleEast(g, x, y, size, isEnabled);
        break;
      }
    g.setColor(savedColor);
  }
  
  /**
   * Paints an upward-pointing triangle.  This method is called by the 
   * {@link #paintTriangle(Graphics, int, int, int, int, boolean)} method.
   * 
   * @param g  the graphics device.
   * @param x  the x-coordinate for the anchor point.
   * @param y  the y-coordinate for the anchor point.
   * @param size  the arrow size (depth).
   * @param isEnabled  if <code>true</code> the arrow is drawn in the enabled
   *                   state, otherwise it is drawn in the disabled state.
   */
  private void paintTriangleNorth(Graphics g, int x, int y, int size, 
          boolean isEnabled)
  {
    int tipX = x + (size - 2) / 2;
    int tipY = y;
    int baseX1 = tipX - (size - 1);
    int baseX2 = tipX + (size - 1);
    int baseY = y + (size - 1);
    Polygon triangle = new Polygon();
    triangle.addPoint(tipX, tipY);
    triangle.addPoint(baseX1, baseY);
    triangle.addPoint(baseX2, baseY);
    if (isEnabled)
     {
       g.setColor(Color.DARK_GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
     }
    else
     {
       g.setColor(Color.GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
       g.setColor(Color.WHITE);
       g.drawLine(baseX1 + 1, baseY + 1, baseX2 + 1, baseY + 1);
     }
  }
  
  /**
   * Paints an downward-pointing triangle.  This method is called by the 
   * {@link #paintTriangle(Graphics, int, int, int, int, boolean)} method.
   * 
   * @param g  the graphics device.
   * @param x  the x-coordinate for the anchor point.
   * @param y  the y-coordinate for the anchor point.
   * @param size  the arrow size (depth).
   * @param isEnabled  if <code>true</code> the arrow is drawn in the enabled
   *                   state, otherwise it is drawn in the disabled state.
   */
  private void paintTriangleSouth(Graphics g, int x, int y, int size, 
          boolean isEnabled)
  {
    int tipX = x + (size - 2) / 2;
    int tipY = y + (size - 1);
    int baseX1 = tipX - (size - 1);
    int baseX2 = tipX + (size - 1);
    int baseY = y;
    Polygon triangle = new Polygon();
    triangle.addPoint(tipX, tipY);
    triangle.addPoint(baseX1, baseY);
    triangle.addPoint(baseX2, baseY);
    if (isEnabled)
     {
       g.setColor(Color.DARK_GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
     }
    else
     {
       g.setColor(Color.GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
       g.setColor(Color.WHITE);
       g.drawLine(tipX + 1, tipY, baseX2, baseY + 1);
       g.drawLine(tipX + 1, tipY + 1, baseX2 + 1, baseY + 1);
     }
  }
  
  /**
   * Paints a right-pointing triangle.  This method is called by the 
   * {@link #paintTriangle(Graphics, int, int, int, int, boolean)} method.
   * 
   * @param g  the graphics device.
   * @param x  the x-coordinate for the anchor point.
   * @param y  the y-coordinate for the anchor point.
   * @param size  the arrow size (depth).
   * @param isEnabled  if <code>true</code> the arrow is drawn in the enabled
   *                   state, otherwise it is drawn in the disabled state.
   */
  private void paintTriangleEast(Graphics g, int x, int y, int size, 
          boolean isEnabled)
  {
    int tipX = x + (size - 1);
    int tipY = y + (size - 2) / 2;
    int baseX = x;
    int baseY1 = tipY - (size - 1);
    int baseY2 = tipY + (size - 1);
    
    Polygon triangle = new Polygon();
    triangle.addPoint(tipX, tipY);
    triangle.addPoint(baseX, baseY1);
    triangle.addPoint(baseX, baseY2);
    if (isEnabled)
     {
       g.setColor(Color.DARK_GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
     }
    else
     {
       g.setColor(Color.GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
       g.setColor(Color.WHITE);
       g.drawLine(baseX + 1, baseY2, tipX, tipY + 1);
       g.drawLine(baseX + 1, baseY2 + 1, tipX + 1, tipY + 1);
     }
  }
  
  /**
   * Paints a left-pointing triangle.  This method is called by the 
   * {@link #paintTriangle(Graphics, int, int, int, int, boolean)} method.
   * 
   * @param g  the graphics device.
   * @param x  the x-coordinate for the anchor point.
   * @param y  the y-coordinate for the anchor point.
   * @param size  the arrow size (depth).
   * @param isEnabled  if <code>true</code> the arrow is drawn in the enabled
   *                   state, otherwise it is drawn in the disabled state.
   */
  private void paintTriangleWest(Graphics g, int x, int y, int size, 
          boolean isEnabled)
  {
    int tipX = x;
    int tipY = y + (size - 2) / 2;
    int baseX = x + (size - 1);
    int baseY1 = tipY - (size - 1);
    int baseY2 = tipY + (size - 1);
    
    Polygon triangle = new Polygon();
    triangle.addPoint(tipX, tipY);
    triangle.addPoint(baseX, baseY1);
    triangle.addPoint(baseX, baseY2);
    if (isEnabled)
     {
       g.setColor(Color.DARK_GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
     }
    else
     {
       g.setColor(Color.GRAY);
       g.fillPolygon(triangle);
       g.drawPolygon(triangle);
       g.setColor(Color.WHITE);
       g.drawLine(baseX + 1, baseY1 + 1, baseX + 1, baseY2 + 1);
     }
  }
  
}
