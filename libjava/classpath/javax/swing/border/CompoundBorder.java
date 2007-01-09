/* CompoundBorder.java -- 
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

/**
 * A Border that is composed of an interior and an exterior border,
 * where the interior border is tightly nested into the exterior.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class CompoundBorder extends AbstractBorder
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = 9054540377030555103L;

  /**
   * The inside border, which is painted between the bordered
   * Component and the outside border. It is valid for
   * <code>insideBorder</code> to be <code>null</code>.
   */
  protected Border insideBorder;

  /**
   * The outside border, which is painted outside both the
   * bordered Component and the inside border. It is valid for
   * <code>outsideBorder</code> to be <code>null</code>.
   */
  protected Border outsideBorder;

  /**
   * Constructs a CompoundBorder whose inside and outside borders
   * are both <code>null</code>. While this does not really make
   * any sense (there exists a class EmptyBorder as well, and not
   * every Component needs to have a border at all), the API
   * specification requires the existence of this constructor.
   *
   * @see EmptyBorder
   */
  public CompoundBorder()
  {
    this (null, null);
  }

  /**
   * Constructs a CompoundBorder with the specified inside and
   * outside borders.
   *
   * @param outsideBorder the outside border, which is painted to the
   *        outside of both <code>insideBorder</code> and the enclosed
   *        component. It is acceptable to pass <code>null</code>, in
   *        which case no outside border is painted.
   *
   * @param insideBorder the inside border, which is painted to
   *        between <code>outsideBorder</code> and the enclosed
   *        component. It is acceptable to pass <code>null</code>, in
   *        which case no inside border is painted.
   */
  public CompoundBorder(Border outsideBorder, Border insideBorder)
  {
    this.outsideBorder = outsideBorder;
    this.insideBorder = insideBorder;
  }

  /**
   * Determines whether or not this border is opaque. An opaque
   * border fills every pixel in its area when painting. Partially
   * translucent borders must return <code>false</code>, or ugly
   * artifacts can appear on screen.
   *
   * @return <code>true</code> if both the inside and outside borders
   *         are opaque, or <code>false</code> otherwise.
   */
  public boolean isBorderOpaque()
  {
    // Although the API specification states that this method 
    // returns true if both the inside and outside borders are non-null
    // and opaque, and false otherwise, a mauve test shows that if both
    // the inside or outside borders are null, then true is returned.
    if ((insideBorder == null) && (outsideBorder == null))
      return true;

    // A mauve test shows that if the inside border has a null value,
    // then true is returned if the outside border is opaque; if the
    // outside border has a null value, then true is returned if the
    // inside border is opaque; else, true is returned if both the
    // inside and outside borders are opaque.
    if (insideBorder == null)
      return outsideBorder.isBorderOpaque();
    else if (outsideBorder == null)
      return insideBorder.isBorderOpaque();
    else
      return insideBorder.isBorderOpaque() && outsideBorder.isBorderOpaque();
  }

  /**
   * Paints the compound border by first painting the outside border,
   * then painting the inside border tightly nested into the outside. 
   *
   * @param c the component whose border is to be painted.
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   */
  public void paintBorder(Component c, Graphics g,
                          int x, int y, int width, int height)
  {
    // If there is an outside border, paint it and reduce the
    // bounding box by its insets.
    //
    if (outsideBorder != null)
    {
      Insets outsideInsets;

      outsideBorder.paintBorder(c, g, x, y, width, height);
      outsideInsets = outsideBorder.getBorderInsets(c);

      x += outsideInsets.left;
      y += outsideInsets.top;

      // Reduce width and height by the respective extent of the
      // outside border.
      width -= outsideInsets.left + outsideInsets.right;
      height -= outsideInsets.top + outsideInsets.bottom;
    }

    if (insideBorder != null)
      insideBorder.paintBorder(c, g, x, y, width, height);
  }

  /**
   * Changes the specified insets to the insets of this border,
   * which is the sum of the insets of the inside and the outside
   * border.
   *
   * @param c the component in the center of this border.
   * @param insets an Insets object for holding the added insets.
   *
   * @return the <code>insets</code> object.
   */
  public Insets getBorderInsets(Component c, Insets insets)
  {
    Insets borderInsets;

    if (insets == null)
      insets = new Insets(0, 0, 0, 0);
    else
      insets.left = insets.right = insets.top = insets.bottom = 0;

    // If there is an outside border, add it to insets.
    if (outsideBorder != null)
    {
      borderInsets = outsideBorder.getBorderInsets(c);
      insets.left += borderInsets.left;
      insets.right += borderInsets.right;
      insets.top += borderInsets.top;
      insets.bottom += borderInsets.bottom;
    }

    // If there is an inside border, add it to insets.
    if (insideBorder != null)
    {
      borderInsets = insideBorder.getBorderInsets(c);
      insets.left += borderInsets.left;
      insets.right += borderInsets.right;
      insets.top += borderInsets.top;
      insets.bottom += borderInsets.bottom;
    }

    return insets;
  }

  /**
   * Determines the insets of this border, which is the sum of the
   * insets of the inside and the outside border.
   *
   * @param c the component in the center of this border.
   */
  public Insets getBorderInsets(Component c)
  {
    // It is not clear why CompoundBorder does not simply inherit
    // the implementation from AbstractBorder. However, we want
    // to be compatible with the API specification, which overrides
    // the getBorderInsets(Component) method.
    return getBorderInsets(c, null);
  }

  /**
   * Returns the outside border, which is painted outside both the
   * bordered Component and the inside border. It is valid for the
   * result to be <code>null</code>.
   * 
   * @return The outside border (possibly <code>null</code>).
   */
  public Border getOutsideBorder()
  {
    return outsideBorder;
  }

  /**
   * Returns the inside border, which is painted between the bordered
   * Component and the outside border. It is valid for the result to
   * be <code>null</code>.
   * 
   * @return The inside border (possibly <code>null</code>).
   */
  public Border getInsideBorder()
  {
    return insideBorder;
  }
}
