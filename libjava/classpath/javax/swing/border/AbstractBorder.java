/* AbstractBorder.java -- 
   Copyright (C) 2003, 2006, Free Software Foundation, Inc.

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
import java.awt.Rectangle;
import java.io.Serializable;


/**
 * An invisible zero-width border, serving as a base class for
 * implementing more interesting borders.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public abstract class AbstractBorder implements Border, Serializable
{
  static final long serialVersionUID = -545885975315191844L;

  /**
   * Constructs a new AbstractBorder.
   */
  public AbstractBorder()
  {
    // Nothing to do here.
  }

  /**
   * Performs nothing, because the default implementation provided by
   * this class is an invisible, zero-width border. Subclasses will
   * likely want to override this method, but they are not required
   * to do so.
   *
   * @param c the component whose border is to be painted.
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   */
  public void paintBorder(Component c, Graphics g, int x, int y, int width,
                          int height)
  {
    // A previous version of Classpath had emitted a warning when
    // this method was called. The warning was removed because it is
    // perfectly legal for a subclass to not override the paintBorder
    // method. An example would be EmptyBorder.
  }

  /**
   * Returns the insets required for drawing this border around the specified 
   * component.
   *
   * @param c  the component that the border applies to (ignored here, 
   *     subclasses may use it).
   *
   * @return an Insets object whose <code>left</code>, <code>right</code>,
   *         <code>top</code> and <code>bottom</code> fields indicate the
   *         width of the border at the respective edge, which is zero
   *         for the default implementation provided by AbstractButton.
   *
   * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
   */
  public Insets getBorderInsets(Component c)
  {
    return new Insets(0, 0, 0, 0);
  }

  /**
   * Returns the insets required for drawing this border around the specified 
   * component.  The default implementation provided here sets the 
   * <code>left</code>, <code>right</code>, <code>top</code> and 
   * <code>bottom</code> fields of the passed <code>insets</code> parameter to 
   * zero.
   *
   * @param c  the component that the border applies to (ignored here, 
   *     subclasses may use it).
   * @param insets  an instance that will be overwritten and returned as the 
   *     result (<code>null</code> not permitted).  
   *
   * @return The border insets (the same object that was passed as the 
   *     <code>insets</code> argument).
   *
   * @see #getBorderInsets(Component)
   * 
   * @throws NullPointerException if <code>insets</code> is <code>null</code>.
   */
  public Insets getBorderInsets(Component c, Insets insets) 
  {
    insets.left = insets.right = insets.top = insets.bottom = 0;
    return insets;
  }

  /**
   * Determines whether or not this border is opaque. An opaque border
   * fills every pixel in its area when painting. Partially
   * translucent borders must return <code>false</code>, or ugly
   * artifacts can appear on screen. The default implementation
   * provided here always returns <code>false</code>.
   *
   * @return <code>false</code>.
   */
  public boolean isBorderOpaque() 
  {
    return false;
  }

  /**
   * Returns a rectangle that covers the specified area minus the insets 
   * required to draw this border.  Components that wish to determine an area 
   * into which they can safely draw without intersecting with a border might
   * want to use this helper method.
   *
   * @param c the component in the center of this border.
   * @param x the horizontal position of the border.
   * @param y the vertical position of the border.
   * @param width the width of the available area for the border.
   * @param height the height of the available area for the border.
   * 
   * @return The interior rectangle.
   */
  public Rectangle getInteriorRectangle(Component c, int x, int y, int width,
                                        int height)
  {
    return getInteriorRectangle(c, this, x, y, width, height);
  }
  
  /**
   * Returns a rectangle that covers the specified area minus the insets 
   * required to draw the specified border (if the border is <code>null</code>,
   * zero insets are assumed).  Components that wish to determine an area into 
   * which they can safely draw without intersecting with a border might want 
   * to use this helper method.
   *
   * @param c the component in the center of this border.
   * @param b the border (<code>null</code> permitted).
   * @param x the horizontal position of the border.
   * @param y the vertical position of the border.
   * @param width the width of the available area for the border.
   * @param height the height of the available area for the border.
   * 
   * @return The interior rectangle.
   */
  public static Rectangle getInteriorRectangle(Component c, Border b, int x,
                                               int y, int width, int height)
  {
    Insets borderInsets;

    if (b != null)
    {
      borderInsets = b.getBorderInsets(c);
      x += borderInsets.left;
      y += borderInsets.top;
      width -= borderInsets.left + borderInsets.right;
      height -= borderInsets.top + borderInsets.bottom;
    }

    return new Rectangle(x, y, width, height);
  }
}
