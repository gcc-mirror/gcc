/* EmptyBorder.java -- 
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


package javax.swing.border;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;


/**
 * A border for leaving a specifiable number of pixels empty around
 * the enclosed component.  An EmptyBorder requires some space on each
 * edge, but does not perform any drawing.
 *
 * <p><img src="EmptyBorder-1.png" width="290" height="200"
 * alt="[An illustration of EmptyBorder]" />
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class EmptyBorder
  extends AbstractBorder
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = -8116076291731988694L;


  /**
   * The number of pixels required at the left edge.
   */
  protected int left;


  /**
   * The number of pixels required at the right edge.
   */
  protected int right;


  /**
   * The number of pixels required at the top edge.
   */
  protected int top;


  /**
   * The number of pixels required at the bottom edge.
   */
  protected int bottom;


  /**
   * Constructs an empty border given the number of pixels required
   * on each side.
   *
   * @param top the number of pixels that the border will need
   *        for its top edge.
   *
   * @param left the number of pixels that the border will need
   *        for its left edge.
   *
   * @param bottom the number of pixels that the border will need
   *        for its bottom edge.
   *
   * @param right the number of pixels that the border will need
   *        for its right edge.
   */
  public EmptyBorder(int top, int left, int bottom, int right)
  {
    this.top = top;
    this.left = left;
    this.bottom = bottom;
    this.right = right;
  }


  /**
   * Constructs an empty border given the number of pixels required
   * on each side, passed in an Insets object.
   *
   * @param borderInsets the Insets for the new border.
   */
  public EmptyBorder(Insets borderInsets)
  {
    this(borderInsets.top, borderInsets.left,
         borderInsets.bottom, borderInsets.right);
  }


  /**
   * Performs nothing because an EmptyBorder does not paint any
   * pixels. While the inherited implementation provided by
   * {@link AbstractBorder#paintBorder} is a no-op as well,
   * it is overwritten in order to match the API of the Sun
   * reference implementation.
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
    return getBorderInsets(c, null);
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

    insets.left = left;
    insets.right = right;
    insets.top = top;
    insets.bottom = bottom;
    return insets;
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
    return getBorderInsets(null, null);
  }
  
  
  /**
   * Determines whether this border fills every pixel in its area
   * when painting. Since an empty border does not paint any pixels
   * whatsoever, the result is <code>false</code>.
   *
   * @return <code>false</code>.
   */
  public boolean isBorderOpaque()
  {
    /* The inherited implementation of AbstractBorder.isBorderOpaque()
     * would also return false. It is not clear why this is overriden
     * in the Sun implementation, at least not from just reading the
     * JavaDoc.
     */
    return false;
  }
}
