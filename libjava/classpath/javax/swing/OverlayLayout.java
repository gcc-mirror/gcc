/* OverlayLayout.java -- A layout manager
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.AWTError;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager2;
import java.io.Serializable;

/**
 * A layout manager that lays out the components of a container one over
 * another.
 *
 * The components take as much space as is available in the container, but not
 * more than specified by their maximum size.
 *
 * The overall layout is mainly affected by the components
 * <code>alignmentX</code> and <code>alignmentY</code> properties. All
 * components are aligned, so that their alignment points (for either
 * direction) are placed in one line (the baseline for this direction).
 *
 * For example: An X alignment of 0.0 means that the component's alignment
 * point is at it's left edge, an X alignment of 0.5 means that the alignment
 * point is in the middle, an X alignment of 1.0 means, the aligment point is
 * at the right edge. So if you have three components, the first with 0.0, the
 * second with 0.5 and the third with 1.0, then they are laid out like this:
 *
 * <pre>
 *          +-------+
 *          |   1   |
 *          +-------+
 *      +-------+
 *      |   2   |
 *      +-------+
 * +---------+
 * |    3    +
 * +---------+
 * </pre>
 * The above picture shows the X alignment between the components. An Y
 * alignment like shown above cannot be achieved with this layout manager. The
 * components are place on top of each other, with the X alignment shown above.
 *
 * @author Roman Kennke (kennke@aicas.com)
 * @author Andrew Selkirk
 */
public class OverlayLayout implements LayoutManager2, Serializable
{
  private static final long serialVersionUID = 18082829169631543L;

  /**
   * The container to be laid out.
   */
  private Container target;

  /**
   * The size requirements of the containers children for the X direction.
   */
  private SizeRequirements[] xChildren;

  /**
   * The size requirements of the containers children for the Y direction.
   */
  private SizeRequirements[] yChildren;

  /**
   * The size requirements of the container to be laid out for the X direction.
   */
  private SizeRequirements xTotal;

  /**
   * The size requirements of the container to be laid out for the Y direction.
   */
  private SizeRequirements yTotal;

  /**
   * The offsets of the child components in the X direction.
   */
  private int[] offsetsX;

  /**
   * The offsets of the child components in the Y direction.
   */
  private int[] offsetsY;

  /**
   * The spans of the child components in the X direction.
   */
  private int[] spansX;

  /**
   * The spans of the child components in the Y direction.
   */
  private int[] spansY;

  /**
   * Creates a new OverlayLayout for the specified container.
   *
   * @param target the container to be laid out
   */
  public OverlayLayout(Container target)
  {
    this.target = target;
  }

  /**
   * Notifies the layout manager that the layout has become invalid. It throws
   * away cached layout information and recomputes it the next time it is
   * requested.
   *
   * @param target not used here
   */
  public void invalidateLayout(Container target)
  {
    xChildren = null;
    yChildren = null;
    xTotal = null;
    yTotal = null;
    offsetsX = null;
    offsetsY = null;
    spansX = null;
    spansY = null;
  }

  /**
   * This method is not used in this layout manager.
   *
   * @param string not used here
   * @param component not used here
   */
  public void addLayoutComponent(String string, Component component)
  {
    // Nothing to do here.
  }

  /**
   * This method is not used in this layout manager.
   *
   * @param component not used here
   * @param constraints not used here
   */
  public void addLayoutComponent(Component component, Object constraints)
  {
    // Nothing to do here.
  }

  /**
   * This method is not used in this layout manager.
   *
   * @param component not used here
   */
  public void removeLayoutComponent(Component component)
  {
    // Nothing to do here.
  }

  /**
   * Returns the preferred size of the container that is laid out. This is
   * computed by the children's preferred sizes, taking their alignments into
   * account.
   *
   * @param target not used here
   *
   * @return the preferred size of the container that is laid out
   */
  public Dimension preferredLayoutSize(Container target)
  {
    if (target != this.target)
      throw new AWTError("OverlayLayout can't be shared");

    checkTotalRequirements();
    return new Dimension(xTotal.preferred, yTotal.preferred);
  }

  /**
   * Returns the minimum size of the container that is laid out. This is
   * computed by the children's minimum sizes, taking their alignments into
   * account.
   *
   * @param target not used here
   *
   * @return the minimum size of the container that is laid out
   */
  public Dimension minimumLayoutSize(Container target)
  {
    if (target != this.target)
      throw new AWTError("OverlayLayout can't be shared");

    checkTotalRequirements();
    return new Dimension(xTotal.minimum, yTotal.minimum);
  }

  /**
   * Returns the maximum size of the container that is laid out. This is
   * computed by the children's maximum sizes, taking their alignments into
   * account.
   *
   * @param target not used here
   *
   * @return the maximum size of the container that is laid out
   */
  public Dimension maximumLayoutSize(Container target)
  {
    if (target != this.target)
      throw new AWTError("OverlayLayout can't be shared");

    checkTotalRequirements();
    return new Dimension(xTotal.maximum, yTotal.maximum);
  }

  /**
   * Returns the X alignment of the container that is laid out. This is
   * computed by the children's preferred sizes, taking their alignments into
   * account.
   *
   * @param target not used here
   *
   * @return the X alignment of the container that is laid out
   */
  public float getLayoutAlignmentX(Container target)
  {
    if (target != this.target)
      throw new AWTError("OverlayLayout can't be shared");

    checkTotalRequirements();
    return xTotal.alignment;
  }

  /**
   * Returns the Y alignment of the container that is laid out. This is
   * computed by the children's preferred sizes, taking their alignments into
   * account.
   *
   * @param target not used here
   *
   * @return the X alignment of the container that is laid out
   */
  public float getLayoutAlignmentY(Container target)
  {
    if (target != this.target)
      throw new AWTError("OverlayLayout can't be shared");

    checkTotalRequirements();
    return yTotal.alignment;
  }

  /**
   * Lays out the container and it's children.
   *
   * The children are laid out one over another.
   *
   * The components take as much space as is available in the container, but
   * not more than specified by their maximum size.
   *
   * The overall layout is mainly affected by the components
   * <code>alignmentX</code> and <code>alignmentY</code> properties. All
   * components are aligned, so that their alignment points (for either
   * direction) are placed in one line (the baseline for this direction).
   *
   * For example: An X alignment of 0.0 means that the component's alignment
   * point is at it's left edge, an X alignment of 0.5 means that the alignment
   * point is in the middle, an X alignment of 1.0 means, the aligment point is
   * at the right edge. So if you have three components, the first with 0.0,
   * the second with 0.5 and the third with 1.0, then they are laid out like
   * this:
   *
   * <pre>
   *          +-------+
   *          |   1   |
   *          +-------+
   *      +-------+
   *      |   2   |
   *      +-------+
   * +---------+
   * |    3    +
   * +---------+
   * </pre>
   * The above picture shows the X alignment between the components. An Y
   * alignment like shown above cannot be achieved with this layout manager.
   * The components are place on top of each other, with the X alignment shown
   * above.
   *
   * @param target not used here
   */
  public void layoutContainer(Container target)
  {
    if (target != this.target)
      throw new AWTError("OverlayLayout can't be shared");

    checkLayout();
    Component[] children = target.getComponents();
    for (int i = 0; i < children.length; i++)
      children[i].setBounds(offsetsX[i], offsetsY[i], spansX[i], spansY[i]);
  }

  /**
   * Makes sure that the xChildren and yChildren fields are correctly set up.
   * A call to {@link #invalidateLayout(Container)} sets these fields to null,
   * so they have to be set up again.
   */
  private void checkRequirements()
  {
    if (xChildren == null || yChildren == null)
      {
        Component[] children = target.getComponents();
        xChildren = new SizeRequirements[children.length];
        yChildren = new SizeRequirements[children.length];
        for (int i = 0; i < children.length; i++)
          {
            if (! children[i].isVisible())
              {
                xChildren[i] = new SizeRequirements();
                yChildren[i] = new SizeRequirements();
              }
            else
              {
                xChildren[i] =
                  new SizeRequirements(children[i].getMinimumSize().width,
                                       children[i].getPreferredSize().width,
                                       children[i].getMaximumSize().width,
                                       children[i].getAlignmentX());
                yChildren[i] =
                  new SizeRequirements(children[i].getMinimumSize().height,
                                       children[i].getPreferredSize().height,
                                       children[i].getMaximumSize().height,
                                       children[i].getAlignmentY());
              }
          }
      }
  }

  /**
   * Makes sure that the xTotal and yTotal fields are set up correctly. A call
   * to {@link #invalidateLayout} sets these fields to null and they have to be
   * recomputed.
   */
  private void checkTotalRequirements()
  {
    if (xTotal == null || yTotal == null)
      {
        checkRequirements();
        xTotal = SizeRequirements.getAlignedSizeRequirements(xChildren);
        yTotal = SizeRequirements.getAlignedSizeRequirements(yChildren);
      }
  }

  /**
   * Makes sure that the offsetsX, offsetsY, spansX and spansY fields are set
   * up correctly. A call to {@link #invalidateLayout} sets these fields
   * to null and they have to be recomputed.
   */
  private void checkLayout()
  {
    if (offsetsX == null || offsetsY == null || spansX == null
        || spansY == null)
      {
        checkRequirements();
        checkTotalRequirements();
        int len = target.getComponents().length;
        offsetsX = new int[len];
        offsetsY = new int[len];
        spansX = new int[len];
        spansY = new int[len];

        Insets in = target.getInsets();
        int width = target.getWidth() - in.left - in.right;
        int height = target.getHeight() - in.top - in.bottom;

        SizeRequirements.calculateAlignedPositions(width, xTotal,
                                                   xChildren, offsetsX, spansX);
        SizeRequirements.calculateAlignedPositions(height, yTotal,
                                                   yChildren, offsetsY, spansY);
      }
  }
}
