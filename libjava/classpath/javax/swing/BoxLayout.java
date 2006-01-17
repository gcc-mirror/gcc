/* BoxLayout.java -- A layout for swing components.
   Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.

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
import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager2;
import java.io.Serializable;

/**
 * A layout that stacks the children of a container in a Box, either
 * horizontally or vertically.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 * @author Roman Kennke (roman@kennke.org)
 */
public class BoxLayout implements LayoutManager2, Serializable
{

  /**
   * Specifies that components are laid out left to right.
   */
  public static final int X_AXIS = 0;

  /**
   * Specifies that components are laid out top to bottom.
   */
  public static final int Y_AXIS = 1;

  /**
   * Specifies that components are laid out in the direction of a line of text.
   */
  public static final int LINE_AXIS = 2;

  /**
   * Sepcifies that components are laid out in the direction of the line flow.
   */
  public static final int PAGE_AXIS = 3;

  /*
   * Needed for serialization.
   */
  private static final long serialVersionUID = -2474455742719112368L;

  /*
   * The container given to the constructor.
   */
  private Container container;
  
  /**
   * Current type of component layouting. Defaults to X_AXIS.
   */
  private int way = X_AXIS;

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
   * Constructs a <code>BoxLayout</code> object.
   *
   * @param container The container that needs to be laid out.
   * @param way The orientation of the components.
   *
   * @exception AWTError If way has an invalid value.
   */
  public BoxLayout(Container container, int way)
  {
    if (way != X_AXIS && way != Y_AXIS && way != LINE_AXIS && way != PAGE_AXIS)
      throw new AWTError("Invalid axis");

    int width = 0;
    int height = 0;
    this.container = container;
    this.way = way;
  }

  /**
   * Adds a component to the layout. Not used in BoxLayout.
   *
   * @param name The name of the component to add.
   * @param component the component to add to the layout.
   */
  public void addLayoutComponent(String name, Component component)
  {
    // Nothing to do here.
  }

  /**
   * Removes a component from the layout. Not used in BoxLayout.
   *
   * @param component The component to remove from the layout.
   */
  public void removeLayoutComponent(Component component)
  {
    // Nothing to do here.
  }

  private boolean isHorizontalIn(Container parent)
  {
    ComponentOrientation orientation = parent.getComponentOrientation();
    return this.way == X_AXIS 
      || (this.way == LINE_AXIS 
          && orientation.isHorizontal())
      || (this.way == PAGE_AXIS
          && (!orientation.isHorizontal()));
  }

  

  /**
   * Returns the preferred size of the layout.
   *
   * @param parent The container that needs to be laid out.
   *
   * @return The dimension of the layout.
   */
  public Dimension preferredLayoutSize(Container parent)
  {
    synchronized (container.getTreeLock())
      {
        if (container != parent)
          throw new AWTError("BoxLayout can't be shared");

        checkTotalRequirements();
        Insets i = container.getInsets();
        return new Dimension(xTotal.preferred + i.left + i.right,
                             yTotal.preferred + i.top + i.bottom);
      }
  }

  /**
   * Returns the minimum size of the layout.
   *
   * @param parent The container that needs to be laid out.
   *
   * @return The dimension of the layout.
   */
  public Dimension minimumLayoutSize(Container parent)
  {
    synchronized (container.getTreeLock())
      {
        if (container != parent)
          throw new AWTError("BoxLayout can't be shared");

        checkTotalRequirements();
        Insets i = container.getInsets();
        return new Dimension(xTotal.minimum + i.left + i.right,
                             yTotal.minimum + i.top + i.bottom);
      }
  }

  /**
   * Lays out the specified container using this layout.
   *
   * @param parent The container that needs to be laid out.
   */
  public void layoutContainer(Container parent)
  {
    synchronized (container.getTreeLock())
      {
        if (container != parent)
          throw new AWTError("BoxLayout can't be shared");
      
        checkLayout();
        Component[] children = container.getComponents();
        Insets in = container.getInsets();
        for (int i = 0; i < children.length; i++)
          children[i].setBounds(offsetsX[i] + in.left, offsetsY[i] + in.top,
                                spansX[i], spansY[i]);
      }
  }

  /**
   * Adds a component to the layout. Not used in BoxLayout
   *
   * @param child The component to add to the layout.
   * @param constraints The constraints for the component in the layout.
   */
  public void addLayoutComponent(Component child, Object constraints)
  {
    // Nothing to do here.
  }

  /**
   * Returns the alignment along the X axis for the container.
   *
   * @param parent The container that needs to be laid out.
   *
   * @return The alignment.
   */
  public float getLayoutAlignmentX(Container parent)
  {
    synchronized (container.getTreeLock())
      {
        if (container != parent)
          throw new AWTError("BoxLayout can't be shared");

        checkTotalRequirements();
        return xTotal.alignment;
      }
  }

  /**
   * Returns the alignment along the Y axis for the container.
   *
   * @param parent The container that needs to be laid out.
   *
   * @return The alignment.
   */
  public float getLayoutAlignmentY(Container parent)
  {
    synchronized (container.getTreeLock())
      {
        if (container != parent)
          throw new AWTError("BoxLayout can't be shared");

        checkTotalRequirements();
        return yTotal.alignment;
      }
  }

  /**
   * Invalidates the layout.
   *
   * @param parent The container that needs to be laid out.
   */
  public void invalidateLayout(Container parent)
  {
    if (container != parent)
      throw new AWTError("BoxLayout can't be shared");

    synchronized (container.getTreeLock())
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
  }

  /**
   * Returns the maximum size of the layout gived the components
   * in the given container.
   *
   * @param parent The container that needs to be laid out.
   *
   * @return The dimension of the layout.
   */
  public Dimension maximumLayoutSize(Container parent)
  {
    synchronized (container.getTreeLock())
      {
        if (container != parent)
          throw new AWTError("BoxLayout can't be shared");

        checkTotalRequirements();
        Insets i = container.getInsets();
        int xDim = xTotal.maximum + i.left + i.right;
        int yDim = yTotal.maximum + i.top + i.bottom;
        
        // Check for overflow
        if (xDim < xTotal.maximum)
          xDim = Integer.MAX_VALUE;
        if (yDim < yTotal.maximum)
          yDim = Integer.MAX_VALUE;
        return new Dimension(xDim, yDim);
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
        if (isHorizontalIn(container))
          {
            xTotal = SizeRequirements.getTiledSizeRequirements(xChildren);
            yTotal = SizeRequirements.getAlignedSizeRequirements(yChildren);
          }
        else
          {
            xTotal = SizeRequirements.getAlignedSizeRequirements(xChildren);
            yTotal = SizeRequirements.getTiledSizeRequirements(yChildren);
          }
      }
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
        Component[] children = container.getComponents();
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
        int len = container.getComponents().length;
        offsetsX = new int[len];
        offsetsY = new int[len];
        spansX = new int[len];
        spansY = new int[len];

        Insets in = container.getInsets();
        int width = container.getWidth() - in.left - in.right;
        int height = container.getHeight() - in.top -in.bottom;

        if (isHorizontalIn(container))
          {
            SizeRequirements.calculateTiledPositions(width,
                                                     xTotal, xChildren,
                                                     offsetsX, spansX);
            SizeRequirements.calculateAlignedPositions(height,
                                                       yTotal, yChildren,
                                                       offsetsY, spansY);
          }
        else
          {
            SizeRequirements.calculateAlignedPositions(width,
                                                       xTotal, xChildren,
                                                       offsetsX, spansX);
            SizeRequirements.calculateTiledPositions(height,
                                                     yTotal, yChildren,
                                                     offsetsY, spansY);
          }
      }
  }
}
