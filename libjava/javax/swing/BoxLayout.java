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
 * A layout for swing components.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
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
  
  /*
   * Current type of component layouting. Defaults to X_AXIS.
   */
  private int way = X_AXIS;

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
  }

  /**
   * Removes a component from the layout. Not used in BoxLayout.
   *
   * @param component The component to remove from the layout.
   */
  public void removeLayoutComponent(Component component)
  {
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
    if (parent != container)
      throw new AWTError("invalid parent");

    Insets insets = parent.getInsets();
    int x = 0;
    int y = 0;

    Component[] children = parent.getComponents();

    if (isHorizontalIn(parent))
      {        
        x = insets.left + insets.right;
        // sum up preferred widths of components, find maximum of preferred
        // heights
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getPreferredSize();
            x += sz.width;
            y = Math.max(y, sz.height);
          }
        y += insets.bottom + insets.top;
      } 
    else 
      {        
        y = insets.top + insets.bottom;
        // sum up preferred heights of components, find maximum of
        //  preferred widths
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getPreferredSize();
            y += sz.height;
            x = Math.max(x, sz.width);
          }
        x += insets.left + insets.right;
      }

    return new Dimension(x, y);
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
    if (parent != container)
      throw new AWTError("invalid parent");

    Insets insets = parent.getInsets();
    int x = insets.left + insets.right;
    int y = insets.bottom + insets.top;

    Component[] children = parent.getComponents();

    if (isHorizontalIn(parent))
      {
        // sum up preferred widths of components, find maximum of preferred
        // heights
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getMinimumSize();
            x += sz.width;
            y = Math.max(y, sz.height);
          }
      }
    else
      {
        // sum up preferred heights of components, find maximum of
        //  preferred widths
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getMinimumSize();
            y += sz.height;
            x = Math.max(x, sz.width);
          }
      }
    
    return new Dimension(x, y);
  }

  /**
   * Lays out the specified container using this layout.
   *
   * @param parent The container that needs to be laid out.
   */
  public void layoutContainer(Container parent)
  {
    if (parent != container)
      throw new AWTError("invalid parent");

    Dimension size = parent.getSize();
    Insets insets = parent.getInsets();
    Dimension innerSize = new Dimension(size.width - insets.left
                                        - insets.right, size.height
                                        - insets.bottom - insets.top);
    Component[] children = parent.getComponents();
    boolean[] laidOut = new boolean[children.length];
    for (int index = 0; index < laidOut.length; index++)
      laidOut[index] = false;

    if (isHorizontalIn(parent))
      {
        // compute overall preferred width
        int preferredWidthAll = 0;
        for (int index = 0; index < children.length; index++)
          {
            preferredWidthAll += children[index].getPreferredSize().width;
          }
        double widthFactor = (double) innerSize.width /
          (double) preferredWidthAll;

        // sort out components that are constrained by minimum or maximum size
        int widthRemain = innerSize.width;
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getPreferredSize();
            Dimension minSize = comp.getMinimumSize();
            Dimension maxSize = comp.getMaximumSize();
            int width = (int) (sz.width * widthFactor);
            int height = Math.min(innerSize.height, maxSize.height);
            // check min size
            if (width < minSize.width)
              {
                width = minSize.width;
                comp.setSize(width, height);
                laidOut[index] = true;
                preferredWidthAll -= sz.width;
                widthRemain -= width;
                continue;
              }
            // check max size
            if (width > maxSize.width)
              {
                width = maxSize.width;
                comp.setSize(width, height);
                laidOut[index] = true;
                preferredWidthAll -= sz.width;
                widthRemain -= width;
                continue;
              }

          }

        // recompute widthFactor for remaining components
        widthFactor = (double) widthRemain / (double) preferredWidthAll;

        int x = insets.left;

        // lay out remaining comonents
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            int width = 0;

            if (!laidOut[index])
              {
                Dimension sz = comp.getPreferredSize();
                Dimension maxSize = comp.getMaximumSize();
                width = (int) (sz.width * widthFactor);
                int height = Math.min(innerSize.height, maxSize.height);
                comp.setSize(width, height);
              }
            else
                width = comp.getWidth();

            int cy = (int) ((innerSize.height - comp.getHeight())
              * comp.getAlignmentY() + insets.top);
            comp.setLocation(x, cy);
            x = x + width;            
          }
      }
    else
      {
        // compute overall preferred height
        int preferredHeightAll = 0;
        for (int index = 0; index < children.length; index++)
          {
            preferredHeightAll += children[index].getPreferredSize().height;
          }
        double heightFactor = (double) innerSize.height /
          (double) preferredHeightAll;

        // sort out components that are constrained by minimum or maximum size
        int heightRemain = innerSize.height;
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getPreferredSize();
            Dimension minSize = comp.getMinimumSize();
            Dimension maxSize = comp.getMaximumSize();
            int height = (int) (sz.height * heightFactor);
            int width = Math.min(innerSize.width, maxSize.width);
            // check min size
            if (height < minSize.height)
              {
                height = minSize.height;
                comp.setSize(width, height);
                laidOut[index] = true;
                preferredHeightAll -= sz.height;
                heightRemain -= height;
                continue;
              }
            // check max size
            if (height > maxSize.height)
              {
                height = maxSize.height;
                comp.setSize(width, height);
                laidOut[index] = true;
                preferredHeightAll -= sz.height;
                heightRemain -= height;
                continue;
              }

          }

        // recompute heightFactor for remaining components
        heightFactor = (double) heightRemain / (double) preferredHeightAll;

        int y = insets.top;

        // lay out remaining comonents
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            int height = 0;

            if (!laidOut[index])
              {
                Dimension sz = comp.getPreferredSize();
                Dimension maxSize = comp.getMaximumSize();
                height = (int) (sz.height * heightFactor);
                int width = Math.min(innerSize.width, maxSize.width);
                comp.setSize(width, height);
              }
            else
              height = comp.getHeight();

            int cx = (int) ((innerSize.width - comp.getWidth())
              * comp.getAlignmentX() + insets.left);
            comp.setLocation(cx, y);
            y = y + height;            
          }
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
    if (parent != container)
      throw new AWTError("invalid parent");
    
    return 0;
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
    if (parent != container)
      throw new AWTError("invalid parent");
    
    return 0;
  }

  /**
   * Invalidates the layout.
   *
   * @param parent The container that needs to be laid out.
   */
  public void invalidateLayout(Container parent)
  {
    if (parent != container)
      throw new AWTError("invalid parent");
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
    if (parent != container)
      throw new AWTError("invalid parent");

    Insets insets = parent.getInsets();
    int x = insets.left + insets.right;
    int y = insets.top + insets.bottom;

    Component[] children = parent.getComponents();

    if (isHorizontalIn(parent))
      {
        
        // sum up preferred widths of components, find maximum of preferred
        // heights
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getMaximumSize();
            x += sz.width;
            y = Math.max(y, sz.height);
          }
      }
    else
      {
        // sum up preferred heights of components, find maximum of
        //  preferred widths
        for (int index = 0; index < children.length; index++)
          {
            Component comp = children[index];
            Dimension sz = comp.getMaximumSize();
            y += sz.height;
            x = Math.max(x, sz.width);
          }
      } 
    return new Dimension(x, y);
  }
}
