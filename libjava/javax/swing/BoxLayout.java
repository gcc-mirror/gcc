/* BoxLayout.java -- A layout for swing components.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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
import java.awt.GridLayout;
import java.awt.LayoutManager2;
import java.io.Serializable;


/**
 * A layout for swing components.
 * This implementation delegates its methods to
 * java.awt.GridLayout to do its work.
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
   * Internal layout.
   */
  private GridLayout grid;

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
    ComponentOrientation orientation = container.getComponentOrientation();

    this.container = container;
    this.way = way;

    switch (way)
      {
      case X_AXIS:
	width = 1;
	break;
      case Y_AXIS:
	height = 1;
	break;
      case LINE_AXIS:
	if (orientation.isHorizontal())
          height = 1;
	else
	  width = 1;
	break;
      case PAGE_AXIS:
	if (!orientation.isHorizontal())
          height = 1;
	else
	  width = 1;
	break;
      default:
	throw new AWTError("Invalid value for way");
      }

    grid = new GridLayout(width, height);
  }

  /**
   * Adds a component to the layout.
   *
   * @param name The name of the component to add.
   * @param component the component to add to the layout.
   */
  public void addLayoutComponent(String name, Component component)
  {
    if (way == X_AXIS
        || (way == LINE_AXIS
            && component.getComponentOrientation().isHorizontal())
        || (way == PAGE_AXIS
            && !component.getComponentOrientation().isHorizontal()))
      grid.setColumns(grid.getColumns() + 1);
    else
      grid.setRows(grid.getRows() + 1);
  }

  /**
   * Removes a component from the layout.
   *
   * @param component The component to remove from the layout.
   */
  public void removeLayoutComponent(Component component)
  {
    grid.removeLayoutComponent(component);

    if (way == X_AXIS
        || (way == LINE_AXIS
            && component.getComponentOrientation().isHorizontal())
        || (way == PAGE_AXIS
            && !component.getComponentOrientation().isHorizontal()))
      grid.setColumns(grid.getColumns() - 1);
    else
      grid.setRows(grid.getRows() - 1);
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
    
    return grid.preferredLayoutSize(parent);
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
    
    return grid.minimumLayoutSize(parent);
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
    
    grid.layoutContainer(parent);
  }

  /**
   * Adds a component to the layout.
   *
   * @param child The component to add to the layout.
   * @param constraints The constraints for the component in the layout.
   */
  public void addLayoutComponent(Component child, Object constraints)
  {
    addLayoutComponent("", child);
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
    
    return preferredLayoutSize(parent);
  }
}
