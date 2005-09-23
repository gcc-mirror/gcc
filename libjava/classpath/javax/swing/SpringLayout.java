/* SpringLayout.java -- 
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager2;
import java.util.HashMap;
import java.util.Map;

/**
 * A very flexible layout manager. Components are laid out by defining the
 * relationships between them. The relationships are expressed as
 * {@link Spring}s. You can attach a Spring for each edge of a component and
 * link it to an edge of a different component. For example, you can say,
 * the northern edge of component A should be attached to the southern edge
 * of component B, and the space between them should be something between
 * x and y pixels, and preferably z pixels.
 * <p>While quite simple, this layout manager can be used to emulate most other
 * layout managers, and can also be used to solve some layout problems, which
 * would be hard to solve with other layout managers.</p>
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
public class SpringLayout implements LayoutManager2
{

  /** The right edge of a component. */
  public static final String EAST = "East";

  /** The top edge of a component. */
  public static final String NORTH = "North";

  /** The bottom edge of a component. */
  public static final String SOUTH = "South";

  /** The left edge of a component. */
  public static final String WEST = "West";

  /** maps components to their constraints. */
  private Map constraintsMap;

  /**
   * The constraints that define the relationships between components.
   * Each Constraints object can hold 4 Springs: one for each edge of the
   * component. Additionally it can hold Springs for the components width
   * and the components height. Since the height and width constraints are
   * dependend on the other constraints, a component can be over-constraint.
   * In this case (like when all of NORTH, SOUTH and HEIGHT are constraint),
   * the values are adjusted, so that the mathematics still hold true.
   *
   * @author Roman Kennke (roman@ontographics.com)
   */
  public static class Constraints
  {

    // The constraints for each edge, and width and height.
    /** The Spring for the left edge. */
    private Spring x;

    /** The Spring for the upper edge. */
    private Spring y;

    /** The Spring for the height. */
    private Spring height;

    /** The Spring for the width. */
    private Spring width;

    /** The Spring for the right edge. */
    private Spring east;

    /** The Spring for the bottom edge. */
    private Spring south;

    /**
     * Creates a new Constraints object.
     * There is no constraint set.
     */
    public Constraints()
    {
      x = y = height = width = east = south = null;
    }

    /**
     * Creates a new Constraints object.
     *
     * @param x the constraint for the left edge of the component.
     * @param y the constraint for the upper edge of the component.
     */
    public Constraints(Spring x, Spring y)
    {
      this.x = x;
      this.y = y;
      width = height = east = south = null;
    }

    /**
     * Creates a new Constraints object.
     *
     * @param x the constraint for the left edge of the component.
     * @param y the constraint for the upper edge of the component.
     * @param width the constraint for the width of the component.
     * @param height the constraint for the height of the component.
     */
    public Constraints(Spring x, Spring y, Spring width, Spring height)
    {
      this.x = x;
      this.y = y;
      this.width = width;
      this.height = height;
      east = south = null;
    }

    /**
     * Returns the constraint for the edge with the <code>edgeName</code>.
     * This is expected to be one of
     * {@link #EAST}, {@link #WEST}, {@link #NORTH} or {@link #SOUTH}.
     *
     * @param edgeName the name of the edge.
     * @return the constraint for the specified edge.
     */
    public Spring getConstraint(String edgeName)
    {
      Spring retVal = null;
      if (edgeName.equals(SpringLayout.NORTH))
	retVal = y;
      else if (edgeName.equals(SpringLayout.WEST))
        retVal = x;
      else if (edgeName.equals(SpringLayout.SOUTH))
        {
          retVal = south;
	  if ((retVal == null) && (y != null) && (height != null))
            retVal = Spring.sum(y, height);
        }
      else if (edgeName.equals(SpringLayout.EAST))
        {
          retVal = east;
          if ((retVal == null) && (x != null) && (width != null))
            retVal = Spring.sum(x, width);
	}

      return retVal;
    }

    /**
     * Returns the constraint for the height of the component.
     *
     * @return the height constraint. 
     */
    public Spring getHeight()
    {
      Spring retVal = height;
      if ((retVal == null) && (y != null) && (south != null))
        {
          retVal = Spring.sum(south, Spring.minus(y));
        }
      return retVal;
    }

    /**
     * Returns the constraint for the width of the component.
     *
     * @return the width constraint.
     */
    public Spring getWidth()
    {
      Spring retVal = width;
      if ((retVal == null) && (x != null) && (east != null))
        {
          retVal = Spring.sum(east, Spring.minus(x));
	}
      return retVal;
    }

    /**
     * Returns the constraint for the left edge of the component.
     *
     * @return the left-edge constraint (== WEST).
     */
    public Spring getX()
    {
      Spring retVal = x;
      if ((retVal == null) && (width != null) && (east != null))
        {
          retVal = Spring.sum(east, Spring.minus(width));
        }
      return retVal;
    }

    /**
     * Returns the constraint for the upper edge of the component.
     *
     * @return the upper-edge constraint (== NORTH).
     */
    public Spring getY()
    {
      Spring retVal = y;
      if ((retVal == null) && (height != null) && (south != null))
        {
          retVal = Spring.sum(south, Spring.minus(height));
        }
      return retVal;
    }

    /**
     * Sets a constraint for the specified edge. If this leads to an
     * over-constrained situation, the constraints get adjusted, so that
     * the mathematics still hold true.
     *
     * @param edgeName the name of the edge, one of {@link #EAST},
     *     {@link #WEST}, {@link #NORTH} or {@link #SOUTH}.
     * @param s the constraint to be set.
     */
    public void setConstraint(String edgeName, Spring s)
    {
    
      if (edgeName.equals(SpringLayout.WEST))
        {
          x = s;
	  if ((width != null) && (east != null))
            width = Spring.sum(east, Spring.minus(x));
        }
      else if (edgeName.equals(SpringLayout.NORTH))
        {
          y = s;
          if ((height != null) && (south != null))
          height = Spring.sum(south, Spring.minus(y));
        }
      else if (edgeName.equals(SpringLayout.EAST))
        {
          east = s;
          if ((x != null) && (width != null))
            x = Spring.sum(east, Spring.minus(width));
        }
      else if (edgeName.equals(SpringLayout.SOUTH))
        {
          south = s;
          if ((height != null) && (y != null))
	    y = Spring.sum(south, Spring.minus(height));
        }

    }

    /**
     * Sets the height-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setHeight(Spring s)
    {
      height = s;
      if ((south != null) && (y != null))
        south = Spring.sum(y, height);

    }

    /**
     * Sets the width-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setWidth(Spring s)
    {
      width = s;
      if ((east != null) && (x != null))
        east = Spring.sum(x, width);

    }

    /**
     * Sets the WEST-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setX(Spring s)
    {
      x = s;
      if ((width != null) && (east != null))
        width = Spring.sum(east, Spring.minus(x));

    }

    /**
     * Sets the NORTH-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setY(Spring s)
    {
      y = s;
      if ((height != null) && (south != null))
        height = Spring.sum(south, Spring.minus(y));

    }
  }

  /**
   * Creates a new SpringLayout.
   */
  public SpringLayout()
  {

    constraintsMap = new HashMap();
  }

  /**
   * Adds a layout component and a constraint object to this layout.
   * This method is usually only called by a {@java.awt.Container}s add
   * Method.
   *
   * @param component the component to be added.
   * @param constraint the constraint to be set.
   */
  public void addLayoutComponent(Component component, Object constraint)
  {
    constraintsMap.put(component, constraint);
  }


  /**
   * Adds a layout component and a constraint object to this layout.
   * This method is usually only called by a {@java.awt.Container}s add
   * Method. This method does nothing, since SpringLayout does not manage
   * String-indexed components.
   *
   * @param name  the name.
   * @param c the component to be added.
   */
  public void addLayoutComponent(String name, Component c)
  {
    // do nothing here.
  }

  /**
   * Returns the constraint of the edge named by <code>edgeName</code>.
   *
   * @param c the component from which to get the constraint.
   * @param edgeName the name of the edge, one of {@link #EAST},
   *     {@link #WEST}, {@link #NORTH} or {@link #SOUTH}.
   * @return the constraint of the edge <code>edgeName</code> of the
   * component c.
   */
  public Spring getConstraint(String edgeName, Component c)
  {
    Constraints constraints = getConstraints(c);
    return constraints.getConstraint(edgeName);
  }

  /**
   * Returns the {@link Constraints} object associated with the specified
   * component.
   *
   * @param c the component for which to determine the constraint.
   * @return the {@link Constraints} object associated with the specified
   *      component.
   */
  public SpringLayout.Constraints getConstraints(Component c)
  {
    Constraints constraints = (Constraints) constraintsMap.get(c);
    if (constraints == null)
      {
        Container parent = c.getParent();
        constraints = new Constraints();
        if (parent != null)
          {
            constraints.setX
              (Spring.constant(parent.getInsets().left));
            constraints.setY
              (Spring.constant(parent.getInsets().top));
          }
        else
          {
            constraints.setX
              (Spring.constant(0));
            constraints.setY
              (Spring.constant(0));

          }
        constraints.setWidth
          (Spring.constant(c.getMinimumSize().width,
                           c.getPreferredSize().width,
                           c.getMaximumSize().width));
        constraints.setHeight
          (Spring.constant(c.getMinimumSize().height,
                           c.getPreferredSize().height,
                           c.getMaximumSize().height));

        constraintsMap.put(c, constraints);

      }

    return constraints;
  }

  /**
   * Returns the X alignment of the Container <code>p</code>.
   *
   * @param p the {@link java.awt.Container} for which to determine the X
   *     alignment.
   * @return always 0.0
   */
  public float getLayoutAlignmentX(Container p)
  {
    return 0.0F;
  }

  /**
   * Returns the Y alignment of the Container <code>p</code>.
   *
   * @param p the {@link java.awt.Container} for which to determine the Y
   *     alignment.
   * @return always 0.0
   */
  public float getLayoutAlignmentY(Container p)
  {
    return 0.0F;
  }

  /**
   * Recalculate a possibly cached layout.
   */
  public void invalidateLayout(Container p)
  {
    // nothing to do here yet
  }

  /**
   * Lays out the container <code>p</code>.
   *
   * @param p the container to be laid out.
   */
  public void layoutContainer(Container p)
  {

    addLayoutComponent(p, new Constraints(Spring.constant(0),
                                          Spring.constant(0)));

    int offsetX = p.getInsets().left;
    int offsetY = p.getInsets().right;

    Component[] components = p.getComponents();
    for (int index = 0; index < components.length; index++)
      {
        Component c = components[index];
        Constraints constraints = getConstraints(c);
        int x = constraints.getX().getValue();
        int y = constraints.getY().getValue();
        int width = constraints.getWidth().getValue();
        int height = constraints.getHeight().getValue();

        c.setLocation(x + offsetX, y + offsetY);
        c.setSize(width, height);
      }

  }

  /**
   * Calculates the maximum size of the layed out container. This
   * respects the maximum sizes of all contained components.
   *
   * @param p the container to be laid out.
   * @return the maximum size of the container.
   */
  public Dimension maximumLayoutSize(Container p)
  {
    int maxX = 0;
    int maxY = 0;

    int offsetX = p.getInsets().left;
    int offsetY = p.getInsets().right;

    Component[] components = p.getComponents();
    for (int index = 0; index < components.length; index++)
      {
        Component c = components[index];
        Constraints constraints = getConstraints(c);
        int x = constraints.getX().getMaximumValue();
        int y = constraints.getY().getMaximumValue();
        int width = constraints.getWidth().getMaximumValue();
        int height = constraints.getHeight().getMaximumValue();

        int rightEdge = offsetX + x + width;
        if (rightEdge > maxX)
          maxX = rightEdge;
        int bottomEdge = offsetY + y + height;
        if (bottomEdge > maxY)
          maxY = bottomEdge;
      }

    return new Dimension(maxX, maxY);
  }


  /**
   * Calculates the minimum size of the layed out container. This
   * respects the minimum sizes of all contained components.
   *
   * @param p the container to be laid out.
   * @return the minimum size of the container.
   */
  public Dimension minimumLayoutSize(Container p)
  {
    int maxX = 0;
    int maxY = 0;

    int offsetX = p.getInsets().left;
    int offsetY = p.getInsets().right;

    Component[] components = p.getComponents();
    for (int index = 0; index < components.length; index++)
      {
        Component c = components[index];
        Constraints constraints = getConstraints(c);
        int x = constraints.getX().getMinimumValue();
        int y = constraints.getY().getMinimumValue();
        int width = constraints.getWidth().getMinimumValue();
        int height = constraints.getHeight().getMinimumValue();

        int rightEdge = offsetX + x + width;
        if (rightEdge > maxX)
          maxX = rightEdge;
        int bottomEdge = offsetY + y + height;
        if (bottomEdge > maxY)
          maxY = bottomEdge;
      }

    return new Dimension(maxX, maxY);
  }

  /**
   * Calculates the preferred size of the layed out container. This
   * respects the preferred sizes of all contained components.
   *
   * @param p the container to be laid out.
   * @return the preferred size of the container.
   */
  public Dimension preferredLayoutSize(Container p)
  {
    int maxX = 0;
    int maxY = 0;

    int offsetX = p.getInsets().left;
    int offsetY = p.getInsets().right;

    Component[] components = p.getComponents();
    for (int index = 0; index < components.length; index++)
      {
        Component c = components[index];
        Constraints constraints = getConstraints(c);
        int x = constraints.getX().getPreferredValue();
        int y = constraints.getY().getPreferredValue();
        int width = constraints.getWidth().getPreferredValue();
        int height = constraints.getHeight().getPreferredValue();

        int rightEdge = offsetX + x + width;
        if (rightEdge > maxX)
          maxX = rightEdge;
        int bottomEdge = offsetY + y + height;
        if (bottomEdge > maxY)
          maxY = bottomEdge;
      }

    return new Dimension(maxX, maxY);
  }

  /**
   * Attaches the edge <code>e1</code> of component <code>c1</code> to
   * the edge <code>e2</code> of component <code>c2</code> width the
   * fixed strut <code>pad</code>.
   *
   * @param e1 the edge of component 1.
   * @param c1 the component 1.
   * @param pad the space between the components in pixels.
   * @param e2 the edge of component 2.
   * @param c2 the component 2.
   */
  public void putConstraint(String e1, Component c1, int pad, String e2, 
                            Component c2)
  {
    Constraints constraints1 = getConstraints(c1);
    Constraints constraints2 = getConstraints(c2);

    Spring strut = Spring.constant(pad);
    Spring otherEdge = constraints2.getConstraint(e2);
    constraints1.setConstraint(e1, Spring.sum(strut, otherEdge));

  }

  /**
   * Attaches the edge <code>e1</code> of component <code>c1</code> to
   * the edge <code>e2</code> of component <code>c2</code> width the
   * {@link Spring} <code>s</code>.
   *
   * @param e1 the edge of component 1.
   * @param c1 the component 1.
   * @param s the space between the components as a {@link Spring} object.
   * @param e2 the edge of component 2.
   * @param c2 the component 2.
   */
  public void putConstraint(String e1, Component c1, Spring s, String e2, 
                            Component c2)
  {
    Constraints constraints1 = getConstraints(c1);
    Constraints constraints2 = getConstraints(c2);

    Spring otherEdge = constraints2.getConstraint(e2);
    constraints1.setConstraint(e1, Spring.sum(s, otherEdge));

  }

  /**
   * Removes a layout component.
   * @param c the layout component to remove.
   */
  public void removeLayoutComponent(Component c)
  {
    // do nothing here
  }
}
