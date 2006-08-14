/* SpringLayout.java -- 
   Copyright (C) 2004, 2006, Free Software Foundation, Inc.

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
     In each axis the user can set three values, i.e. x, width, east, if all
     three are set, then there's no room for manoeuvre so in those cases the 
     third will be described by the below spring which is calculated in terms 
     of the other two
    */
    private Spring v;
    private Spring h;

    /**
     * Creates a new Constraints object.
     * There is no constraint set.
     */
    public Constraints()
    {
      x = y = height = width = east = south = v = h = null;
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
      width = height = east = south = v = h = null;
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
      east = south = v = h = null;
    }

    /**
     * Create a new Constraints object which tracks the indicated
     * component.  The x and y positions for this Constraints object
     * are constant Springs created with the component's location at
     * the time this constructor is called.  The width and height
     * of this Constraints are Springs created using
     * {@link Spring#width(Component)} and {@link Spring#height(Component)},
     * respectively.
     * @param component the component to track
     * @since 1.5
     */
    public Constraints(Component component)
    {
      this(Spring.constant(component.getX()),
           Spring.constant(component.getY()),
           Spring.width(component),
           Spring.height(component));
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
	retVal = getY();
      else if (edgeName.equals(SpringLayout.WEST))
        retVal = getX();
      else if (edgeName.equals(SpringLayout.SOUTH))
        retVal = getSouth();
      else if (edgeName.equals(SpringLayout.EAST))
        retVal = getEast();
      return retVal;
    }

    /**
     * Returns the constraint for the height of the component.
     *
     * @return the height constraint. 
     */
    public Spring getHeight()
    {
      if (height != null)
        return height;
      else if ((v == null) && (y != null) && (south != null))
          v = Spring.sum(south, Spring.minus(y));
      return v;
    }

    /**
     * Returns the constraint for the width of the component.
     *
     * @return the width constraint.
     */
    public Spring getWidth()
    {
      if (width != null)
        return width;
      else if ((h == null) && (x != null) && (east != null))
        h = Spring.sum(east, Spring.minus(x));
      return h;
    }

    /**
     * Returns the constraint for the left edge of the component.
     *
     * @return the left-edge constraint (== WEST).
     */
    public Spring getX()
    {
      if (x != null)
        return x;
      else if ((h == null) && (width != null) && (east != null))
        h = Spring.sum(east, Spring.minus(width));
      return h;
    }

    /**
     * Returns the constraint for the upper edge of the component.
     *
     * @return the upper-edge constraint (== NORTH).
     */
    public Spring getY()
    {
      if (y != null)
        return y;
      else if ((v == null) && (height != null) && (south != null))
        v = Spring.sum(south, Spring.minus(height));
      return v;
    }

    /**
     * Returns the constraint for the lower edge of the component.
     *
     * @return the lower-edge constraint (== SOUTH).
     */
    public Spring getSouth()
    {
      if (south != null)
        return south;
      else if ((v == null) && (height != null) && (y != null))
        v = Spring.sum(y, height);
      return v;
    }

    /**
     * Returns the constraint for the right edge of the component.
     *
     * @return the right-edge constraint (== EAST).
     */
    public Spring getEast()
    {
      if (east != null)
        return east;
      else if ((h == null) && (width != null) && (x != null))
        h = Spring.sum(x, width);
      return h;
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
        setX(s);
      else if (edgeName.equals(SpringLayout.NORTH))
        setY(s);
      else if (edgeName.equals(SpringLayout.EAST))
        setEast(s);
      else if (edgeName.equals(SpringLayout.SOUTH))
        setSouth(s);

    }

    /**
     * Sets the height-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setHeight(Spring s)
    {
      height = s;
      v = null;
      if ((south != null) && (y != null) && (height != null))
          south = null;
    }

    /**
     * Sets the width-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setWidth(Spring s)
    {
      width = s;
      h = null;
      if ((east != null) && (x != null) && (width != null))
        east = null;
    }

    /**
     * Sets the WEST-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setX(Spring s)
    {
      x = s;
      h = null;
      if ((width != null) && (east != null) && (x != null))
        width = null;
    }

    /**
     * Sets the NORTH-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setY(Spring s)
    {
      y = s;
      v = null;
      if ((height != null) && (south != null) && (y != null))
        height = null;
    }

    /**
     * Sets the SOUTH-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setSouth(Spring s)
    {
      south = s;
      v = null;
      if ((height != null) && (south != null) && (y != null))
        y = null;
    }

    /**
     * Sets the EAST-constraint.
     *
     * @param s the constraint to be set.
     */
    public void setEast(Spring s)
    {
      east = s;
      h = null;
      if ((width != null) && (east != null) && (x != null))
        x = null;
    }

    public void dropCalcResult()
    {
      if (x != null)
        x.setValue(Spring.UNSET);
      if (y != null)
        y.setValue(Spring.UNSET);
      if (width != null)
        width.setValue(Spring.UNSET);
      if (height != null)
        height.setValue(Spring.UNSET);
      if (east != null) 
        east.setValue(Spring.UNSET);
      if (south != null)
        south.setValue(Spring.UNSET);
      if (h != null)
        h.setValue(Spring.UNSET);
      if (v != null)
        v.setValue(Spring.UNSET);
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
   * This method is usually only called by a {@link java.awt.Container}s add
   * method.
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
   * This method is usually only called by a {@link java.awt.Container}s add
   * method. This method does nothing, since SpringLayout does not manage
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
   * The trick to SpringLayout is that the network of Springs needs to
   * completely created before the positioning results are generated.
   *
   * Using the springs directly during network creation will set their values 
   * before the network is completed, Using Deferred Springs during creation of 
   * the network allows all the edges to be connected together and the network 
   * to be created without resolving the Springs until their results need to be 
   * known, at which point the network is complete and the spring addition and 
   * and substitution calculations will work on a complete and valid network.
   *
   * @author Caolan McNamara (caolanm@redhat.com)
   */
  private static class DeferredSpring extends Spring 
  {
    private SpringLayout sl;
    private String edgeName;
    private Component c;

    public String toString()
    {
      return "DeferredSpring of edge" + edgeName + " of " + "something";
    }

    public DeferredSpring(SpringLayout s, String edge, Component component)
    {
        sl = s;
        edgeName = edge;
        c = component;
    }

    private Spring resolveSpring() 
    {
        return sl.getConstraints(c).getConstraint(edgeName);
    }

    public int getMaximumValue() 
    {
        return resolveSpring().getMaximumValue();
    }

    public int getMinimumValue() 
    {
        return resolveSpring().getMinimumValue();
    }

    public int getPreferredValue() 
    {
        return resolveSpring().getPreferredValue();
    }

    public int getValue() 
    {
        int nRet = resolveSpring().getValue();
        if (nRet == Spring.UNSET)
            nRet = getPreferredValue();
        return nRet;
    }

    public void setValue(int size) 
    {
        resolveSpring().setValue(size);
    }
  }

  private abstract static class DeferredDimension extends Spring
  {
    private int value;

    public DeferredDimension()
    {
      value = Spring.UNSET;
    }

    public void setValue(int val)
    {
      value = val;
    }

    public int getValue()
    {
      if (value == Spring.UNSET)
          return getPreferredValue();
      return value;
    }
  }

  private static class DeferredWidth extends DeferredDimension
  {
    private Component c;


    public DeferredWidth(Component component)
    {
        c = component;
    }

    public String toString()
    {
      return "DeferredWidth of " + "something";
    }

    //clip max to a value we can do meaningful calculation with
    public int getMaximumValue() 
    {
        int widget_width = c.getMaximumSize().width;
        return Math.min(Short.MAX_VALUE, widget_width);
    }

    public int getMinimumValue() 
    {
        return c.getMinimumSize().width;
    }

    public int getPreferredValue() 
    {
        return c.getPreferredSize().width;
    }
  }

  private static class DeferredHeight extends DeferredDimension
  {
    private Component c;

    public String toString()
    {
        return "DeferredHeight of " + "something";
    }

    public DeferredHeight(Component component)
    {
        c = component;
    }

    //clip max to a value we can do meaningful calculations with it
    public int getMaximumValue() 
    {
        int widget_height = c.getMaximumSize().height;
        return Math.min(Short.MAX_VALUE, widget_height);
    }

    public int getMinimumValue() 
    {
        return c.getMinimumSize().height;
    }

    public int getPreferredValue() 
    {
        return c.getPreferredSize().height;
    }
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
    return new DeferredSpring(this, edgeName, c);
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
      constraints = new Constraints();

      constraints.setWidth(new DeferredWidth(c));
      constraints.setHeight(new DeferredHeight(c));
      constraints.setX(Spring.constant(0));
      constraints.setY(Spring.constant(0));

      constraintsMap.put(c, constraints);
    }

    return constraints;
  }

  /**
   * Returns the X alignment of the Container <code>p</code>.
   * 
   * @param p
   *          the {@link java.awt.Container} for which to determine the X
   *          alignment.
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

  private Constraints initContainer(Container p)
  {
    Constraints c = getConstraints(p);

    c.setX(Spring.constant(0));
    c.setY(Spring.constant(0));
    c.setWidth(null);
    c.setHeight(null);
    if (c.getEast() == null)
      c.setEast(Spring.constant(0, 0, Integer.MAX_VALUE));
    if (c.getSouth() == null) 
      c.setSouth(Spring.constant(0, 0, Integer.MAX_VALUE));

    return c;
  }

  /**
   * Lays out the container <code>p</code>.
   *
   * @param p the container to be laid out.
   */
  public void layoutContainer(Container p)
  {
    java.awt.Insets insets = p.getInsets();

    Component[] components = p.getComponents();

    Constraints cs = initContainer(p);
    cs.dropCalcResult();

    for (int index = 0 ; index < components.length; index++)
    {
        Component c = components[index];
        getConstraints(c).dropCalcResult();
    }

    int offsetX = p.getInsets().left;
    int offsetY = p.getInsets().right;

    cs.getX().setValue(0);
    cs.getY().setValue(0);
    cs.getWidth().setValue(p.getWidth() - offsetX - insets.right);
    cs.getHeight().setValue(p.getHeight() - offsetY - insets.bottom);

    for (int index = 0; index < components.length; index++)
    {
      Component c = components[index];

      Constraints constraints = getConstraints(c);
      
      int x = constraints.getX().getValue();
      int y = constraints.getY().getValue();
      int width = constraints.getWidth().getValue();
      int height = constraints.getHeight().getValue();
      
      c.setBounds(x + offsetX, y + offsetY, width, height);
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
    java.awt.Insets insets = p.getInsets();

    Constraints cs = initContainer(p);

    int maxX = cs.getWidth().getMaximumValue() + insets.left + insets.right;
    int maxY = cs.getHeight().getMaximumValue() + insets.top + insets.bottom;

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
    java.awt.Insets insets = p.getInsets();

    Constraints cs = initContainer(p);

    int maxX = cs.getWidth().getMinimumValue() + insets.left + insets.right;
    int maxY = cs.getHeight().getMinimumValue() + insets.top + insets.bottom;

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
    java.awt.Insets insets = p.getInsets();

    Constraints cs = initContainer(p);

    int maxX = cs.getWidth().getPreferredValue() + insets.left + insets.right;
    int maxY = cs.getHeight().getPreferredValue() + insets.top + insets.bottom;

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
    putConstraint(e1, c1, Spring.constant(pad), e2, c2);
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

    Spring otherEdge = getConstraint(e2, c2);
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
