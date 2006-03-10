/* BorderLayout.java -- A layout manager class
   Copyright (C) 1999, 2002, 2005  Free Software Foundation, Inc.

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


package java.awt;


/**
  * This class implements a layout manager that positions components
  * in certain sectors of the parent container.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Rolf W. Rasmussen  (rolfwr@ii.uib.no)
  */
public class BorderLayout implements LayoutManager2, java.io.Serializable
{

  /**
   * Constant indicating the top of the container
   */
  public static final String NORTH = "North";

  /**
   * Constant indicating the bottom of the container
   */
  public static final String SOUTH = "South";

  /**
   * Constant indicating the right side of the container
   */
  public static final String EAST = "East";

  /**
   * Constant indicating the left side of the container
   */
  public static final String WEST = "West";

  /**
   * Constant indicating the center of the container
   */
  public static final String CENTER = "Center";


  /**
   * The constant indicating the position before the first line of the
   * layout.  The exact position depends on the writing system: For a
   * top-to-bottom orientation, it is the same as {@link #NORTH}, for
   * a bottom-to-top orientation, it is the same as {@link #SOUTH}.
   *
   * <p>This constant is an older name for {@link #PAGE_START} which
   * has exactly the same value.
   *
   * @since 1.2
   */
  public static final String BEFORE_FIRST_LINE = "First";

  /**
   * The constant indicating the position after the last line of the
   * layout.  The exact position depends on the writing system: For a
   * top-to-bottom orientation, it is the same as {@link #SOUTH}, for
   * a bottom-to-top orientation, it is the same as {@link #NORTH}.
   *
   * <p>This constant is an older name for {@link #PAGE_END} which
   * has exactly the same value.
   *
   * @since 1.2
   */
  public static final String AFTER_LAST_LINE = "Last";

  /**
   * The constant indicating the position before the first item of the
   * layout.  The exact position depends on the writing system: For a
   * left-to-right orientation, it is the same as {@link #WEST}, for
   * a right-to-left orientation, it is the same as {@link #EAST}.
   *
   * <p>This constant is an older name for {@link #LINE_START} which
   * has exactly the same value.
   *
   * @since 1.2
   */
  public static final String BEFORE_LINE_BEGINS = "Before";

  /**
   * The constant indicating the position after the last item of the
   * layout.  The exact position depends on the writing system: For a
   * left-to-right orientation, it is the same as {@link #EAST}, for
   * a right-to-left orientation, it is the same as {@link #WEST}.
   *
   * <p>This constant is an older name for {@link #LINE_END} which
   * has exactly the same value.
   *
   * @since 1.2
   */
  public static final String AFTER_LINE_ENDS = "After";

  /**
   * The constant indicating the position before the first line of the
   * layout.  The exact position depends on the writing system: For a
   * top-to-bottom orientation, it is the same as {@link #NORTH}, for
   * a bottom-to-top orientation, it is the same as {@link #SOUTH}.
   *
   * @since 1.4
   */
  public static final String PAGE_START = BEFORE_FIRST_LINE;

  /**
   * The constant indicating the position after the last line of the
   * layout.  The exact position depends on the writing system: For a
   * top-to-bottom orientation, it is the same as {@link #SOUTH}, for
   * a bottom-to-top orientation, it is the same as {@link #NORTH}.
   *
   * @since 1.4
   */
  public static final String PAGE_END = AFTER_LAST_LINE;

  /**
   * The constant indicating the position before the first item of the
   * layout.  The exact position depends on the writing system: For a
   * left-to-right orientation, it is the same as {@link #WEST}, for
   * a right-to-left orientation, it is the same as {@link #EAST}.
   *
   * @since 1.4
   */
  public static final String LINE_START = BEFORE_LINE_BEGINS;

  /**
   * The constant indicating the position after the last item of the
   * layout.  The exact position depends on the writing system: For a
   * left-to-right orientation, it is the same as {@link #EAST}, for
   * a right-to-left orientation, it is the same as {@link #WEST}.
   *
   * @since 1.4
   */
  public static final String LINE_END = AFTER_LINE_ENDS;


  /**
   * Serialization constant.
   */
  private static final long serialVersionUID = -8658291919501921765L;


  /**
   * @serial
   */
  private Component north;

  /**
   * @serial
   */
  private Component south;

  /**
   * @serial
   */
  private Component east;

  /**
   * @serial
   */
  private Component west;

  /**
   * @serial
  */
  private Component center;

  /**
   * @serial
   */
  private Component firstLine;

  /**
   * @serial
   */
  private Component lastLine;

  /**
   * @serial
   */
  private Component firstItem;

  /**
   * @serial
   */
  private Component lastItem;

  /**
   * @serial The horizontal gap between components
   */
  private int hgap;

  /**
   * @serial The vertical gap between components
   */
  private int vgap;


  // Some constants for use with calcSize().
  private static final int MIN = 0;
  private static final int MAX = 1;
  private static final int PREF = 2;


  /**
   * Initializes a new instance of <code>BorderLayout</code> with no
   * horiztonal or vertical gaps between components.
   */
  public BorderLayout()
  {
    this(0,0);
  }

  /**
   * Initializes a new instance of <code>BorderLayout</code> with the
   * specified horiztonal and vertical gaps between components.
   *
   * @param hgap The horizontal gap between components.
   * @param vgap The vertical gap between components.
   */
  public BorderLayout(int hgap, int vgap)
  {
    this.hgap = hgap;
    this.vgap = vgap;
  }

  /**
   * Returns the horitzontal gap value.
   *
   * @return The horitzontal gap value.
   */
  public int getHgap()
  {
    return(hgap);
  }

  /**
   * Sets the horizontal gap to the specified value.
   *
   * @param hgap The new horizontal gap.
   */
  public void setHgap(int hgap)
  {
    this.hgap = hgap;
  }

  /**
   * Returns the vertical gap value.
   *
   * @return The vertical gap value.
   */
  public int getVgap()
  {
    return(vgap);
  }

  /**
   * Sets the vertical gap to the specified value.
   *
   * @param vgap The new vertical gap value.
   */
  public void setVgap(int vgap)
  {
    this.vgap = vgap;
  }

  /**
   * Adds a component to the layout in the specified constraint position, 
   * which must be one of the string constants defined in this class.
   *
   * @param component The component to add.
   * @param constraints The constraint string.
   *
   * @exception IllegalArgumentException If the constraint object is not
   * a string, or is not one of the specified constants in this class.
   */
  public void addLayoutComponent(Component component, Object constraints)
  {
    if (constraints != null && ! (constraints instanceof String))
      throw new IllegalArgumentException("Constraint must be a string");

    addLayoutComponent((String) constraints, component);
  }

  /**
   * Adds a component to the layout in the specified constraint position, 
   * which must be one of the string constants defined in this class.
   *
   * @param constraints The constraint string.
   * @param component The component to add.
   *
   * @exception IllegalArgumentException If the constraint object is not
   *            one of the specified constants in this class.
   *
   * @deprecated This method is deprecated in favor of
   *             <code>addLayoutComponent(Component, Object)</code>.
   */
  public void addLayoutComponent(String constraints, Component component)
  {
    String str = constraints;

    if (str == null || str.equals(CENTER))
      center = component;
    else if (str.equals(NORTH))
      north = component;
    else if (str.equals(SOUTH))
      south = component;
    else if (str.equals(EAST))
      east = component;
    else if (str.equals(WEST))
      west = component;
    else if (str.equals(BEFORE_FIRST_LINE))
      firstLine = component;
    else if (str.equals(AFTER_LAST_LINE))
      lastLine = component;
    else if (str.equals(BEFORE_LINE_BEGINS))
      firstItem = component;
    else if (str.equals(AFTER_LINE_ENDS))
      lastItem = component;
    else
      throw new IllegalArgumentException("Constraint value not valid: " + str);
  }

  /**
   * Removes the specified component from the layout.
   *
   * @param component The component to remove from the layout.
   */
  public void removeLayoutComponent(Component component)
  {
    if (north == component)
      north = null;
    if (south == component)
      south = null;
    if (east == component)
      east = null;
    if (west == component)
      west = null;
    if (center == component)
      center = null;
    if (firstItem == component)
      firstItem = null;
    if (lastItem == component)
      lastItem = null;
    if (firstLine == component)
      firstLine = null;
    if (lastLine == component)
      lastLine = null;
  }

  /**
   * Returns the minimum size of the specified container using this layout.
   *
   * @param target The container to calculate the minimum size for.
   *
   * @return The minimum size of the container
   */
  public Dimension minimumLayoutSize(Container target)
  {
    return calcSize(target, MIN);
  }

  /**
   * Returns the preferred size of the specified container using this layout.
   *
   * @param target The container to calculate the preferred size for.
   *
   * @return The preferred size of the container
   */
  public Dimension preferredLayoutSize(Container target)
  {
    return calcSize(target, PREF);
  }

  /**
   * Returns the maximum size of the specified container using this layout.
   *
   * @param target The container to calculate the maximum size for.
   *
   * @return The maximum size of the container
   */
  public Dimension maximumLayoutSize(Container target)
  {
    return new Dimension (Integer.MAX_VALUE, Integer.MAX_VALUE);
  }

  /**
   * Returns the X axis alignment, which is a <code>float</code> indicating
   * where along the X axis this container wishs to position its layout.
   * 0 indicates align to the left, 1 indicates align to the right, and 0.5
   * indicates align to the center.
   *
   * @param parent The parent container.
   *
   * @return The X alignment value.
   */
  public float getLayoutAlignmentX(Container parent)
  {
    return 0.5F;
  }

  /**
   * Returns the Y axis alignment, which is a <code>float</code> indicating
   * where along the Y axis this container wishs to position its layout.
   * 0 indicates align to the top, 1 indicates align to the bottom, and 0.5
   * indicates align to the center.
   *
   * @param parent The parent container.
   *
   * @return The Y alignment value.
   */
  public float getLayoutAlignmentY(Container parent)
  {
    return 0.5F;
  }

  /**
   * Instructs this object to discard any layout information it might
   * have cached.
   *
   * @param parent The parent container.
   */
  public void invalidateLayout(Container parent)
  {
    // Nothing to do here.
  }

  /**
   * Lays out the specified container according to the constraints in this
   * object.
   * 
   * @param target The container to lay out.
   */
  public void layoutContainer(Container target)
  {
    synchronized (target.getTreeLock())
      {
        Insets i = target.getInsets();
        int top = i.top;
        int bottom = target.height - i.bottom;
        int left = i.left;
        int right = target.width - i.right;

        boolean left_to_right = target.getComponentOrientation().isLeftToRight();

        Component my_north = north;
        Component my_east = east;
        Component my_south = south;
        Component my_west = west;

        // Note that we currently don't handle vertical layouts.
        // Neither does JDK 1.3.
        if (firstLine != null)
          my_north = firstLine;
        if (lastLine != null)
          my_south = lastLine;
        if (firstItem != null)
          {
            if (left_to_right)
              my_west = firstItem;
            else
              my_east = firstItem;
          }
        if (lastItem != null)
          {
            if (left_to_right)
              my_east = lastItem;
            else
              my_west = lastItem;
          }

        if (my_north != null)
          {
            Dimension n = calcCompSize(my_north, PREF);
            my_north.setBounds(left, top, right - left, n.height);
            top += n.height + vgap;
          }

        if (my_south != null)
          {
            Dimension s = calcCompSize(my_south, PREF);
            my_south.setBounds(left, bottom - s.height, right - left, s.height);
            bottom -= s.height + vgap;
          }

        if (my_east != null)
          {
            Dimension e = calcCompSize(my_east, PREF);
            my_east.setBounds(right - e.width, top, e.width, bottom - top);
            right -= e.width + hgap;
          }

        if (my_west != null)
          {
            Dimension w = calcCompSize(my_west, PREF);
            my_west.setBounds(left, top, w.width, bottom - top);
            left += w.width + hgap;
          }

        if (center != null)
          center.setBounds(left, top, right - left, bottom - top);
      }
  }

  /**
   * Returns a string representation of this layout manager.
   * 
   * @return A string representation of this object.
   */
  public String toString()
  {
    return getClass().getName() + "[hgap=" + hgap + ",vgap=" + vgap + "]";
  }

  private Dimension calcCompSize(Component comp, int what)
  {
    if (comp == null || ! comp.isVisible())
      return new Dimension(0, 0);
    if (what == MIN)
      return comp.getMinimumSize();
    else if (what == MAX)
      return comp.getMaximumSize();
    return comp.getPreferredSize();
  }

  /**
   * This is a helper function used to compute the various sizes for this
   * layout.
   */
  private Dimension calcSize(Container target, int what)
  {
    synchronized (target.getTreeLock())
      {
        Insets ins = target.getInsets();

        ComponentOrientation orient = target.getComponentOrientation ();
        boolean left_to_right = orient.isLeftToRight ();

        Component my_north = north;
        Component my_east = east;
        Component my_south = south;
        Component my_west = west;

        // Note that we currently don't handle vertical layouts.  Neither
        // does JDK 1.3.
        if (firstLine != null)
          my_north = firstLine;
        if (lastLine != null)
          my_south = lastLine;
        if (firstItem != null)
          {
            if (left_to_right)
              my_west = firstItem;
            else
              my_east = firstItem;
          }
        if (lastItem != null)
          {
            if (left_to_right)
              my_east = lastItem;
            else
              my_west = lastItem;
          }

        Dimension ndim = calcCompSize(my_north, what);
        Dimension sdim = calcCompSize(my_south, what);
        Dimension edim = calcCompSize(my_east, what);
        Dimension wdim = calcCompSize(my_west, what);
        Dimension cdim = calcCompSize(center, what);

        int width = edim.width + cdim.width + wdim.width + (hgap * 2);
        // Check for overflow.
        if (width < edim.width || width < cdim.width || width < cdim.width)
          width = Integer.MAX_VALUE;

        if (ndim.width > width)
          width = ndim.width;
        if (sdim.width > width)
          width = sdim.width;

        width += (ins.left + ins.right);

        int height = edim.height;
        if (cdim.height > height)
          height = cdim.height;
        if (wdim.height > height)
          height = wdim.height;

        int addedHeight = height + (ndim.height + sdim.height + (vgap * 2)
                                    + ins.top + ins.bottom);
        // Check for overflow.
        if (addedHeight < height)
          height = Integer.MAX_VALUE;
        else
          height = addedHeight;

        return(new Dimension(width, height));
      }
  }

  /**
   * Return the component at the indicated location, or null if no component
   * is at that location.  The constraints argument must be one of the 
   * location constants specified by this class.   
   * @param constraints the location
   * @return the component at that location, or null
   * @throws IllegalArgumentException if the constraints argument is not 
   * recognized
   * @since 1.5
   */
  public Component getLayoutComponent(Object constraints)
  {
    if (constraints == CENTER)
      return center;
    if (constraints == NORTH)
      return north;
    if (constraints == EAST)
      return east;
    if (constraints == SOUTH)
      return south;
    if (constraints == WEST)
      return west;
    if (constraints == PAGE_START)
      return firstLine;
    if (constraints == PAGE_END)
      return lastLine;
    if (constraints == LINE_START)
      return firstItem;
    if (constraints == LINE_END)
      return lastItem;
    throw new IllegalArgumentException("constraint " + constraints 
                                       + " is not recognized");
  }

  /**
   * Return the component at the specified location, which must be one
   * of the absolute constants such as CENTER or SOUTH.  The container's
   * orientation is used to map this location to the correct corresponding
   * component, so for instance in a right-to-left container, a request
   * for the EAST component could return the LINE_END component.  This will
   * return null if no component is available at the given location.
   * @param container the container whose orientation is used
   * @param constraints the absolute location of the component
   * @return the component at the location, or null
   * @throws IllegalArgumentException if the constraint is not recognized
   */
  public Component getLayoutComponent(Container container, Object constraints)
  {
    ComponentOrientation orient = container.getComponentOrientation();
    if (constraints == CENTER)
      return center;
    // Note that we don't support vertical layouts.
    if (constraints == NORTH)
      return north;
    if (constraints == SOUTH)
      return south;
    if (constraints == WEST)
      {
        // Note that relative layout takes precedence.
        if (orient.isLeftToRight())
          return firstItem == null ? west : firstItem;
        return lastItem == null ? west : lastItem;
      }
    if (constraints == EAST)
      {
        // Note that relative layout takes precedence.
        if (orient.isLeftToRight())
          return lastItem == null ? east : lastItem;
        return firstItem == null ? east : firstItem;
      }
    throw new IllegalArgumentException("constraint " + constraints
                                       + " is not recognized");
  }

  /**
   * Return the constraint corresponding to a component in this layout.
   * If the component is null, or is not in this layout, returns null.
   * Otherwise, this will return one of the constraint constants defined
   * in this class.
   * @param c the component
   * @return the constraint, or null
   * @since 1.5
   */
  public Object getConstraints(Component c)
  {
    if (c == null)
      return null;
    if (c == center)
      return CENTER;
    if (c == north)
      return NORTH;
    if (c == east)
      return EAST;
    if (c == south)
      return SOUTH;
    if (c == west)
      return WEST;
    if (c == firstLine)
      return PAGE_START;
    if (c == lastLine)
      return PAGE_END;
    if (c == firstItem)
      return LINE_START;
    if (c == lastItem)
      return LINE_END;
    return null;
  }
}
