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
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import gnu.java.awt.AWTUtilities;

/**
 * A layout for swing components.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 * @author Roman Kennke (roman@kennke.org)
 */
public class BoxLayout implements LayoutManager2, Serializable
{

  /**
   * This is an abstraction that allows the BoxLayout algorithm to
   * be applied to both direction (X and Y) without duplicating the
   * algorithm. It defines several methods that access properties of
   * a component for a specific direction.
   */
  static interface Direction
  {
    /**
     * Returns the correct part of <code>d</code> for this direction. This will
     * be <code>d.width</code> for horizontal and <code>d.height</code> for
     * vertical direction.
     *
     * @param d the size as Dimension object
     *
     * @return the correct part of <code>d</code> for this direction
     */
    int size(Dimension d);

    /**
     * Returns the lower bounds of the {@link Insets} object according to this
     * direction. This will be <code>insets.top</code> for vertical direction
     * and <code>insets.left</code> for horizontal direction.
     *
     * @param the {@link Insets} object from which to return the lower bounds
     *
     * @return the lower bounds of the {@link Insets} object according to this
     *     direction
     */
    int lower(Insets insets);

    /**
     * Returns the alignment property according to this direction.
     *
     * @param comp the Component for which to return the alignment property
     *
     * @return the alignment property according to this direction
     */
    float alignment(Component comp);

    /**
     * Sets the location for Component <code>c</code>. <code>coord1</code>
     * specifies the coordinate of the location in this direction,
     * <code>coord2</code> the coordinate of the location in the opposite
     * direction.
     *
     * @param c the Component for which to set the location
     * @param coord1 the coordinate in this direction
     * @param coord2 the coordinate in the opposite direction
     */
    void setLocation(Component c, int coord1, int coord2);

    /**
     * Sets the size for Component <code>c</code>. <code>coord1</code>
     * specifies the size in this direction,
     * <code>coord2</code> the size in the opposite
     * direction.
     *
     * @param c the Component for which to set the size
     * @param size1 the size in this direction
     * @param size2 the size in the opposite direction
     */
    void setSize(Component c, int size1, int size2);
  }

  /**
   * The horizontal direction.
   */
  static class Horizontal implements Direction
  {
    /**
     * Returns the correct part of <code>d</code> for this direction. This will
     * be <code>d.width</code> for horizontal and <code>d.height</code> for
     * vertical direction.
     *
     * @param d the size as Dimension object
     *
     * @return the correct part of <code>d</code> for this direction
     */
    public int size(Dimension d)
    {
      return d.width;
    }

    /**
     * Returns the lower bounds of the {@link Insets} object according to this
     * direction. This will be <code>insets.top</code> for vertical direction
     * and <code>insets.left</code> for horizontal direction.
     *
     * @param the {@link Insets} object from which to return the lower bounds
     *
     * @return the lower bounds of the {@link Insets} object according to this
     *     direction
     */
    public int lower(Insets insets)
    {
      return insets.left;
    }

    /**
     * Returns the alignment property according to this direction.
     *
     * @param comp the Component for which to return the alignment property
     *
     * @return the alignment property according to this direction
     */
    public float alignment(Component comp)
    {
      return comp.getAlignmentX();
    }

    /**
     * Sets the location for Component <code>c</code>. <code>coord1</code>
     * specifies the coordinate of the location in this direction,
     * <code>coord2</code> the coordinate of the location in the opposite
     * direction.
     *
     * @param c the Component for which to set the location
     * @param coord1 the coordinate in this direction
     * @param coord2 the coordinate in the opposite direction
     */
    public void setLocation(Component c, int coord1, int coord2)
    {
      c.setLocation(coord1, coord2);
    }

    /**
     * Sets the size for Component <code>c</code>. <code>coord1</code>
     * specifies the size in this direction,
     * <code>coord2</code> the size in the opposite
     * direction.
     *
     * @param c the Component for which to set the size
     * @param size1 the size in this direction
     * @param size2 the size in the opposite direction
     */
    public void setSize(Component c, int size1, int size2)
    {
      c.setSize(size1, size2);
    }
  }
  /**
   * The vertical direction.
   */
  static class Vertical implements Direction
  {
    /**
     * Returns the correct part of <code>d</code> for this direction. This will
     * be <code>d.width</code> for horizontal and <code>d.height</code> for
     * vertical direction.
     *
     * @param d the size as Dimension object
     *
     * @return the correct part of <code>d</code> for this direction
     */
    public int size(Dimension d)
    {
      return d.height;
    }

    /**
     * Returns the lower bounds of the {@link Insets} object according to this
     * direction. This will be <code>insets.top</code> for vertical direction
     * and <code>insets.left</code> for horizontal direction.
     *
     * @param the {@link Insets} object from which to return the lower bounds
     *
     * @return the lower bounds of the {@link Insets} object according to this
     *     direction
     */
    public int lower(Insets insets)
    {
      return insets.top;
    }

    /**
     * Returns the alignment property according to this direction.
     *
     * @param comp the Component for which to return the alignment property
     *
     * @return the alignment property according to this direction
     */
    public float alignment(Component comp)
    {
      return comp.getAlignmentY();
    }

    /**
     * Sets the location for Component <code>c</code>. <code>coord1</code>
     * specifies the coordinate of the location in this direction,
     * <code>coord2</code> the coordinate of the location in the opposite
     * direction.
     *
     * @param c the Component for which to set the location
     * @param coord1 the coordinate in this direction
     * @param coord2 the coordinate in the opposite direction
     */
    public void setLocation(Component c, int coord1, int coord2)
    {
      c.setLocation(coord2, coord1);
    }

    /**
     * Sets the size for Component <code>c</code>. <code>coord1</code>
     * specifies the size in this direction,
     * <code>coord2</code> the size in the opposite
     * direction.
     *
     * @param c the Component for which to set the size
     * @param size1 the size in this direction
     * @param size2 the size in the opposite direction
     */
    public void setSize(Component c, int size1, int size2)
    {
      c.setSize(size2, size1);
    }
  }

  /**
   * A helper class that temporarily stores the size specs of a component.
   */
  static class SizeReq
  {
    int size;
    int min;
    int pref;
    int max;
    float align;
    Component comp;
    SizeReq(Component comp, Direction dir)
    {
      this.min = dir.size(comp.getMinimumSize());
      this.pref = dir.size(comp.getPreferredSize());
      this.max = dir.size(comp.getMaximumSize());
      this.size = dir.size(comp.getSize());
      this.align = dir.alignment(comp);
      this.comp = comp;
    }
  }

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

  /** Constant for the horizontal direction. */
  private static final Direction HORIZONTAL = new Horizontal();

  /** Constant for the vertical direction. */
  private static final Direction VERTICAL = new Vertical();

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

    List children = AWTUtilities.getVisibleChildren(parent);

    if (isHorizontalIn(parent))
      {        
        x = insets.left + insets.right;
        // sum up preferred widths of components, find maximum of preferred
        // heights
        for (Iterator i = children.iterator(); i.hasNext();)
          {
            Component comp = (Component) i.next();
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
        for (Iterator i = children.iterator(); i.hasNext();)
          {
            Component comp = (Component) i.next();
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

    List children = AWTUtilities.getVisibleChildren(parent);

    if (isHorizontalIn(parent))
      {
        // sum up preferred widths of components, find maximum of preferred
        // heights
        for (Iterator i = children.iterator(); i.hasNext();)
          {
	    Component comp = (Component) i.next();
            Dimension sz = comp.getMinimumSize();
            x += sz.width;
            y = Math.max(y, sz.height);
          }
      }
    else
      {
        // sum up preferred heights of components, find maximum of
        //  preferred widths
        for (Iterator i = children.iterator(); i.hasNext();)
          {
	    Component comp = (Component) i.next();
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
    if (isHorizontalIn(parent))
      layoutAlgorithm(parent, HORIZONTAL, VERTICAL);
    else
      layoutAlgorithm(parent, VERTICAL, HORIZONTAL);
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

    List children = AWTUtilities.getVisibleChildren(parent);

    if (isHorizontalIn(parent))
      {
        
        // sum up preferred widths of components, find maximum of preferred
        // heights
        for (Iterator i = children.iterator(); i.hasNext();)
          {
            Component comp = (Component) i.next();
            Dimension sz = comp.getMaximumSize();
            x += sz.width;
            // Check for overflow.
            if (x < 0)
              x = Integer.MAX_VALUE;
            y = Math.max(y, sz.height);
          }
      }
    else
      {
        // sum up preferred heights of components, find maximum of
        //  preferred widths
        for (Iterator i = children.iterator(); i.hasNext();)
          {
            Component comp = (Component) i.next();
            Dimension sz = comp.getMaximumSize();
            y += sz.height;
            // Check for overflow
            if (y < 0)
              y = Integer.MAX_VALUE;
            x = Math.max(x, sz.width);
          }
      } 
    return new Dimension(x, y);
  }

  /**
   * Lays out the Container <code>c</code> in the layout direction
   * <code>layoutDir</code>. The direction that is crossing the layout
   * direction is specified in <code>crossDir</code>.
   *
   * @param parent
   * @param layoutDir
   * @param crossDir
   */
  void layoutAlgorithm(Container parent, Direction layoutDir, Direction crossDir)
  {
    if (parent != container)
      throw new AWTError("invalid parent");

    Dimension parentSize = parent.getSize();
    Insets insets = parent.getInsets();
    Dimension innerSize = new Dimension(parentSize.width - insets.left
                                        - insets.right, parentSize.height
                                        - insets.bottom - insets.top);

    // Set all components to their preferredSizes and sum up the allocated
    // space. Create SizeReqs for each component and store them in
    // sizeReqs. Find the maximum size in the crossing direction.
    List children = AWTUtilities.getVisibleChildren(parent);
    Vector sizeReqs = new Vector();
    int allocated = 0;
    for (Iterator i = children.iterator(); i.hasNext();)
      {
	Component c = (Component) i.next();
	SizeReq sizeReq = new SizeReq(c, layoutDir);
	int preferred = layoutDir.size(c.getPreferredSize());
	sizeReq.size = preferred;
	allocated += preferred;
	sizeReqs.add(sizeReq);
      }

    // Distribute remaining space (may be positive or negative) over components
    int remainder = layoutDir.size(innerSize) - allocated;
    distributeSpace(sizeReqs, remainder, layoutDir);

    // Resize and relocate components. If the component can be sized to
    // take the full space in the crossing direction, then do so, otherwise
    // align according to its alingnmentX or alignmentY property.
    int loc = 0;
    int offset1 = layoutDir.lower(insets);
    int offset2 = crossDir.lower(insets);
    for (Iterator i = sizeReqs.iterator(); i.hasNext();)
      {
	SizeReq sizeReq = (SizeReq) i.next();
	Component c = sizeReq.comp;
	int availCrossSize = crossDir.size(innerSize);
	int maxCross = crossDir.size(c.getMaximumSize());
	int crossSize = Math.min(availCrossSize, maxCross);
	int crossRemainder = availCrossSize - crossSize;
	int crossLoc = (int) (crossDir.alignment(c) * crossRemainder);
	layoutDir.setSize(c, sizeReq.size, crossSize);
	layoutDir.setLocation(c, offset1 + loc, offset2 + crossLoc);
	loc += sizeReq.size;
      }
  }

  /**
   * Distributes some space over a set of components. This implementation
   * tries to set the components as close as possible to their
   * <code>preferredSize</code>s, and respects the components
   * <code>minimumSize</code> and <code>maximumSize</code>.
   *
   * The algorithm is implemented as follows:
   *
   * <ul>
   * <li>The <code>remainder</code> is divided by the number of components
   * in <code>freeComponents</code>.</li>
   * <li>The result is added to (or substracted from) the size of each
   * component.</li>
   * <li>If the <code>minimumSize</code> or <code>maximumSize</code> of a
   * component is exceeded, then this component is set to its
   * <code>minimumSize</code> or <code>maximumSize</code>, it is removed from
   * <code>freeComponents</code> and the difference is added to a new
   * remainder.</li>
   * <li>Finally, if there is a new remainer != 0 and the
   * <code>freeComponents.size() != 0</code>, then this method is called
   * recursivly to distribute the newly allocated remaining space.</li>
   * </ul>
   *
   * @param freeComponents a SizeReq collection for components that have space
   *     left so that they can be moved freely
   * @param remainder the space that should be distributed between the
   *     components
   * @param dir the direction in which we operate
   */
  void distributeSpace(Collection freeComponents, int remainder, Direction dir)
  {
    // Sum up total available space in components. If the remainder is negative
    // then we sum up the difference between minSize and size. If remainder
    // is positive we sum up the difference between maxSize and size.
    double totalAvailable = 0;
    for (Iterator i = freeComponents.iterator(); i.hasNext();)
      {
        SizeReq sizeReq = (SizeReq) i.next();
        if (remainder >= 0)
          totalAvailable += sizeReq.max - sizeReq.size;
        else
          totalAvailable += sizeReq.min - sizeReq.size;
      }
    if (totalAvailable == 0)
      if (remainder >= 0)
        totalAvailable = 1;
      else
        totalAvailable = -1;

    int newRemainder = 0;
    Vector stillFree = new Vector();
    for (Iterator i = freeComponents.iterator(); i.hasNext();)
      {
	// Add/substract share to component.
	SizeReq sizeReq = (SizeReq) i.next();
        double available = 0;
        if (remainder >= 0)
          available = sizeReq.max - sizeReq.size;
        else
          available = sizeReq.min - sizeReq.size;
        int share = (int) ((available / totalAvailable) * remainder);
	sizeReq.size += share;
	// check for min/maximumSize
	if (sizeReq.size < sizeReq.min)
	  {
	    newRemainder += sizeReq.size - sizeReq.min;
	    sizeReq.size = sizeReq.min;
	  }
	else if (sizeReq.size > sizeReq.max)
	  {
	    newRemainder += sizeReq.size - sizeReq.max;
	    sizeReq.size = sizeReq.max;
	  }
	else
	  stillFree.add(sizeReq);
      }
    // recursivly call this method if necessary
    if (newRemainder != 0 && stillFree.size() > 0)
      distributeSpace(stillFree, newRemainder, dir);
  }
}
