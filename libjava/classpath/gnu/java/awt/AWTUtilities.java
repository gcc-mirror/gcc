/* AWTUtilities.java -- Common utility methods for AWT and Swing.
   Copyright (C) 2005  Free Software Foundation, Inc.

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

package gnu.java.awt;

import java.applet.Applet;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.util.AbstractSequentialList;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.WeakHashMap;
import java.lang.reflect.InvocationTargetException;

/**
 * This class mirrors the javax.swing.SwingUtilities class. It 
 * provides commonly needed functionalities for AWT classes without
 * the need to reference classes in the javax.swing package.
 */
public class AWTUtilities
{

  /**
   * This List implementation wraps the Component[] returned by
   * {@link Container#getComponents()} and iterates over the visible Components
   * in that array. This class is used in {@link #getVisibleChildren}.
   */
  static class VisibleComponentList extends AbstractSequentialList
  {
    /**
     * The ListIterator for this List.
     */
    class VisibleComponentIterator implements ListIterator
    {
      /** The current index in the Component[]. */
      int index;

      /** The index in the List of visible Components. */
      int listIndex;

      /**
       * Creates a new VisibleComponentIterator that starts at the specified
       * <code>listIndex</code>. The array of Components is searched from
       * the beginning to find the matching array index.
       *
       * @param listIndex the index from where to begin iterating
       */
      VisibleComponentIterator(int listIndex)
      {
	this.listIndex = listIndex;
	int visibleComponentsFound = 0;
	for (index = 0; visibleComponentsFound != listIndex; index++)
	  {
	    if (components[index].isVisible())
	      visibleComponentsFound++;
	  }
      }

      /**
       * Returns <code>true</code> if there are more visible components in the
       * array, <code>false</code> otherwise.
       *
       * @return <code>true</code> if there are more visible components in the
       *     array, <code>false</code> otherwise
       */
      public boolean hasNext()
      {
	boolean hasNext = false;
	for (int i = index; i < components.length; i++)
	  {
	    if (components[i].isVisible())
	      {
		hasNext = true;
		break;
	      }
	  }
	return hasNext;
      }

      /**
       * Returns the next visible <code>Component</code> in the List.
       *
       * @return the next visible <code>Component</code> in the List
       *
       * @throws if there is no next element
       */
      public Object next()
      {
	Object o = null;
	for (; index < components.length; index++)
	  {
	    if (components[index].isVisible())
	      {
		o = components[index];
		break;
	      }
	  }
	if (o != null)
	  {
	    index++;
	    listIndex++;
	    return o;
	  }
	else
	  throw new NoSuchElementException();
      }

      /**
       * Returns <code>true</code> if there are more visible components in the
       * array in the reverse direction, <code>false</code> otherwise.
       *
       * @return <code>true</code> if there are more visible components in the
       *     array in the reverse direction, <code>false</code> otherwise
       */
      public boolean hasPrevious()
      {
	boolean hasPrevious = false;
	for (int i = index - 1; i >= 0; i--)
	  {
	    if (components[i].isVisible())
	      {
		hasPrevious = true;
		break;
	      }
	  }
	return hasPrevious;
      }

      /**
       * Returns the previous visible <code>Component</code> in the List.
       *
       * @return the previous visible <code>Component</code> in the List
       *
       * @throws NoSuchElementException if there is no previous element
       */
      public Object previous()
      {
	Object o = null;
	for (index--; index >= 0; index--)
	  {
	    if (components[index].isVisible())
	      {
		o = components[index];
		break;
	      }
	  }
	if (o != null)
	  {
	    listIndex--;
	    return o;
	  }
	else
	  throw new NoSuchElementException();
      }

      /**
       * Returns the index of the next element in the List.
       *
       * @return the index of the next element in the List
       */
      public int nextIndex()
      {
	return listIndex + 1;
      }

      /**
       * Returns the index of the previous element in the List.
       *
       * @return the index of the previous element in the List
       */
      public int previousIndex()
      {
	return listIndex - 1;
      }

      /**
       * This operation is not supported because the List is immutable.
       *
       * @throws UnsupportedOperationException because the List is immutable
       */
      public void remove()
      {
	throw new UnsupportedOperationException
	  ("VisibleComponentList is immutable");
      }

      /**
       * This operation is not supported because the List is immutable.
       *
       * @param o not used here
       *
       * @throws UnsupportedOperationException because the List is immutable
       */
      public void set(Object o)
      {
	throw new UnsupportedOperationException
	  ("VisibleComponentList is immutable");
      }

      /**
       * This operation is not supported because the List is immutable.
       *
       * @param o not used here
       *
       * @throws UnsupportedOperationException because the List is immutable
       */
      public void add(Object o)
      {
	throw new UnsupportedOperationException
	  ("VisibleComponentList is immutable");
      }
    }

    /**
     * The components over which we iterate. Only the visible components
     * are returned by this List.
     */
    Component[] components;

    /**
     * Creates a new instance of VisibleComponentList that wraps the specified
     * <code>Component[]</code>.
     *
     * @param c the <code>Component[]</code> to be wrapped.
     */
    VisibleComponentList(Component[] c)
    {
      components = c;
    }

    /**
     * Returns a {@link ListIterator} for iterating over this List.
     *
     * @return a {@link ListIterator} for iterating over this List
     */
    public ListIterator listIterator(int index)
    {
      return new VisibleComponentIterator(index);
    }

    /**
     * Returns the number of visible components in the wrapped Component[].
     *
     * @return the number of visible components
     */
    public int size()
    {
      int visibleComponents = 0;
      for (int i = 0; i < components.length; i++)
	if (components[i].isVisible())
	  visibleComponents++;
      return visibleComponents;
    }
  }

  /**
   * The cache for our List instances. We try to hold one instance of
   * VisibleComponentList for each Component[] that is requested. Note
   * that we use a WeakHashMap for caching, so that the cache itself
   * does not keep the array or the List from beeing garbage collected
   * if no other objects hold references to it.
   */
  static WeakHashMap visibleChildrenCache = new WeakHashMap();

  /**
   * Returns the visible children of a {@link Container}. This method is
   * commonly needed in LayoutManagers, because they only have to layout
   * the visible children of a Container.
   *
   * @param c the Container from which to extract the visible children
   *
   * @return the visible children of <code>c</code>
   */
  public static List getVisibleChildren(Container c)
  {
    Component[] children = c.getComponents();
    Object o = visibleChildrenCache.get(children);
    VisibleComponentList visibleChildren = null;
    if (o == null)
      {
	visibleChildren = new VisibleComponentList(children);
	visibleChildrenCache.put(children, visibleChildren);
      }
    else
      visibleChildren = (VisibleComponentList) o;

    return visibleChildren;
  }

  /**
   * Calculates the portion of the base rectangle which is inside the
   * insets.
   *
   * @param base The rectangle to apply the insets to
   * @param insets The insets to apply to the base rectangle
   * @param ret A rectangle to use for storing the return value, or
   * <code>null</code>
   *
   * @return The calculated area inside the base rectangle and its insets,
   * either stored in ret or a new Rectangle if ret is <code>null</code>
   *
   * @see #calculateInnerArea
   */
  public static Rectangle calculateInsetArea(Rectangle base, Insets insets,
                                             Rectangle ret)
  {
    if (ret == null)
      ret = new Rectangle();
    ret.setBounds(base.x + insets.left, base.y + insets.top,
        base.width - (insets.left + insets.right),
        base.height - (insets.top + insets.bottom));
    return ret;
  }

  /**
   * Calculates the bounds of a component in the component's own coordinate
   * space. The result has the same height and width as the component's
   * bounds, but its location is set to (0,0).
   *
   * @param aComponent The component to measure
   *
   * @return The component's bounds in its local coordinate space
   */
  public static Rectangle getLocalBounds(Component aComponent)
  {
    Rectangle bounds = aComponent.getBounds();
    return new Rectangle(0, 0, bounds.width, bounds.height);
  }

  /**
   * Returns the font metrics object for a given font. The metrics can be
   * used to calculate crude bounding boxes and positioning information,
   * for laying out components with textual elements.
   *
   * @param font The font to get metrics for
   *
   * @return The font's metrics
   *
   * @see java.awt.font.GlyphMetrics
   */
  public static FontMetrics getFontMetrics(Font font)
  {
    return Toolkit.getDefaultToolkit().getFontMetrics(font);
  }

  /**
   * Returns the least ancestor of <code>comp</code> which has the
   * specified name.
   *
   * @param name The name to search for
   * @param comp The component to search the ancestors of
   *
   * @return The nearest ancestor of <code>comp</code> with the given
   * name, or <code>null</code> if no such ancestor exists
   *
   * @see java.awt.Component#getName
   * @see #getAncestorOfClass
   */
  public static Container getAncestorNamed(String name, Component comp)
  {
    while (comp != null && (comp.getName() != name))
      comp = comp.getParent();
    return (Container) comp;
  }

  /**
   * Returns the least ancestor of <code>comp</code> which is an instance
   * of the specified class.
   *
   * @param c The class to search for
   * @param comp The component to search the ancestors of
   *
   * @return The nearest ancestor of <code>comp</code> which is an instance
   * of the given class, or <code>null</code> if no such ancestor exists
   *
   * @see #getAncestorOfClass
   * @see #windowForComponent
   * @see 
   * 
   */
  public static Container getAncestorOfClass(Class c, Component comp)
  {
    while (comp != null && (! c.isInstance(comp)))
      comp = comp.getParent();
    return (Container) comp;
  }

  /**
   * Equivalent to calling <code>getAncestorOfClass(Window, comp)</code>.
   *
   * @param comp The component to search for an ancestor window 
   *
   * @return An ancestral window, or <code>null</code> if none exists
   */
  public static Window windowForComponent(Component comp)
  {
    return (Window) getAncestorOfClass(Window.class, comp);
  }

  /**
   * Returns the "root" of the component tree containint <code>comp</code>
   * The root is defined as either the <em>least</em> ancestor of
   * <code>comp</code> which is a {@link Window}, or the <em>greatest</em>
   * ancestor of <code>comp</code> which is a {@link Applet} if no {@link
   * Window} ancestors are found.
   *
   * @param comp The component to search for a root
   *
   * @return The root of the component's tree, or <code>null</code>
   */
  public static Component getRoot(Component comp)
  {
    Applet app = null;
    Window win = null;

    while (comp != null)
     {
      if (win == null && comp instanceof Window)
        win = (Window) comp;
      else if (comp instanceof Applet)
        app = (Applet) comp;
      comp = comp.getParent();
    }

    if (win != null)
      return win;
    else
      return app;
  }

  /**
   * Return true if a descends from b, in other words if b is an
   * ancestor of a.
   *
   * @param a The child to search the ancestry of
   * @param b The potential ancestor to search for
   *
   * @return true if a is a descendent of b, false otherwise
   */
  public static boolean isDescendingFrom(Component a, Component b)
  {
    while (true)
     {
      if (a == null || b == null)
        return false;
      if (a == b)
        return true;
      a = a.getParent();
    }
  }

  /**
   * Returns the deepest descendent of parent which is both visible and
   * contains the point <code>(x,y)</code>. Returns parent when either
   * parent is not a container, or has no children which contain
   * <code>(x,y)</code>. Returns <code>null</code> when either
   * <code>(x,y)</code> is outside the bounds of parent, or parent is
   * <code>null</code>.
   * 
   * @param parent The component to search the descendents of
   * @param x Horizontal coordinate to search for
   * @param y Vertical coordinate to search for
   *
   * @return A component containing <code>(x,y)</code>, or
   * <code>null</code>
   *
   * @see java.awt.Container#findComponentAt
   */
  public static Component getDeepestComponentAt(Component parent, int x, int y)
  {
    if (parent == null || (! parent.contains(x, y)))
      return null;

    if (! (parent instanceof Container))
      return parent;

    Container c = (Container) parent;
    return c.findComponentAt(x, y);
  }

  /**
   * Converts a point from a component's local coordinate space to "screen"
   * coordinates (such as the coordinate space mouse events are delivered
   * in). This operation is equivalent to translating the point by the
   * location of the component (which is the origin of its coordinate
   * space).
   *
   * @param p The point to convert
   * @param c The component which the point is expressed in terms of
   *
   * @see convertPointFromScreen
   */
  public static void convertPointToScreen(Point p, Component c)
  {
    Point c0 = c.getLocationOnScreen();
    p.translate(c0.x, c0.y);
  }

  /**
   * Converts a point from "screen" coordinates (such as the coordinate
   * space mouse events are delivered in) to a component's local coordinate
   * space. This operation is equivalent to translating the point by the
   * negation of the component's location (which is the origin of its
   * coordinate space).
   *
   * @param p The point to convert
   * @param c The component which the point should be expressed in terms of
   */
  public static void convertPointFromScreen(Point p, Component c)
  {
    Point c0 = c.getLocationOnScreen();
    p.translate(-c0.x, -c0.y);
  }

  /**
   * Converts a point <code>(x,y)</code> from the coordinate space of one
   * component to another. This is equivalent to converting the point from
   * <code>source</code> space to screen space, then back from screen space
   * to <code>destination</code> space. If exactly one of the two
   * Components is <code>null</code>, it is taken to refer to the root
   * ancestor of the other component. If both are <code>null</code>, no
   * transformation is done.
   *
   * @param source The component which the point is expressed in terms of
   * @param x Horizontal coordinate of point to transform
   * @param y Vertical coordinate of point to transform
   * @param destination The component which the return value will be
   * expressed in terms of
   *
   * @return The point <code>(x,y)</code> converted from the coordinate
   *         space of the
   * source component to the coordinate space of the destination component
   *
   * @see #convertPointToScreen
   * @see #convertPointFromScreen
   * @see #convertRectangle
   * @see #getRoot
   */
  public static Point convertPoint(Component source, int x, int y,
                                   Component destination)
  {
    Point pt = new Point(x, y);

    if (source == null && destination == null)
      return pt;

    if (source == null)
      source = getRoot(destination);

    if (destination == null)
      destination = getRoot(source);

    convertPointToScreen(pt, source);
    convertPointFromScreen(pt, destination);

    return pt;
  }

  
  /**
   * Converts a rectangle from the coordinate space of one component to
   * another. This is equivalent to converting the rectangle from
   * <code>source</code> space to screen space, then back from screen space
   * to <code>destination</code> space. If exactly one of the two
   * Components is <code>null</code>, it is taken to refer to the root
   * ancestor of the other component. If both are <code>null</code>, no
   * transformation is done.
   *
   * @param source The component which the rectangle is expressed in terms of
   * @param rect The rectangle to convert
   * @param destination The component which the return value will be
   * expressed in terms of
   *
   * @return A new rectangle, equal in size to the input rectangle, but
   * with its position converted from the coordinate space of the source
   * component to the coordinate space of the destination component
   *
   * @see #convertPointToScreen
   * @see #convertPointFromScreen
   * @see #convertPoint
   * @see #getRoot
   */
  public static Rectangle convertRectangle(Component source, Rectangle rect,
                                           Component destination)
  {
    Point pt = convertPoint(source, rect.x, rect.y, destination);
    return new Rectangle(pt.x, pt.y, rect.width, rect.height);
  }

  /**
   * Convert a mouse event which refrers to one component to another.  This
   * includes changing the mouse event's coordinate space, as well as the
   * source property of the event. If <code>source</code> is
   * <code>null</code>, it is taken to refer to <code>destination</code>'s
   * root component. If <code>destination</code> is <code>null</code>, the
   * new event will remain expressed in <code>source</code>'s coordinate
   * system.
   *
   * @param source The component the mouse event currently refers to
   * @param sourceEvent The mouse event to convert
   * @param destination The component the new mouse event should refer to
   *
   * @return A new mouse event expressed in terms of the destination
   * component's coordinate space, and with the destination component as
   * its source
   *
   * @see #convertPoint
   */
  public static MouseEvent convertMouseEvent(Component source,
                                             MouseEvent sourceEvent,
                                             Component destination)
  {
    Point newpt = convertPoint(source, sourceEvent.getX(), sourceEvent.getY(),
                               destination);

    return new MouseEvent(destination, sourceEvent.getID(),
                          sourceEvent.getWhen(), sourceEvent.getModifiers(),
                          newpt.x, newpt.y, sourceEvent.getClickCount(),
                          sourceEvent.isPopupTrigger(),
                          sourceEvent.getButton());
  }


  /** 
   * Calls {@link java.awt.EventQueue.invokeLater} with the
   * specified {@link Runnable}. 
   */
  public static void invokeLater(Runnable doRun)
  {
    java.awt.EventQueue.invokeLater(doRun);
  }

  /** 
   * Calls {@link java.awt.EventQueue.invokeAndWait} with the
   * specified {@link Runnable}. 
   */
  public static void invokeAndWait(Runnable doRun)
  throws InterruptedException,
  InvocationTargetException
  {
    java.awt.EventQueue.invokeAndWait(doRun);
  }

  /** 
   * Calls {@link java.awt.EventQueue.isEventDispatchThread}.
   */
  public static boolean isEventDispatchThread()
  {
    return java.awt.EventQueue.isDispatchThread();
  }
}
