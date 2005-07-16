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

import java.awt.Component;
import java.awt.Container;
import java.util.AbstractSequentialList;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.WeakHashMap;

/**
 * This class provides utility methods that are commonly used in AWT
 * (and Swing).
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
}
