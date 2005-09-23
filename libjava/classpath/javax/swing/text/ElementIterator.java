/* ElementIterator.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package javax.swing.text;

/**
 * This class can be used to iterate over the {@link Element} tree of
 * a {@link Document} or an {@link Element}.  This iterator performs
 * an "in-order" traversal -- first it visits a node, then each of the
 * node's children in order.  No locking is performed during the
 * iteration; that is up to the caller.
 */
public class ElementIterator implements Cloneable
{
  // The root element.
  private Element root;
  // The current element.
  private Element currentElement;
  // The depth to which we have descended in the tree.
  private int currentDepth;

  // This is at least as big as the current depth, and at index N
  // contains the index of the child element we're currently
  // examining.
  private int[] state;

  // The previous item.
  private Element previousItem;

  /**
   * Create a new ElementIterator to iterate over the given document.
   * @param document the Document over which we iterate
   */
  public ElementIterator(Document document)
  {
    this.root = document.getDefaultRootElement();
    this.currentElement = root;
    this.state = new int[5];
  }

  /**
   * Create a new ElementIterator to iterate over the given document.
   * @param root the Document over which we iterate
   */
  public ElementIterator(Element root)
  {
    this.root = root;
    this.currentElement = root;
    this.state = new int[5];
  }

  /**
   * Returns a new ElementIterator which is a clone of this
   * ElementIterator.
   */
  public Object clone()
  {
    try
      {
	return super.clone();
      }
    catch (CloneNotSupportedException _)
      {
	// Can't happen.
	return null;
      }
  }

  /**
   * Returns the current element.
   */
  public Element current()
  {
    return currentElement;
  }

  /**
   * Returns the depth to which we have descended in the tree.
   */
  public int depth()
  {
    return currentDepth;
  }

  /**
   * Returns the first element in the tree.
   */
  public Element first()
  {
    // Reset the iterator.
    currentElement = root;
    currentDepth = 0;
    previousItem = null;
    return root;
  }

  /**
   * Advance the iterator and return the next element of the tree,
   * performing an "in-order" traversal.
   */
  public Element next()
  {
    previousItem = currentElement;
    if (currentElement == null)
      return null;
    if (! currentElement.isLeaf())
      {
	++currentDepth;
	if (currentDepth > state.length)
	  {
	    int[] newState = new int[state.length * 2];
	    System.arraycopy(state, 0, newState, 0, state.length);
	    state = newState;
	  }
	state[currentDepth] = 0;
	currentElement = currentElement.getElement(0);
	return currentElement;
      }

    while (currentDepth > 0)
      {
	// At a leaf, or done with a non-leaf's children, so go up a
	// level.
	--currentDepth;
	currentElement = currentElement.getParentElement();
	++state[currentDepth];
	if (state[currentDepth] < currentElement.getElementCount())
	  {
	    currentElement = currentElement.getElement(state[currentDepth]);
	    return currentElement;
	  }
      }

    currentElement = null;
    return currentElement;
  }

  /**
   * Returns the previous item.  Does not modify the iterator state.
   */
  public Element previous()
  {
    if (currentElement == null || currentElement == root)
      return null;
    return previousItem;
  }
}
