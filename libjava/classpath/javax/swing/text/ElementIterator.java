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

import java.util.Stack;

/**
 * This class can be used to iterate over the {@link Element} tree of
 * a {@link Document} or an {@link Element}.  This iterator performs
 * an "in-order" traversal -- first it visits a node, then each of the
 * node's children in order.  No locking is performed during the
 * iteration; that is up to the caller.
 */
public class ElementIterator implements Cloneable
{
  /**
   * Uses to track the iteration on the stack. 
   */
  private class ElementRef
  {
    /**
     * The element.
     */
    Element element;

    /**
     * The child index. -1 means the element itself. >= 0 values mean the
     * n-th child of the element.
     */
    int index;

    /**
     * Creates a new ElementRef.
     *
     * @param el the element
     */
    ElementRef(Element el)
    {
      element = el;
      index = -1;
    }
  }

  // The root element.
  private Element root;

  /**
   * Holds ElementRefs.
   */
  private Stack stack;

  /**
   * Create a new ElementIterator to iterate over the given document.
   * @param document the Document over which we iterate
   */
  public ElementIterator(Document document)
  {
    root = document.getDefaultRootElement();
  }

  /**
   * Create a new ElementIterator to iterate over the given document.
   * @param root the Document over which we iterate
   */
  public ElementIterator(Element root)
  {
    this.root = root;
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
    Element current;
    if (stack == null)
      current = first();
    else
      {
        current = null;
        if (! stack.isEmpty())
          {
            ElementRef ref = (ElementRef) stack.peek();
            Element el = ref.element;
            int index = ref.index;
            if (index == -1)
              current = el;
            else
              current = el.getElement(index);
          }
      }
    return current;
  }

  /**
   * Returns the depth to which we have descended in the tree.
   */
  public int depth()
  {
    int depth = 0;
    if (stack != null)
      depth = stack.size();
    return depth;
  }

  /**
   * Returns the first element in the tree.
   */
  public Element first()
  {
    Element first = null;
    if (root != null)
      {
        stack = new Stack();
        if (root.getElementCount() > 0)
          stack.push(new ElementRef(root));
        first = root;
      }
    return first;
  }

  /**
   * Advance the iterator and return the next element of the tree,
   * performing an "in-order" traversal.
   */
  public Element next()
  {
    Element next;
    if (stack == null)
      next = first();
    else
      {
        next = null;
        if (! stack.isEmpty())
          {
            ElementRef ref = (ElementRef) stack.peek();
            Element el = ref.element;
            int index = ref.index;
            if (el.getElementCount() > index + 1)
              {
                Element child = el.getElement(index + 1);
                if (child.isLeaf())
                  ref.index++;
                else
                  stack.push(new ElementRef(child));
                next = child;
                next = child;
              }
            else
              {
                stack.pop();
                if (! stack.isEmpty())
                  {
                    ElementRef top = (ElementRef) stack.peek();
                    top.index++;
                    next = next();
                  }
              }
          }
        // else return null.
      }
    return next;
  }

  /**
   * Returns the previous item.  Does not modify the iterator state.
   */
  public Element previous()
  {
    Element previous = null;
    int stackSize;
    if (stack != null && (stackSize = stack.size()) > 0)
      {
        ElementRef ref = (ElementRef) stack.peek();
        Element el = ref.element;
        int index = ref.index;
        if (index > 0)
          {
            previous = deepestLeaf(el.getElement(--index));
          }
        else if (index == 0)
          {
            previous = el;
          }
        else if (index == -1)
          {
            ElementRef top = (ElementRef) stack.pop();
            ElementRef item = (ElementRef) stack.peek();
            stack.push(top);
            index = item.index;
            el = item.element;
            previous = index == -1 ? el : deepestLeaf(el.getElement(index));
          }
      }
    return previous;
  }

  /**
   * Determines and returns the deepest leaf of the element <code>el</code>.
   *
   * @param el the base element
   *
   * @returnthe deepest leaf of the element <code>el</code>
   */
  private Element deepestLeaf(Element el)
  {
    Element leaf;
    if (el.isLeaf())
      leaf = el;
    else
      {
        int count = el.getElementCount();
        if (count == 0)
          leaf = el;
        else
          leaf = deepestLeaf(el.getElement(count - 1));
      }
    return leaf;
  }
}
