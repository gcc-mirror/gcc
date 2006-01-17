/* CompositeView.java -- An abstract view that manages child views
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


package javax.swing.text;

import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SwingConstants;

/**
 * An abstract base implementation of {@link View} that manages child
 * <code>View</code>s.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public abstract class CompositeView
  extends View
{

  /**
   * The child views of this <code>CompositeView</code>.
   */
  View[] children;

  /**
   * The allocation of this <code>View</code> minus its insets. This is
   * initialized in {@link #getInsideAllocation} and reused and modified in
   * {@link #childAllocation(int, Rectangle)}.
   */
  Rectangle insideAllocation;

  /**
   * The insets of this <code>CompositeView</code>. This is initialized
   * in {@link #setInsets}.
   */
  Insets insets;

  /**
   * Creates a new <code>CompositeView</code> for the given
   * <code>Element</code>.
   *
   * @param element the element that is rendered by this CompositeView
   */
  public CompositeView(Element element)
  {
    super(element);
    children = new View[0];
    insets = new Insets(0, 0, 0, 0);
  }

  /**
   * Loads the child views of this <code>CompositeView</code>. This method
   * is called from {@link #setParent} to initialize the child views of
   * this composite view.
   *
   * @param f the view factory to use for creating new child views
   *
   * @see #setParent
   */
  protected void loadChildren(ViewFactory f)
  {
    Element el = getElement();
    int count = el.getElementCount();
    View[] newChildren = new View[count];
    for (int i = 0; i < count; ++i)
      {
        Element child = el.getElement(i);
        View view = f.create(child);
        newChildren[i] = view;
      }
    replace(0, getViewCount(), newChildren);
  }

  /**
   * Sets the parent of this <code>View</code>.
   * In addition to setting the parent, this calls {@link #loadChildren}, if
   * this <code>View</code> does not already have its children initialized.
   *
   * @param parent the parent to set
   */
  public void setParent(View parent)
  {
    super.setParent(parent);
    if (parent != null && ((children == null) || children.length == 0))
      loadChildren(getViewFactory());
  }

  /**
   * Returns the number of child views.
   *
   * @return the number of child views
   */
  public int getViewCount()
  {
    return children.length;
  }

  /**
   * Returns the child view at index <code>n</code>.
   *
   * @param n the index of the requested child view
   *
   * @return the child view at index <code>n</code>
   */
  public View getView(int n)
  {
    return children[n];
  }

  /**
   * Replaces child views by some other child views. If there are no views to
   * remove (<code>length == 0</code>), the result is a simple insert, if
   * there are no children to add (<code>view == null</code>) the result
   * is a simple removal.
   *
   * @param offset the start offset from where to remove children
   * @param length the number of children to remove
   * @param views the views that replace the removed children
   */
  public void replace(int offset, int length, View[] views)
  {
    // Check for null views to add.
    for (int i = 0; i < views.length; ++i)
      if (views[i] == null)
        throw new NullPointerException("Added views must not be null");

    int endOffset = offset + length;

    // First we set the parent of the removed children to null.
    for (int i = offset; i < endOffset; ++i)
      children[i].setParent(null);

    View[] newChildren = new View[children.length - length + views.length];
    System.arraycopy(children, 0, newChildren, 0, offset);
    System.arraycopy(views, 0, newChildren, offset, views.length);
    System.arraycopy(children, offset + length, newChildren,
                     offset + views.length,
                     children.length - (offset + length));
    children = newChildren;

    // Finally we set the parent of the added children to this.
    for (int i = 0; i < views.length; ++i)
      views[i].setParent(this);
  }

  /**
   * Returns the allocation for the specified child <code>View</code>.
   *
   * @param index the index of the child view
   * @param a the allocation for this view
   *
   * @return the allocation for the specified child <code>View</code>
   */
  public Shape getChildAllocation(int index, Shape a)
  {
    Rectangle r = getInsideAllocation(a);
    childAllocation(index, r);
    return r;
  }

  /**
   * Maps a position in the document into the coordinate space of the View.
   * The output rectangle usually reflects the font height but has a width
   * of zero.
   *
   * @param pos the position of the character in the model
   * @param a the area that is occupied by the view
   * @param bias either {@link Position.Bias#Forward} or
   *        {@link Position.Bias#Backward} depending on the preferred
   *        direction bias. If <code>null</code> this defaults to
   *        <code>Position.Bias.Forward</code>
   *
   * @return a rectangle that gives the location of the document position
   *         inside the view coordinate space
   *
   * @throws BadLocationException if <code>pos</code> is invalid
   * @throws IllegalArgumentException if b is not one of the above listed
   *         valid values
   */
  public Shape modelToView(int pos, Shape a, Position.Bias bias)
    throws BadLocationException
  {
    int childIndex = getViewIndex(pos, bias);
    if (childIndex != -1)
      {
        View child = getView(childIndex);
        Rectangle r = a.getBounds();
        childAllocation(childIndex, r);
        Shape result = child.modelToView(pos, r, bias);
        if (result == null)
          throw new AssertionError("" + child.getClass().getName()
                                   + ".modelToView() must not return null");
        return result;
      }
    else
      throw new BadLocationException("No child view for the specified location",
                                     pos);
  }

  /**
   * Maps a region in the document into the coordinate space of the View.
   *
   * @param p1 the beginning position inside the document
   * @param b1 the direction bias for the beginning position
   * @param p2 the end position inside the document
   * @param b2 the direction bias for the end position
   * @param a the area that is occupied by the view
   *
   * @return a rectangle that gives the span of the document region
   *         inside the view coordinate space
   *
   * @throws BadLocationException if <code>p1</code> or <code>p2</code> are
   *         invalid
   * @throws IllegalArgumentException if b1 or b2 is not one of the above
   *         listed valid values
   */
  public Shape modelToView(int p1, Position.Bias b1,
			   int p2, Position.Bias b2, Shape a)
    throws BadLocationException
  {
    // TODO: This is most likely not 100% ok, figure out what else is to
    // do here.
    return super.modelToView(p1, b1, p2, b2, a);
  }

  /**
   * Maps coordinates from the <code>View</code>'s space into a position
   * in the document model.
   *
   * @param x the x coordinate in the view space, x >= 0
   * @param y the y coordinate in the view space, y >= 0
   * @param a the allocation of this <code>View</code>
   * @param b the bias to use
   *
   * @return the position in the document that corresponds to the screen
   *         coordinates <code>x, y</code> >= 0
   */
  public int viewToModel(float x, float y, Shape a, Position.Bias[] b)
  {
    if (x >= 0 && y >= 0)
      {
        Rectangle r = getInsideAllocation(a);
        View view = getViewAtPoint((int) x, (int) y, r);
        return view.viewToModel(x, y, a, b);
      }
    return 0;
  }

  /**
   * Returns the next model location that is visible in eiter north / south
   * direction or east / west direction. This is used to determine the placement
   * of the caret when navigating around the document with the arrow keys. This
   * is a convenience method for {@link #getNextNorthSouthVisualPositionFrom}
   * and {@link #getNextEastWestVisualPositionFrom}.
   * 
   * @param pos
   *          the model position to start search from
   * @param b
   *          the bias for <code>pos</code>
   * @param a
   *          the allocated region for this view
   * @param direction
   *          the direction from the current position, can be one of the
   *          following:
   *          <ul>
   *          <li>{@link SwingConstants#WEST}</li>
   *          <li>{@link SwingConstants#EAST}</li>
   *          <li>{@link SwingConstants#NORTH}</li>
   *          <li>{@link SwingConstants#SOUTH}</li>
   *          </ul>
   * @param biasRet
   *          the bias of the return value gets stored here
   * @return the position inside the model that represents the next visual
   *         location
   * @throws BadLocationException
   *           if <code>pos</code> is not a valid location inside the document
   *           model
   * @throws IllegalArgumentException
   *           if <code>direction</code> is invalid
   */
  public int getNextVisualPositionFrom(int pos, Position.Bias b, Shape a,
                                       int direction, Position.Bias[] biasRet)
    throws BadLocationException
  {
    int retVal = -1;
    switch (direction)
      {
      case SwingConstants.WEST:
      case SwingConstants.EAST:
        retVal = getNextEastWestVisualPositionFrom(pos, b, a, direction,
                                                   biasRet);
        break;
      case SwingConstants.NORTH:
      case SwingConstants.SOUTH:
        retVal = getNextNorthSouthVisualPositionFrom(pos, b, a, direction,
                                                     biasRet);
        break;
      default:
        throw new IllegalArgumentException("Illegal value for direction.");
      }
    return retVal;
  }

  /**
   * Returns the index of the child view that represents the specified
   * model location.
   *
   * @param pos the model location for which to determine the child view index
   * @param b the bias to be applied to <code>pos</code>
   *
   * @return the index of the child view that represents the specified
   *         model location
   */
  public int getViewIndex(int pos, Position.Bias b)
  {
    // FIXME: Handle bias somehow.
    return getViewIndexAtPosition(pos);
  }

  /**
   * Returns <code>true</code> if the specified point lies before the
   * given <code>Rectangle</code>, <code>false</code> otherwise.
   *
   * &quot;Before&quot; is typically defined as being to the left or above.
   *
   * @param x the X coordinate of the point
   * @param y the Y coordinate of the point
   * @param r the rectangle to test the point against
   *
   * @return <code>true</code> if the specified point lies before the
   *         given <code>Rectangle</code>, <code>false</code> otherwise
   */
  protected abstract boolean isBefore(int x, int y, Rectangle r);

  /**
   * Returns <code>true</code> if the specified point lies after the
   * given <code>Rectangle</code>, <code>false</code> otherwise.
   *
   * &quot;After&quot; is typically defined as being to the right or below.
   *
   * @param x the X coordinate of the point
   * @param y the Y coordinate of the point
   * @param r the rectangle to test the point against
   *
   * @return <code>true</code> if the specified point lies after the
   *         given <code>Rectangle</code>, <code>false</code> otherwise
   */
  protected abstract boolean isAfter(int x, int y, Rectangle r);

  /**
   * Returns the child <code>View</code> at the specified location.
   *
   * @param x the X coordinate
   * @param y the Y coordinate
   * @param r the inner allocation of this <code>BoxView</code> on entry,
   *        the allocation of the found child on exit
   *
   * @return the child <code>View</code> at the specified location
   */
  protected abstract View getViewAtPoint(int x, int y, Rectangle r);

  /**
   * Computes the allocation for a child <code>View</code>. The parameter
   * <code>a</code> stores the allocation of this <code>CompositeView</code>
   * and is then adjusted to hold the allocation of the child view.
   *
   * @param index the index of the child <code>View</code>
   * @param a the allocation of this <code>CompositeView</code> before the
   *        call, the allocation of the child on exit
   */
  protected abstract void childAllocation(int index, Rectangle a);

  /**
   * Returns the child <code>View</code> that contains the given model
   * position. The given <code>Rectangle</code> gives the parent's allocation
   * and is changed to the child's allocation on exit.
   *
   * @param pos the model position to query the child <code>View</code> for
   * @param a the parent allocation on entry and the child allocation on exit
   *
   * @return the child view at the given model position
   */
  protected View getViewAtPosition(int pos, Rectangle a)
  {
    int i = getViewIndexAtPosition(pos);
    View view = children[i];
    childAllocation(i, a);
    return view;
  }

  /**
   * Returns the index of the child <code>View</code> for the given model
   * position.
   *
   * @param pos the model position for whicht the child <code>View</code> is
   *        queried
   *
   * @return the index of the child <code>View</code> for the given model
   *         position
   */
  protected int getViewIndexAtPosition(int pos)
  {
    int index = -1;
    for (int i = 0; i < children.length; i++)
      {
        if (children[i].getStartOffset() <= pos
            && children[i].getEndOffset() > pos)
          {
            index = i;
            break;
          }
      }
    return index;
  }

  /**
   * Returns the allocation that is given to this <code>CompositeView</code>
   * minus this <code>CompositeView</code>'s insets.
   *
   * Also this translates from an immutable allocation to a mutable allocation
   * that is typically reused and further narrowed, like in
   * {@link #childAllocation}.
   *
   * @param a the allocation given to this <code>CompositeView</code>
   *
   * @return the allocation that is given to this <code>CompositeView</code>
   *         minus this <code>CompositeView</code>'s insets or
   *         <code>null</code> if a was <code>null</code>
   */
  protected Rectangle getInsideAllocation(Shape a)
  {
    if (a == null)
      return null;

    Rectangle alloc = a.getBounds();
    // Initialize the inside allocation rectangle. This is done inside
    // a synchronized block in order to avoid multiple threads creating
    // this instance simultanously.
    Rectangle inside;
    synchronized(this)
      {
        inside = insideAllocation;
        if (inside == null)
          {
            inside = new Rectangle();
            insideAllocation = inside;
          }
      }
    inside.x = alloc.x + insets.left;
    inside.y = alloc.y + insets.top;
    inside.width = alloc.width - insets.left - insets.right;
    inside.height = alloc.height - insets.top - insets.bottom;
    return inside;
  }

  /**
   * Sets the insets defined by attributes in <code>attributes</code>. This
   * queries the attribute keys {@link StyleConstants#SpaceAbove},
   * {@link StyleConstants#SpaceBelow}, {@link StyleConstants#LeftIndent} and
   * {@link StyleConstants#RightIndent} and calls {@link #setInsets} to
   * actually set the insets on this <code>CompositeView</code>.
   *
   * @param attributes the attributes from which to query the insets
   */
  protected void setParagraphInsets(AttributeSet attributes)
  {
    Float l = (Float) attributes.getAttribute(StyleConstants.LeftIndent);
    short left = 0;
    if (l != null)
      left = l.shortValue();
    Float r = (Float) attributes.getAttribute(StyleConstants.RightIndent);
    short right = 0;
    if (r != null)
      right = r.shortValue();
    Float t = (Float) attributes.getAttribute(StyleConstants.SpaceAbove);
    short top = 0;
    if (t != null)
      top = t.shortValue();
    Float b = (Float) attributes.getAttribute(StyleConstants.SpaceBelow);
    short bottom = 0;
    if (b != null)
      bottom = b.shortValue();
    setInsets(top, left, bottom, right);
  }

  /**
   * Sets the insets of this <code>CompositeView</code>.
   *
   * @param top the top inset
   * @param left the left inset
   * @param bottom the bottom inset
   * @param right the right inset
   */
  protected void setInsets(short top, short left, short bottom, short right)
  {
    insets.top = top;
    insets.left = left;
    insets.bottom = bottom;
    insets.right = right;
  }

  /**
   * Returns the left inset of this <code>CompositeView</code>.
   *
   * @return the left inset of this <code>CompositeView</code>
   */
  protected short getLeftInset()
  {
    return (short) insets.left;
  }

  /**
   * Returns the right inset of this <code>CompositeView</code>.
   *
   * @return the right inset of this <code>CompositeView</code>
   */
  protected short getRightInset()
  {
    return (short) insets.right;
  }

  /**
   * Returns the top inset of this <code>CompositeView</code>.
   *
   * @return the top inset of this <code>CompositeView</code>
   */
  protected short getTopInset()
  {
    return (short) insets.top;
  }

  /**
   * Returns the bottom inset of this <code>CompositeView</code>.
   *
   * @return the bottom inset of this <code>CompositeView</code>
   */
  protected short getBottomInset()
  {
    return (short) insets.bottom;
  }

  /**
   * Returns the next model location that is visible in north or south
   * direction.
   * This is used to determine the
   * placement of the caret when navigating around the document with
   * the arrow keys.
   *
   * @param pos the model position to start search from
   * @param b the bias for <code>pos</code>
   * @param a the allocated region for this view
   * @param direction the direction from the current position, can be one of
   *        the following:
   *        <ul>
   *        <li>{@link SwingConstants#NORTH}</li>
   *        <li>{@link SwingConstants#SOUTH}</li>
   *        </ul>
   * @param biasRet the bias of the return value gets stored here
   *
   * @return the position inside the model that represents the next visual
   *         location
   *
   * @throws BadLocationException if <code>pos</code> is not a valid location
   *         inside the document model
   * @throws IllegalArgumentException if <code>direction</code> is invalid
   */
  protected int getNextNorthSouthVisualPositionFrom(int pos, Position.Bias b,
                                                    Shape a, int direction,
                                                    Position.Bias[] biasRet)
    throws BadLocationException
  {
    // FIXME: Implement this correctly.
    return pos;
  }

  /**
   * Returns the next model location that is visible in east or west
   * direction.
   * This is used to determine the
   * placement of the caret when navigating around the document with
   * the arrow keys.
   *
   * @param pos the model position to start search from
   * @param b the bias for <code>pos</code>
   * @param a the allocated region for this view
   * @param direction the direction from the current position, can be one of
   *        the following:
   *        <ul>
   *        <li>{@link SwingConstants#EAST}</li>
   *        <li>{@link SwingConstants#WEST}</li>
   *        </ul>
   * @param biasRet the bias of the return value gets stored here
   *
   * @return the position inside the model that represents the next visual
   *         location
   *
   * @throws BadLocationException if <code>pos</code> is not a valid location
   *         inside the document model
   * @throws IllegalArgumentException if <code>direction</code> is invalid
   */
  protected int getNextEastWestVisualPositionFrom(int pos, Position.Bias b,
                                                  Shape a, int direction,
                                                  Position.Bias[] biasRet)
    throws BadLocationException
  {
    // FIXME: Implement this correctly.
    return pos;
  }

  /**
   * Determines if the next view in horinzontal direction is located to
   * the east or west of the view at position <code>pos</code>. Usually
   * the <code>View</code>s are laid out from the east to the west, so
   * we unconditionally return <code>false</code> here. Subclasses that
   * support bidirectional text may wish to override this method.
   *
   * @param pos the position in the document
   * @param bias the bias to be applied to <code>pos</code>
   *
   * @return <code>true</code> if the next <code>View</code> is located
   *         to the EAST, <code>false</code> otherwise
   */
  protected boolean flipEastAndWestAtEnds(int pos, Position.Bias bias)
  {
    return false;
  }
}
