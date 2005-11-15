/* FlowView.java -- A composite View
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

import java.awt.Container;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;

/**
 * A <code>View</code> that can flows it's children into it's layout space.
 *
 * The <code>FlowView</code> manages a set of logical views (that are
 * the children of the {@link #layoutPool} field). These are translated
 * at layout time into a set of physical views. These are the views that
 * are managed as the real child views. Each of these child views represents
 * a row and are laid out within a box using the superclasses behaviour.
 * The concrete implementation of the rows must be provided by subclasses.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public abstract class FlowView extends BoxView
{
  /**
   * A strategy for translating the logical views of a <code>FlowView</code>
   * into the real views.
   */
  public static class FlowStrategy
  {
    /**
     * Creates a new instance of <code>FlowStragegy</code>.
     */
    public FlowStrategy()
    {
      // Nothing to do here.
    }

    /**
     * Receives notification from a <code>FlowView</code> that some content
     * has been inserted into the document at a location that the
     * <code>FlowView</code> is responsible for.
     *
     * The default implementation simply calls {@link #layout}.
     *
     * @param fv the flow view that sends the notification
     * @param e the document event describing the change
     * @param alloc the current allocation of the flow view
     */
    public void insertUpdate(FlowView fv, DocumentEvent e, Rectangle alloc)
    {
      layout(fv);
    }

    /**
     * Receives notification from a <code>FlowView</code> that some content
     * has been removed from the document at a location that the
     * <code>FlowView</code> is responsible for.
     *
     * The default implementation simply calls {@link #layout}.
     *
     * @param fv the flow view that sends the notification
     * @param e the document event describing the change
     * @param alloc the current allocation of the flow view
     */
    public void removeUpdate(FlowView fv, DocumentEvent e, Rectangle alloc)
    {
      layout(fv);
    }

    /**
     * Receives notification from a <code>FlowView</code> that some attributes
     * have changed in the document at a location that the
     * <code>FlowView</code> is responsible for.
     *
     * The default implementation simply calls {@link #layout}.
     *
     * @param fv the flow view that sends the notification
     * @param e the document event describing the change
     * @param alloc the current allocation of the flow view
     */
    public void changedUpdate(FlowView fv, DocumentEvent e, Rectangle alloc)
    {
      layout(fv);
    }

    /**
     * Returns the logical view of the managed <code>FlowView</code>.
     *
     * @param fv the flow view for which to return the logical view
     *
     * @return the logical view of the managed <code>FlowView</code>
     */
    public View getLogicalView(FlowView fv)
    {
      return fv.layoutPool;
    }

    /**
     * Performs the layout for the whole view. By default this rebuilds
     * all the physical views from the logical views of the managed FlowView.
     *
     * This is called by {@link FlowView#layout} to update the layout of
     * the view.
     *
     * @param fv the flow view for which we perform the layout
     */
    public void layout(FlowView fv)
    {
      fv.removeAll();
      Element el = fv.getElement();

      int rowStart = el.getStartOffset();
      int end = el.getEndOffset();
      int rowIndex = 0;
      while (rowStart >= 0 && rowStart < end)
        {
          View row = fv.createRow();
          fv.append(row);
          rowStart = layoutRow(fv, rowIndex, rowStart);
          rowIndex++;
        }
    }

    /**
     * Lays out one row of the flow view. This is called by {@link #layout}
     * to fill one row with child views until the available span is exhausted.
     *
     * @param fv the flow view for which we perform the layout
     * @param rowIndex the index of the row
     * @param pos the start position for the row
     *
     * @return the start position of the next row
     */
    protected int layoutRow(FlowView fv, int rowIndex, int pos)
    {
      int spanLeft = fv.getFlowSpan(rowIndex);
      if (spanLeft <= 0)
        return -1;

      int offset = pos;
      View row = fv.getView(rowIndex);
      int flowAxis = fv.getFlowAxis();

      while (spanLeft > 0)
        {
          View child = createView(fv, offset, spanLeft, rowIndex);
          if (child == null)
            {
              offset = -1;
              break;
            }

          int span = (int) child.getPreferredSpan(flowAxis);
          if (span > spanLeft)
            {
              offset = -1;
              break;
            }

          row.append(child);
          spanLeft -= span;
          offset = child.getEndOffset();
        }
      return offset;
    }

    /**
     * Creates physical views that form the rows of the flow view. This
     * can be an entire view from the logical view (if it fits within the
     * available span), a fragment of such a view (if it doesn't fit in the
     * available span and can be broken down) or <code>null</code> (if it does
     * not fit in the available span and also cannot be broken down).
     *
     * @param fv the flow view
     * @param offset the start offset for the view to be created
     * @param spanLeft the available span
     * @param rowIndex the index of the row
     *
     * @return a view to fill the row with, or <code>null</code> if there
     *         is no view or view fragment that fits in the available span
     */
    protected View createView(FlowView fv, int offset, int spanLeft,
                              int rowIndex)
    {
      // Find the logical element for the given offset.
      View logicalView = getLogicalView(fv);

      int viewIndex = logicalView.getViewIndex(offset, Position.Bias.Forward);
      if (viewIndex == -1)
        return null;

      View child = logicalView.getView(viewIndex);
      int flowAxis = fv.getFlowAxis();
      int span = (int) child.getPreferredSpan(flowAxis);

      if (span <= spanLeft)
        return child;
      else if (child.getBreakWeight(flowAxis, offset, spanLeft)
               > BadBreakWeight)
        // FIXME: What to do with the pos parameter here?
        return child.breakView(flowAxis, offset, 0, spanLeft);
      else
        return null;
    }
  }

  /**
   * This special subclass of <code>View</code> is used to represent
   * the logical representation of this view. It does not support any
   * visual representation, this is handled by the physical view implemented
   * in the <code>FlowView</code>.
   */
  class LogicalView extends View
  {
    /**
     * The child views of this logical view.
     */
    Vector children;

    /**
     * Creates a new LogicalView instance.
     */
    LogicalView(Element el)
    {
      super(el);
      children = new Vector();
    }

    /**
     * Returns the container that holds this view. The logical view returns
     * the enclosing FlowView's container here.
     *
     * @return the container that holds this view
     */
    public Container getContainer()
    {
      return FlowView.this.getContainer();
    }

    /**
     * Returns the number of child views of this logical view.
     *
     * @return the number of child views of this logical view
     */
    public int getViewCount()
    {
      return children.size();
    }

    /**
     * Returns the child view at the specified index.
     *
     * @param index the index
     *
     * @return the child view at the specified index
     */
    public View getView(int index)
    {
      return (View) children.get(index);
    }

    /**
     * Replaces some child views with other child views.
     *
     * @param offset the offset at which to replace child views
     * @param length the number of children to remove
     * @param views the views to be inserted
     */
    public void replace(int offset, int length, View[] views)
    {
      if (length > 0)
        {
          for (int count = 0; count < length; ++count)
            children.remove(offset);
        }

      int endOffset = offset + views.length;
      for (int i = offset; i < endOffset; ++i)
        {
          children.add(i, views[i - offset]);
          // Set the parent of the child views to the flow view itself so
          // it has something to resolve.
          views[i - offset].setParent(FlowView.this);
        }
    }

    /**
     * Returns the index of the child view that contains the specified
     * position in the document model.
     *
     * @param pos the position for which we are searching the child view
     * @param b the bias
     *
     * @return the index of the child view that contains the specified
     *         position in the document model
     */
    public int getViewIndex(int pos, Position.Bias b)
    {
      int index = -1;
      int i = 0;
      for (Iterator it = children.iterator(); it.hasNext(); i++)
        {
          View child = (View) it.next();
          if (child.getStartOffset() >= pos
              && child.getEndOffset() < pos)
            {
              index = i;
              break;
            }
        }
      return index;
    }

    /**
     * Throws an AssertionError because it must never be called. LogicalView
     * only serves as a holder for child views and has no visual
     * representation.
     */
    public float getPreferredSpan(int axis)
    {
      throw new AssertionError("This method must not be called in "
                               + "LogicalView.");
    }

    /**
     * Throws an AssertionError because it must never be called. LogicalView
     * only serves as a holder for child views and has no visual
     * representation.
     */
    public Shape modelToView(int pos, Shape a, Position.Bias b)
      throws BadLocationException
    {
      throw new AssertionError("This method must not be called in "
                               + "LogicalView.");
    }

    /**
     * Throws an AssertionError because it must never be called. LogicalView
     * only serves as a holder for child views and has no visual
     * representation.
     */
    public void paint(Graphics g, Shape s)
    {
      throw new AssertionError("This method must not be called in "
                               + "LogicalView.");
    }

    /**
     * Throws an AssertionError because it must never be called. LogicalView
     * only serves as a holder for child views and has no visual
     * representation.
     */
    public int viewToModel(float x, float y, Shape a, Position.Bias[] b)
    {
      throw new AssertionError("This method must not be called in "
                               + "LogicalView.");
    }

    /**
     * Returns the document position that is (visually) nearest to the given
     * document position <code>pos</code> in the given direction <code>d</code>.
     *
     * @param c the text component
     * @param pos the document position
     * @param b the bias for <code>pos</code>
     * @param d the direction, must be either {@link SwingConstants#NORTH},
     *        {@link SwingConstants#SOUTH}, {@link SwingConstants#WEST} or
     *        {@link SwingConstants#EAST}
     * @param biasRet an array of {@link Position.Bias} that can hold at least
     *        one element, which is filled with the bias of the return position
     *        on method exit
     *
     * @return the document position that is (visually) nearest to the given
     *         document position <code>pos</code> in the given direction
     *         <code>d</code>
     *
     * @throws BadLocationException if <code>pos</code> is not a valid offset in
     *         the document model
     */
    public int getNextVisualPositionFrom(JTextComponent c, int pos,
                                         Position.Bias b, int d,
                                         Position.Bias[] biasRet)
      throws BadLocationException
    {
      assert false : "getNextVisualPositionFrom() must not be called in "
        + "LogicalView";
      return 0;
    }
  }

  /**
   * The shared instance of FlowStrategy.
   */
  static final FlowStrategy sharedStrategy = new FlowStrategy();

  /**
   * The span of the <code>FlowView</code> that should be flowed.
   */
  protected int layoutSpan;

  /**
   * Represents the logical child elements of this view, encapsulated within
   * one parent view (an instance of a package private <code>LogicalView</code>
   * class). These will be translated to a set of real views that are then
   * displayed on screen. This translation is performed by the inner class
   * {@link FlowStrategy}.
   */
  protected View layoutPool;

  /**
   * The <code>FlowStrategy</code> to use for translating between the
   * logical and physical view.
   */
  protected FlowStrategy strategy;

  /**
   * Creates a new <code>FlowView</code> for the given
   * <code>Element</code> and <code>axis</code>.
   *
   * @param element the element that is rendered by this FlowView
   * @param axis the axis along which the view is tiled, either
   *        <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>, the flow
   *        axis is orthogonal to this one
   */
  public FlowView(Element element, int axis)
  {
    super(element, axis);
    strategy = sharedStrategy;
  }

  /**
   * Returns the axis along which the view should be flowed. This is
   * orthogonal to the axis along which the boxes are tiled.
   *
   * @return the axis along which the view should be flowed
   */
  public int getFlowAxis()
  {
    int axis = getAxis();
    int flowAxis;
 
    if (axis == X_AXIS)
      flowAxis = Y_AXIS;
    else
      flowAxis = X_AXIS;

    return flowAxis;

  }

  /**
   * Returns the span of the flow for the specified child view. A flow
   * layout can be shaped by providing different span values for different
   * child indices. The default implementation returns the entire available
   * span inside the view.
   *
   * @param index the index of the child for which to return the span
   *
   * @return the span of the flow for the specified child view
   */
  public int getFlowSpan(int index)
  {
    return layoutSpan;
  }

  /**
   * Returns the location along the flow axis where the flow span starts
   * given a child view index. The flow can be shaped by providing
   * different values here.
   *
   * @param index the index of the child for which to return the flow location
   *
   * @return the location along the flow axis where the flow span starts
   */
  public int getFlowStart(int index)
  {
    return getLeftInset(); // TODO: Is this correct?
  }

  /**
   * Creates a new view that represents a row within a flow.
   *
   * @return a view for a new row
   */
  protected abstract View createRow();

  /**
   * Loads the children of this view. The <code>FlowView</code> does not
   * directly load its children. Instead it creates a logical view
   * (@{link #layoutPool}) which is filled by the logical child views.
   * The real children are created at layout time and each represent one
   * row.
   *
   * This method is called by {@link View#setParent} in order to initialize
   * the view.
   *
   * @param vf the view factory to use for creating the child views
   */
  protected void loadChildren(ViewFactory vf)
  {
    if (layoutPool == null)
      {
        layoutPool = new LogicalView(getElement());

        Element el = getElement();
        int count = el.getElementCount();
        for (int i = 0; i < count; ++i)
          {
            Element childEl = el.getElement(i);
            View childView = vf.create(childEl);
            layoutPool.append(childView);
          }
      }
  }

  /**
   * Performs the layout of this view. If the span along the flow axis changed,
   * this first calls {@link FlowStrategy#layout} in order to rebuild the
   * rows of this view. Then the superclass's behaviour is called to arrange
   * the rows within the box.
   *
   * @param width the width of the view
   * @param height the height of the view
   */
  protected void layout(int width, int height)
  {
    boolean rebuild = false;

    int flowAxis = getFlowAxis();
    if (flowAxis == X_AXIS)
      {
        rebuild = !(width == layoutSpan);
        layoutSpan = width;
      }
    else
      {
        rebuild = !(height == layoutSpan);
        layoutSpan = height;
      }

    if (rebuild)
      strategy.layout(this);

    // TODO: If the span along the box axis has changed in the process of
    // relayouting the rows (that is, if rows have been added or removed),
    // call preferenceChanged in order to throw away cached layout information
    // of the surrounding BoxView.

    super.layout(width, height);
  }

  /**
   * Receice notification that some content has been inserted in the region
   * that this view is responsible for. This calls
   * {@link FlowStrategy#insertUpdate}.
   *
   * @param changes the document event describing the changes
   * @param a the current allocation of the view
   * @param vf the view factory that is used for creating new child views
   */
  public void insertUpdate(DocumentEvent changes, Shape a, ViewFactory vf)
  {
    strategy.insertUpdate(this, changes, getInsideAllocation(a));
  }

  /**
   * Receice notification that some content has been removed from the region
   * that this view is responsible for. This calls
   * {@link FlowStrategy#removeUpdate}.
   *
   * @param changes the document event describing the changes
   * @param a the current allocation of the view
   * @param vf the view factory that is used for creating new child views
   */
  public void removeUpdate(DocumentEvent changes, Shape a, ViewFactory vf)
  {
    strategy.removeUpdate(this, changes, getInsideAllocation(a));
  }

  /**
   * Receice notification that some attributes changed in the region
   * that this view is responsible for. This calls
   * {@link FlowStrategy#changedUpdate}.
   *
   * @param changes the document event describing the changes
   * @param a the current allocation of the view
   * @param vf the view factory that is used for creating new child views
   */
  public void changedUpdate(DocumentEvent changes, Shape a, ViewFactory vf)
  {
    strategy.changedUpdate(this, changes, getInsideAllocation(a));
  }

  /**
   * Returns the index of the child <code>View</code> for the given model
   * position.
   *
   * This is implemented to iterate over the children of this
   * view (the rows) and return the index of the first view that contains
   * the given position.
   *
   * @param pos the model position for whicht the child <code>View</code> is
   *        queried
   *
   * @return the index of the child <code>View</code> for the given model
   *         position
   */
  protected int getViewIndexAtPosition(int pos)
  {
    // First make sure we have a valid layout.
    if (!isAllocationValid())
      layout(getWidth(), getHeight());

    int count = getViewCount();
    int result = -1;

    for (int i = 0; i < count; ++i)
      {
        View child = getView(i);
        int start = child.getStartOffset();
        int end = child.getEndOffset();
        if (start <= pos && end > pos)
          {
            result = i;
            break;
          }
      }
    return result;
  }
}
